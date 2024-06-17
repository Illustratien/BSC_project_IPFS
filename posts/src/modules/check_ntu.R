rm(list = ls())
pacman::p_load(dplyr,ggplot2,purrr,gridExtra,foreach,ggpmisc)
# read files --------------------------------------------------------------
ntu_file <- list.files("result/Ntu",pattern='*.csv')
sourcetype <- ntu_file %>% gsub('_wblurry.csv','',.)

Ntu_dlist <- map(ntu_file,~{
  data.table::fread(file.path("result/Ntu/",.x)) %>% 
    rename(pic_name="File Name") %>% 
    # recover the unit of bounding box length to pixel 
    mutate(
      across(ends_with(c("x", "width")),~.x*2592),
      across(ends_with(c("y", "height")),~.x*1944)
    )%>% 
    mutate(type="ntu") %>% 
    rename(stomata.cx=boundingbox_x,stomata.cy=boundingbox_y,boundingbox_length
           =boundingbox_height)
})

# experiment folder name 
folder_vec <-map_chr(Ntu_dlist,~{
  .x$pic_name %>% 
    strsplit("_") %>% map_depth(.,1,~{.x[1]}) %>% unlist() %>% unique() %>% paste(.,collapse="_")
})
g_dfvec <- list.files(path = "result/intermediate/",
                      pattern = "*data.csv",recursive = T,full.names = T)

ground_df <- map_dfr(g_dfvec,~{
  read.csv(.x)
}) %>% 
  mutate(type="truth") %>% 
  rename(class=stomata.type)
# picturname that is in ground truth 
gdf_pic <- ground_df$pic_name %>% unique()

walk(1:length(folder_vec),function(foldid){
  det_df <- Ntu_dlist[[foldid]]
  folder <- folder_vec[foldid]
  message(paste0("\n",folder))
  
  ntu_pic <- det_df$pic_name %>% unique()
  in_pic <- intersect(gdf_pic,ntu_pic)
  
  ntu_dlist <-  det_df %>% filter(pic_name%in%in_pic)

  ground_df <- ground_df %>% filter(pic_name%in%in_pic)
  
  pb = txtProgressBar(min = 0, max = 4,
                      style = 3,    # Progress bar style (also available style = 1 and style = 2)
                      width = 30,initial = 0)
  # parallel processing -------------------------------------------------------------------------
  message("\nstart matching ground truth and detection:")
  setTxtProgressBar(pb,1)
  n.cores <- parallel::detectCores() - 1
  #create the cluster
  my.cluster <- parallel::makeCluster(
    n.cores,
    type = "PSOCK"
  )
  doParallel::registerDoParallel(cl = my.cluster)
  source("src/modules/match_pipeline_fun.R")
  system.time(
    re <- purrr::map (1:1,function(k){ # for with blurry and without blurry
      # subset data and merge
      ntu_df <- ntu_dlist %>% 
        dplyr::select(stomata.cx,stomata.cy,pic_name,class,confidence,type) %>% 
        group_by(pic_name) %>% group_split() %>% 
        map_dfr(.,~{rm_rep(.x)})
      pic_tar<- ntu_df$pic_name %>% unique()
      # split for each picture 
      mdf<- bind_rows(ground_df %>% 
                        dplyr::select(stomata.cx,stomata.cy,pic_name,class,type) ,
                      ntu_df) %>%
        # add display column 
        mutate(display.y=1944-stomata.cy) %>% 
        group_by(pic_name) %>%
        mutate(id=1:n()) %>% 
        group_split()
      
      res <- foreach(
        i  = 1:length(mdf),
        .packages = c("dplyr","purrr","ggplot2","tidyr","gridExtra")
      ) %dopar% {
        source("src/modules/match_pipeline_fun.R")
        plotfun(mdf[[i]])
      }
    })
  )
  doParallel::stopImplicitCluster()
  
  # output directory ------------------------------------------------------------------
  dir.create(file.path("./result/check_ground_truth"), showWarnings = FALSE)
  tarfoldr <- file.path("./result/check_ground_truth",folder)
  dir.create(tarfoldr, showWarnings = FALSE)
  # dataframe export---------------------------------------------------------------
  
  dff<- map_depth(re,2,~{.x[[1]]}) %>% 
    map(.,~{Reduce("rbind",.x)}) %>% 
    imap(.,~{.x %>% mutate(source=sourcetype[.y])}) %>% 
    Reduce("rbind",.)
  data.table::fwrite(dff,paste0(tarfoldr,"/",folder,"_check.csv"),row.names = F)
  message("\nremoved replicates:")
  setTxtProgressBar(pb,2)
  # -------------------------------------------------------------------------
  gmeg <- ground_df %>% 
    select(-c(stomata.row,stomata.per.row,pic_width,pic_length,
              display.y,type)) %>% 
    rename(truth.class=class) 
  names(gmeg)<- gsub("(stomata\\.|boundingbox_)","truth.",names(gmeg))
  
  dff<- map_depth(re,2,~{.x[[3]]}) %>% 
    map(.,~{Reduce("rbind",.x)}) %>% 
    imap(.,~{.x %>%
        mutate(source=sourcetype[foldid]) %>% 
        left_join(.,ntu_dlist,
                  by=c("stomata.cx", "stomata.cy", "pic_name",
                       "class","confidence"))
    }) %>% 
    Reduce("rbind",.) %>% 
    dplyr::select(-type)%>% 
    rename(detect.class=class)
  names(dff)<- gsub("(stomata\\.|boundingbox_)","detect.",names(dff))
  out <- dff%>% 
    mutate(
      across(ends_with("length") | ends_with("width"),function(x){x*0.4}), #from pixel to microm
      detect.area=detect.width*detect.length)  %>% 
    full_join(.,gmeg%>% 
                mutate(across(ends_with("length") | ends_with("width"),function(x){x*0.4}), #from pixel to microm               
                       truth.area=truth.width*truth.length),c("pic_name", "truth.cx","truth.cy")) %>% 
    relocate(source,pic_name,detect.width,detect.length,detect.area)
  
  data.table::fwrite(out,
                     paste0(tarfoldr,"/",folder,"_detect.csv"),row.names = F)
  
  # -------------------------------------------------------------------------
  colv <- c(names(out)[grepl("(width|length)",names(out))],"pic_name","truth.class","source","confidence","detect.class")
  
  longdf <- out %>% select(all_of(colv)) %>% mutate(id=1:n()) %>% 
    tidyr::pivot_longer(
      -c(id,pic_name,truth.class,source,confidence,detect.class),
      names_to = c("Var", ".value"), 
      names_sep="\\." ) %>% 
    tidyr::pivot_longer(width:length,names_to="trait",
                        values_to = "Trait") %>% 
    tidyr::pivot_wider(values_from = Trait,names_from = Var) 
  
  longdf%>% filter(truth.class=="complete") %>% 
    ggplot(aes(detect,truth))+
    geom_point(shape=1,aes(color=confidence))+
    scale_color_viridis_c()+
    geom_abline(intercept = 0,slope=1)+
    scale_x_continuous(limits = c(0,200))+
    scale_y_continuous(limits = c(0,200))+
    facet_grid(trait~source)+theme_test()+
    stat_poly_line(color="darkred",se=F) +
    stat_poly_eq(use_label(c("eq", "R2")))+
    ggtitle("complete ground truth")
  
  longdf%>% filter(detect.class=="complete") %>% 
    ggplot(aes(detect,truth))+
    geom_point(shape=1,aes(color=confidence))+
    scale_color_viridis_c()+
    geom_abline(intercept = 0,slope=1)+
    scale_x_continuous(limits = c(0,200))+
    scale_y_continuous(limits = c(0,200))+
    facet_grid(trait~source)+theme_test()+
    stat_poly_line(color="darkred",se=F) +
    stat_poly_eq(use_label(c("eq", "R2")))+
    ggtitle("complete detect truth")
  
  
  # plot --------------------------------------------------------------------
  message("\nexport pdf:")
  setTxtProgressBar(pb,3)
  plot_res<- map_depth(re,2,~{.x[[2]]})
  names(plot_res) <- sourcetype[foldid]
  
  
  pdf(paste0(tarfoldr,"/",folder,"_",sourcetype[foldid],"_check.pdf"),
      width=10,height=4,
      onefile = T)
  
  plot_res %>% purrr::walk(.,~{.x %>% print()})
  
  
  dev.off()
  
  
  
  
  message("\ndone!")
  setTxtProgressBar(pb,4)
})


