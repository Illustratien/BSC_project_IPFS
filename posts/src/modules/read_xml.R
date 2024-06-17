pacman::p_load(dplyr,ggplot2,broom,purrr,tidyr,foreach)
# -------------------------------------------------------------------------
path <- dirname(getwd())
source(paste0(path,"src/modules/processing_fun.R"))
# parallel setting
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
# raw data preparation-------------------------------------------------------------------------
folders <-list.dirs("data",recursive = F) %>% gsub("data/","",.)
dir.create(file.path("./result/intermediate"), showWarnings = FALSE)

for (folder in folders){
  message(paste0("\n",folder))
  message("reading, row class slope and graph:")
  pb = txtProgressBar(min = 0, max = 3,
                      style = 3,    # Progress bar style (also available style = 1 and style = 2)
                      width = 30,initial = 0)
  dir.create(file.path("./result/intermediate/",folder), showWarnings = FALSE)
  # read raw data -----------------------------------------------------------
  flist <- list.files(paste0("data/",folder,"/"),".xml")
  
  system.time(
    format_res <- foreach(
      y  = 1:length(flist),
      x=flist,
      .packages = c('XML','purrr','dplyr')
    ) %dopar% {
      xmlread(paste0("data/",folder,"/",x),80) %>%
        mutate(i=y)
    } %>%map_dfr(.,~{.x}) %>%
      dplyr::select(-c(type,pose:difficult,i)) %>%
      dplyr::rename(pic_width=width,pic_length=length,
                    stomata.type=name,pic_name=pic,
                    stomata.row=rowclass) %>%
      dplyr::relocate(stomata.row) %>%
      group_by(pic_name,stomata.row) %>%
      mutate(stomata.per.row=n())
  )
  
  names(format_res) <-
    names(format_res) %>%
    gsub("robndbox","stomata",.)
  format_res<- format_res %>%
    rename(stomata.length=stomata.h,stomata.width=stomata.w)
  
  
  raw_name <- paste0("result/intermediate/",folder,"/",folder,"_xml_data.csv")
  write.csv(format_res,
            raw_name,
            row.names = F  )
  setTxtProgressBar(pb,1)
  
  subdf <- format_res %>% filter(is.na(stomata.cx))
  if(nrow(subdf)>0){
    pl <-   unique(subdf$pic_name) %>% paste(.,collapse="\n")
    warnings(sprintf("These file contains wrongly labled rectangles!, please check:\n %s",
                    pl ))
  }
  
  # slope -------------------------------------------------------------------
  
  format_res <- read.csv(raw_name)%>% 
    mutate(stomata.row=factor(stomata.row)) %>% 
    dplyr::filter(!stomata.type=='hair')
  
  res_ls<- format_res %>% 
    dplyr::filter(stomata.per.row>1) %>% 
    droplevels() %>% 
    group_by(pic_name,stomata.row) %>% 
    group_split() 
  
  system.time(
    slope_df <- foreach(
      i  = 1:length(res_ls),
      df = res_ls,
      .packages = c('dplyr','tidyr')
    ) %dopar% {
      sloptable <- df %>% 
        summarise(mod = broom::tidy(lm(
          display.y ~ stomata.cx , data = .)))%>%
        unnest(cols = c(mod)) 
      sub_df <- df %>% 
        select(pic_name,stomata.row,stomata.per.row) %>% 
        .[1,] 
      
      df <- cbind(sub_df[rep(1,nrow(sloptable)),],
                  sloptable)
      return(df)
    } %>% 
      map_dfr(.,~{.x})%>% 
      dplyr::select(pic_name:estimate) %>% 
      mutate(term=case_when(term=='(Intercept)'~'intercept',
                            T~'slope')) %>% 
      tidyr::pivot_wider(names_from = 'term',values_from = 'estimate')
  )
  
  slop_name <- paste0("result/intermediate/",folder,"/",folder,"_slope.csv")
  write.csv(slope_df,slop_name,row.names = F)
  setTxtProgressBar(pb,2)
  # export pdf -------------------------------------------------------------------------
  pic_name <- format_res$pic_name %>% unique()
  
  pic_ls <- read.csv(raw_name)%>% 
    mutate(stomata.row=factor(stomata.row)) %>% 
    group_by(pic_name) %>% 
    group_split()
  
  pdf(paste0("result/intermediate/",folder,"/",folder,"_stomata_position_check.pdf"),onefile = T)
  
  system.time(
    p_df<- foreach(
      df = pic_ls,
      .packages = c('dplyr','tidyr',"ggplot2")
    ) %dopar% {
      suppressWarnings(plot_fun(df))
    } 
  )
  iwalk(p_df,~{
    suppressWarnings( print(.x))
  })
  dev.off()
  setTxtProgressBar(pb,3)
}
invisible(gc())
parallel::stopCluster(my.cluster)
invisible(gc())

# easy summary
# format_res %>%
#   group_by(stomata_type) %>%
#   summarise(count=n()) %>%
#   mutate(percentage=count*100/nrow(format_res),
#          total=nrow(format_res))
