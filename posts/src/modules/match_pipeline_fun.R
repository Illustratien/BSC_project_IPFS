shp <-c(5,9,1,8,10)
names(shp) <- c("blurry.complete","blurry.incomplete","complete","hair","incomplete")

plotfun <- function(df,disthresh=80){
  # disthresh : threshold value to judge whether it belongs to same point or not
  ndf <- df%>%filter(type=="ntu")
  gdf <-   df%>%filter(type=="truth")
  if(nrow(gdf)==0){
    stop(sprintf("no ground truth found for  %s!",df$pic_name[1]))
  }
  # match each point to ground truth
  ntd<- map_dfr(1:nrow(ndf),~{
    # for each point in estimated pipeline
    # bind witn ground truth and calculate the distance
    d <- bind_rows(ndf[.x,],
                   gdf) %>% 
      dplyr::select(stomata.cx,stomata.cy) %>% 
      stats::dist() %>%
      as.matrix(diag = TRUE, upper = TRUE) 
    # focus on distance not larger than 20 (subset distance matrix)
    condi <- setdiff(which(d[1,]<disthresh),1)-1
    
    ndf[.x,] %>% 
      mutate(
        # row index of ground truth that falls within this threshold
        gid=  ifelse(length(condi)==0,NA,condi),
        # ground truth x
        truth.cx=ifelse(!is.na(gid),gdf[gid,]$"stomata.cx",NA),
        # ground truth y
        truth.cy=ifelse(!is.na(gid),gdf[gid,]$"stomata.cy",NA)
      )
  })%>%
    mutate(display.y=1944-stomata.cy) 
  
  # check how many points share the same ground truth 
  repeateddf <- ntd %>%
    na.omit() %>% 
    group_by(gid) %>%
    summarise(n=n()-1) %>% 
    filter(n>0)
  
  ntd <- rbind(ntd %>%
                 filter(!is.na(gid)) %>% 
                 group_by(gid) %>%
                 filter(confidence==max(confidence)),
               ntd %>%
                 filter(is.na(gid))) %>% ungroup()
  
  # repn <-repeateddf%>% nrow()
  unmd <- nrow(gdf)-ntd$gid %>% na.omit()%>% unique() %>% length()
  # -------------------------------------------------------------------------
  SummaryTable <- bind_rows(gdf,ntd) %>% 
    group_by(type) %>% mutate(totaln=n()) %>% 
    group_by(type,class) %>% mutate(indivin=n()) %>% 
    select(type,class,totaln,indivin) %>% distinct()
  briefdf <- SummaryTable %>%.[,-c(2,4)] %>% distinct()
  detaildf <- SummaryTable %>% .[,-3] %>%
    tidyr::pivot_wider(names_from = type,
                       values_from = indivin) %>% 
    mutate(ratio=(ntu/truth) %>%
             round(.,digits = 2))
  
  # -------------------------------------------------------------------------
  plotdf <- bind_rows(gdf,ntd) %>% 
    ggplot(aes(stomata.cx,display.y))+
    # new detected 
    geom_point(data=ntd %>% 
                 filter(is.na(gid)),
               mapping=aes(x = stomata.cx, y = display.y,shape=class),
               show.legend = F,size=3,
               color="black",stroke=2.5,alpha=.5)+
    # old
    geom_point(aes(shape=class,color=type),size=3,stroke=1.5,alpha=.5)+
    scale_shape_manual(values=shp)+
    theme_bw()+
    ggtitle(paste0(df$pic_name[1],"\ndetect/truth = ",
                           round(briefdf[2,2]/briefdf[1,2],digit=2),
                           # "% \nestimated repeated coordinates = ",repn,
                           "  miss match = ",unmd,
                           "                , new match= ",ntd %>%filter(is.na(gid)) %>% nrow())
    )+
    labs(caption=paste0("distance tolerance: ",disthresh, "(pixel)"))+
    #Label new
    ggrepel::geom_text_repel(data=ntd %>% 
                               filter(is.na(gid)),
                             mapping=aes(x = stomata.cx, y = display.y,
                                         label=class),
                             point.padding = 1,box.padding = .8,
                             show.legend =F)+
    
    coord_fixed()+
    theme(axis.title = element_blank(),
          # axis.text = element_blank()
    )
  
  
  # table
  tt <- gridExtra::ttheme_default(colhead=list(fg_params = list(parse=TRUE)))
  
  tbl <- gridExtra::tableGrob(detaildf %>% 
                                mutate(ratio=round(ratio,3)),
                              rows=NULL, theme=tt)
  tbl2 <- gridExtra::tableGrob(briefdf %>% t(), rows=NULL, theme=tt)
  
  tb <- gridExtra::arrangeGrob(tbl2,tbl,nrow=2,
                               # as.table=TRUE,
                               heights=c(3,3))
  # Plot chart and table into one object
  p <-  cowplot::plot_grid(plotdf, tb,
                           nrow=1,rel_widths = c(5,2.3))

  resdf <- data.frame(pic_name=df$pic_name[1],
                      detect=briefdf[2,]$totaln,
                      ground=briefdf[1,]$totaln,
                      new.match = ntd %>%filter(is.na(gid)) %>% nrow(),
                      miss.match = unmd)
  ntd <- ntd %>% ungroup() %>% dplyr::select(-c(id,gid,type,display.y))
  return(list(resdf,p,ntd))
}

rm_rep <- function(ndf,disthresh = 10){
  # for removing the replicate from NTU alone
  d <- ndf %>% 
    dplyr::select(stomata.cx,stomata.cy) %>% 
    stats::dist() %>%
    as.matrix(diag = TRUE, upper = TRUE) 
  d[lower.tri(d,diag = T)] <- NA
  #generate from-to index 
  mat <- as.data.frame(t(combn(dim(ndf)[1],2))) 
  colnames(mat) <- c('from','to')
  mat <- mat %>% mutate(
    # transform the upper triangle into linear by row
    dist=na.omit(as.vector(t(d))))
  repdf <- mat %>% group_by(from) %>%
    dplyr::filter(dist==min(dist,na.rm = T),dist<disthresh) %>%
    mutate(gid=row_number())
  repid <- repdf %>% .[,1:2] %>% unlist()
  
  if (nrow(repdf)>0){
    
    ndf <- ndf %>% mutate(gid=NA,id=row_number())
    for(i in 1:nrow(repdf)){
      tid <- repdf[i,1:2] %>% unlist() %>% as.numeric()
      ndf <- ndf %>% mutate(gid=case_when(id%in%tid~i,
                                          T~gid))  
    }
    
    res <- rbind(ndf %>%
                   filter(!is.na(gid)) %>% 
                   group_by(gid) %>%
                   filter(confidence==max(confidence)),
                 ndf %>%
                   filter(is.na(gid))) %>% ungroup()%>%
      select(-c("gid","id"))
  }else{
    res <- ndf
  }
  
  return(res )
}