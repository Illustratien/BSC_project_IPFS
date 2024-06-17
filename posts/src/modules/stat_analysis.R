pacman::p_load(dplyr,ggplot2,broom,purrr,tidyr,foreach)
# -------------------------------------------------------------------------
source("src/modules/processing_fun.R")
# parallel setting
n.cores <- parallel::detectCores() - 1
#create the cluster
my.cluster <- parallel::makeCluster(
  n.cores, 
  type = "PSOCK"
)
doParallel::registerDoParallel(cl = my.cluster)
# read -------------------------------------------------------------------------
folders <-list.dirs("data",recursive = F) %>% gsub("data/","",.)
for (folder in folders){
  message(folder)
  message("statistics for stomata geometry and row_distance:")
  pb = txtProgressBar(min = 0, max = 2,
                      style = 3,    # Progress bar style (also available style = 1 and style = 2)
                      width = 30,initial = 0)
  format_res <- read.csv(paste0("result/intermediate/",folder,"/",folder,"_xml_data.csv"))%>% 
    mutate(stomata.row=factor(stomata.row))%>% 
    dplyr::filter(!stomata.type=='hair')
  
  slope_df <-  read.csv(paste0("result/intermediate/",folder,"/",folder,"_slope.csv"))
  # easy summary of raw data-------------------------------------------------------------------------
  # area cover 1*complete, .5*incomplete
  # total number 1*complete, .5*incomplete
  # sub_df <- format_res %>%  dplyr::filter(pic_name==pic_examp) 
  # simple_stat(sub_df)
  
  df_ls <- format_res %>% group_by(pic_name) %>% group_split()
  
  system.time(
    stat_df <- foreach(
      sub_df = df_ls,
      .packages = c('dplyr','purrr')
    ) %dopar% {
      source("src/modules/processing_fun.R")
      sub_df %>% simple_stat()
    } %>% 
      map_dfr(.,~{.x})
  )
  
  write.csv(stat_df,
            paste0("result/intermediate/",folder,"/",folder,"_stomata_stat.csv"),
            row.names = F)
  setTxtProgressBar(pb,1)
  # slope check -------------------------------------------------------------------------
  # find pic with big slope
  # slope_df %>% 
  #   group_by(pic_name) %>% 
  #   summarise(m=mean(slope)) %>% 
  #   filter(m>.01)
  
  # find the row overview
  # subslope <- slope_df %>% 
  #   select(pic_name,intercept,stomata.row)%>%
  #   tidyr::pivot_wider(names_from = "stomata.row",
  #                      values_from = "intercept",
  #                      names_prefix = "I") 
  # row distance
  # subslope <- slope_df %>% dplyr::filter(pic_name==pic_examp)
  # subslope %>% row_dist()
  
  slope_ls <- slope_df %>% group_by(pic_name) %>% group_split()
  
  system.time(
    rowdist_df <- foreach(
      subslope = slope_ls,
      .packages = c('dplyr','purrr')
    ) %dopar% {
      subslope %>% row_dist_fun()
    } %>% 
      map_dfr(.,~{.x})
  )
  
  rowdist_disperse <- rowdist_df %>%
    group_by(pic_name) %>%
    Disperse_1D(.,"row_dist") %>%
    left_join(rowdist_df,.,"pic_name")
  
  write.csv(rowdist_disperse,
            paste0("result/intermediate/",folder,"/",folder,"_rowdist_dispersion.csv"),
            row.names = F)
  setTxtProgressBar(pb,2)

# top salesman route -------------------------------------------------------------------------
  # system.time(
  #   topsalesman_df <- foreach(
  #     sub_df = df_ls,
  #     .packages = c('dplyr','ompr','ompr.roi','ROI.plugin.glpk')
  #   ) %dopar% {
  #     source("src/modules/2d_descriptor.R")
  #     sub_df %>% optim_d()
  #   } %>% 
  #     map_dfr(.,~{.x})
  # )
  # write.csv(topsalesman_df,
  #           paste0("result/intermediate/",folder,"/",folder,"_route.csv"),
  #           row.names = F)
  # setTxtProgressBar(pb,3)
  
  doParallel::stopImplicitCluster()
}
invisible(gc())
parallel::stopCluster(my.cluster)
invisible(gc())
