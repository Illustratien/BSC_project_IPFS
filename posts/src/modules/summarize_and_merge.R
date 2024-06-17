pacman::p_load(dplyr,ggplot2,broom,purrr,tidyr,foreach)
# raw data preparation-------------------------------------------------------------------------
folders <-list.dirs("data",recursive = F) %>% gsub("data/","",.)
pb = txtProgressBar(min = 0, max = length(folders),
                    style = 3,    # Progress bar style (also available style = 1 and style = 2)
                    width = 30,initial = 0)
message("merge intermediate results:")

iwalk(folders,function(folder,i){
  message(folder)
  inter_path <- paste0("./result/intermediate/",folder)
  rowdis <- paste0(inter_path,"/",folder,"_rowdist_dispersion.csv") %>% read.csv() %>% 
    select(-c(row_dist:row_combi)) %>% distinct()
  stat<- paste0(inter_path,"/",folder,"_stomata_stat.csv")%>% read.csv() %>% 
    select(-c(stomata.row:row_kurtosis,stomata.complete_count)) %>% distinct()
  row_stat<- paste0(inter_path,"/",folder,"_stomata_stat.csv")%>% read.csv() %>% 
    select(pic_name:row_kurtosis,-stomata.row) %>% 
    mutate(pic_name=as.factor(pic_name)) %>% 
    group_by(pic_name) %>% 
    summarise(across(where(is.numeric),mean),
              .groups = 'drop')
  raw <- paste0(inter_path,"/",folder,"_xml_data.csv")%>% read.csv() %>% 
    group_by(pic_name,stomata.type) %>% 
    summarise(stomata.type.count=n(),.groups = 'drop') %>% 
    pivot_wider(names_from = stomata.type,values_from = stomata.type.count)
  slope <- paste0(inter_path,"/",folder,"_slope.csv")%>% read.csv() %>% 
    group_by(pic_name) %>% 
    summarise(across(stomata.per.row,list(mean=mean,sd=sd)),
              across(slope,
                     list(mean=mean,sd=sd),.names=paste0("row_","{.col}_{.fn}")
                    
              ), .groups = 'drop')
  
  res <- list(raw,stat,slope,rowdis,row_stat) %>%
    Reduce(function(x, y) left_join(x, y, by = "pic_name"), .) %>% 
    mutate(Descriptor2D= "not available now")
  
  write.csv(res,
            paste0("result/",folder,"_summary.csv"),row.names = F)
  setTxtProgressBar(pb,i)
})
# columns_with_na <- colSums(is.na(res))
# 
# # Print the column names with NA values
# print(names(columns_with_na)[columns_with_na > 0])