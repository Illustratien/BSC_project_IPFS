library(dplyr)
library(ggplot2)
df<- read.csv("./posts/data/DFG2021.csv") %>% filter(namCombine=="plot_tkw")
ggplot(df%>% filter(var%in%c("Capone","Apertus",
                                "Patras","Pionier")) ,
       aes(var,Trait,color=timeid))+
  geom_point(shape=1)+
  theme_bw()+
  theme(panel.grid.minor.x = element_blank(),
        strip.background = element_blank())
