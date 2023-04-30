
### Protocols of making a regression plot
[![Click to read original article](Protocol.jpg)](https://besjournals.onlinelibrary.wiley.com/doi/full/10.1111/2041-210X.12577)



## Spikelet position practice
### Data overview

```{r,eval=FALSE}
dat <-read.csv('./data/kernel_combine.csv',
               header = T,stringsAsFactors = F) %>%
  dplyr::filter(tiller=="M")
glimpse(dat)
dat %>% 
  group_by(car,var,nitrogen,time,rep) %>% 
  distinct()
```

### classify spikelet based on position

the spike of the main shoot was dissected to count the total number of floret in

-   basal (third-fifth spikelet from the bottom)

-   central (middle spikelets)

-   apical (third-fifth spikelet from the top) spikelets throughout the spike [reference](https://doi.org/10.1016/j.fcr.2020.107908)

```{r,}
dat %<>%
  group_by(car,var,nitrogen,time,rep) %>% 
  mutate(type=cut(spike,3) %>% as.numeric(),
         type=case_when(type==1~"basal",
                        type==2~"central",
                        T~"apical")) %>% 
  group_by(car,var,nitrogen,time,rep,type) %>% 
  dplyr::arrange(spike) %>% 
  mutate(Fl=seq(1,n())) %>% 
  dplyr::arrange(var,nitrogen,time,rep,spike)
glimpse(dat)
```

## lookuptable of treatment (not yet)

## basic summary of kernel development summ for single spike

Sp: total spikelet
Fl: maximum floret 
sfl: total floret
kf: total full kernel
kh: total half kernel
ks: total small kernel
kp: potential kernel number 
fr: filling rate 
fc: potential filling rate

```{r }
sum.dat <- dat %>% 
  dplyr::group_by(nitrogen,time,var,rep,tiller,type) %>% 
  dplyr::summarise(
    Sp=max(spike),#total spikelet
    Fl=max(flower),# maximum floret 
    sfl=sum(flower),# total floret
    kf=sum(kernel.full),# total full kernel
    kh=sum(kernel.half),# total half kernel
    ks=sum(kernel.small),# total small kernel
    kp=kf+kh,# potential kernel number 
    fr=kf/sfl,# filling rate 
    fc=kf/kp)#potential filling rate
glimpse(sum.dat)
```

## Data wrangling and plot with facet

```{r }
long_format <- dat %>% 
  tidyr::pivot_longer(cols=c(nitrogen,time),
                      names_to = "treatment",
                      values_to = "levels") %>% 
  group_by(spike,var,treatment,levels,type,rep) %>%
  summarise(fertile_flower=max(kernel.full)) %>% 
  group_by(spike,var,treatment,levels,type) %>%
  summarise(fertile_flower=mean(fertile_flower))

long_format%>% 
  filter(fertile_flower<10) %>% 
  ggplot(aes(fertile_flower,spike,color=type,shape=levels))+
  geom_point()+
  facet_grid(treatment~var)+
  theme_classic()+
  scale_x_continuous(limits = c(0,5),breaks=seq(0,4))

```