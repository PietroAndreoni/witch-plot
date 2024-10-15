
kotz <- read.csv("kotz_analysis_precipitations.csv") %>%
  rename(iso3=iso) %>%
  pivot_longer(!c(iso3)) %>%
  mutate(name=str_remove_all(name,"w_gr_prec_|sd")) %>%
  filter(str_detect(name,"_real")) %>%
  mutate(name=str_remove_all(name,"_real|plus")) %>%
  mutate(name=as.numeric(ifelse(str_detect(name,"minus"),paste0("-",str_remove(name,"minus")),name ) ) ) %>%
  inner_join(reg %>% 
               group_by(iso3) %>%
               summarise(minlat=min(lat),maxlat=max(lat),meanlat=mean(lat),
                         minlong=min(long),maxlong=max(long),meanlong=mean(long) ) %>%  
               mutate(latitude=abs(round(meanlat/15)*15) ) %>% 
               mutate(latitude=case_when(latitude==15 | n=="ind"  ~ "Tropical",
                                         latitude==0  ~ "Equatorial",
                                         latitude==30 ~ "Subtropical",
                                         latitude %in% c(45,60,75) ~ "High latitudes")) %>%
               mutate(latitude=ordered(latitude,c("Equatorial","Tropical","Subtropical","High latitudes"))) ) %>%
  group_by(latitude,name) %>%
  summarise(mean=mean(value),
            min=quantile(value,0.33),
            max=quantile(value,0.66) )

pnas <- base_prec %>% 
  inner_join(sd_prec) %>%
  cross_join(data.frame(name=c(-3,-1,1,3))) %>%
  group_by(n,name) %>%
  summarise( value = -0.0168*(prec0+name*sd)^2+0.0469*(prec0+name*sd)-(-0.0168*prec0^2+0.0469*prec0) ) %>%
  inner_join(countries_map) %>%
  group_by(latitude,name) %>%
  summarise(mean=median(value),
            min=quantile(value,0.33),
            max=quantile(value,0.66) )

damania <- base_prec %>% 
  inner_join(sd_prec) %>%
  cross_join(data.frame(name=c(-3,-1,1,3))) %>%
  group_by(n,name) %>%
  summarise( value = -0.00251*(prec0+name*sd)^2+0.01573*(prec0+name*sd)-(-0.00251*prec0^2+0.01573*prec0) ) %>%
  inner_join(countries_map) %>%
  group_by(latitude,name) %>%
  summarise(mean=median(value),
            min=quantile(value,0.33),
            max=quantile(value,0.66) )

bind_rows(kotz %>% mutate(impact="kotz",name=as.numeric(name)),
          damania %>% mutate(impact="damania"),
          pnas %>% mutate(impact="pnas") ) %>%
  ggplot() +
  geom_pointrange(aes(x=as.numeric(name),
                      ymin=min*100,
                      ymax=max*100,
                      y=mean*100,
                      color=latitude) ) +
  theme_pubr() + xlab("Precipitation variation [std]") + ylab("% loss GDP/yr") +
  theme(text=element_text(size=12)) +
  facet_wrap(impact~.,)