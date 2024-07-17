impacts_temp <- coef_T %>%
  filter(file=="coop_maxiso3_SRMusa_INJfree_POLcba_IMPT10modified_IMPP10modified_TSPR1") %>%
  cross_join(data.frame(temp=seq(0,+2,by=0.1))) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(n,file,temp) %>%
  summarise(value=value[Coefficient=="a"]+value[Coefficient=="b"]*(temp+preind)+value[Coefficient=="c"]*(temp+preind)^2,
            preind=preind) %>%
  inner_join(optimal_temperature) %>%
  inner_join(countries_map) %>%
  group_by(temp,latitude,file) %>%
  summarise(max=quantile(value,0.66),
            min=quantile(value,0.33),
            med=median(value),
            maxopt=quantile(opttemp-preind,0.66),
            minopt=quantile(opttemp-preind,0.33),
            medopt=median(opttemp-preind) ) %>% 
  unique() %>%
  inner_join(sanitized_names) %>%
  ggplot() +
  geom_line(aes(x=temp,
                y=med*100,
                color=latitude),
            linewidth=1) +
  geom_ribbon(aes(x=temp,
                  ymin=min*100,
                  ymax=max*100,
                  fill=latitude),
              alpha=0.2) +
  geom_vline(aes(xintercept=medopt,color=latitude)) +
  theme_pubr() + ylab("% loss GDP/yr") + xlab("Global temperature increase")

kotz <- read.csv("kotz_analysis_precipitations.csv") %>%
  rename(iso3=iso) %>%
  pivot_longer(!c(iso3)) %>%
  filter(str_detect(name,"real")) %>%
  mutate(name=str_remove_all(name,"w_gr_prec_|sd_real|plus")) %>%
  mutate(name=ifelse(str_detect(name,"minus"),paste0("-",str_remove(name,"minus")),name ) ) %>%
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


impacts_prec <- coef_P %>%
  filter(file %in% c("noncoop_maxiso3_SRMusa_INJfree_POLcba_IMPT10modified_IMPP50modified_TSPR1",
                     "noncoop_maxiso3_SRMusa_INJfree_POLcba_IMPT10modified_IMPP10modified_TSPR1")  ) %>%
  group_by(n,file,value,Coefficient) %>%
  expand(prec=seq(-3,+3,by=0.1)) %>%
  inner_join(optimal_precipitation) %>%
  inner_join(sd_prec) %>%
  group_by(n,file,prec) %>%
  summarise(value=value[Coefficient=="a"]+value[Coefficient=="b"]*(optprec+optprec*prec*sd)/1000+value[Coefficient=="c"]*((optprec+optprec*prec*sd)/1000)^2) %>%
  inner_join(countries_map) %>%
  group_by(prec,latitude,file) %>%
  summarise(max=quantile(value,0.66),
            min=quantile(value,0.33),
            med=median(value)) %>%
  unique() %>%
  inner_join(optimal_precipitation) %>%
  inner_join(sanitized_names) %>%
  inner_join(coef %>% filter(V1=="alpha_precip") %>% rename(preind=value)) %>%
  filter(nsrm!="no SRM" & pimp %in% c(5,1) ) %>%
  ggplot() +
  geom_pointrange(data=kotz,
                aes(x=as.numeric(name),
                    ymin=min*100,
                    ymax=max*100,
                    y=mean*100,
                    color=latitude) ) +
  geom_line(aes(x=prec,
                y=med*as.numeric(pimp)*100,
                color=latitude,
                linetype=pimp),
            linewidth=1) +
  geom_ribbon(aes(x=prec,
                ymin=min*as.numeric(pimp)*100,
                ymax=max*as.numeric(pimp)*100,
                fill=latitude,
                group=interaction(pimp,latitude) ),
              alpha=0.2) +
  scale_linetype_manual(values=c(2,1),
                        name="Precipitation impacts",
                        labels=c("Low","High") ) + 
  theme_pubr() + xlab("Precipitation variation [std]") + ylab("% loss GDP/yr")

impacts <- ggarrange(impacts_temp,impacts_prec,common.legend=TRUE)   
ggsave("impacts.png",impacts,width=8.8,height=5)
