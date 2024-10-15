modulate_temp <- get_witch("modulate_damages") %>% 
  filter(d=="temp") %>% 
  select(-d) %>% 
  rename(modtemp=value)

impacts_temp <- coef_T %>%
  filter(file %in% c("coop_maxiso3_SRMno_INJno_POLcba_IMPT10modified_IMPP10original_TSPR1")) %>%
  cross_join(data.frame(temp=seq(0,+3,by=0.1))) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  inner_join(modulate_temp) %>%
  group_by(n,file,temp) %>%
  summarise(value= modtemp*(value[Coefficient=="a"]+value[Coefficient=="b"]*(temp+preind)+value[Coefficient=="c"]*(temp+preind)^2),
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
  theme_pubr() + ylab("% loss GDP/yr") + xlab("Global temperature increase") +
  theme(text=element_text(size=12)) 

kotz <- read.csv("kotz_analysis_precipitations.csv") %>%
  rename(iso3=iso) %>%
  pivot_longer(!c(iso3)) %>%
  mutate(name=str_remove_all(name,"w_gr_prec_|sd")) %>%
  filter(str_detect(name,"_real")) %>%
  mutate(name=str_remove_all(name,"_real|plus")) %>%
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

modulate_prec <- get_witch("modulate_damages") %>% 
  filter(d=="prec") %>% 
  select(-d) %>% 
  rename(modprec=value)

impacts_prec <- coef_P %>%
  filter(file %in% c("noncoop_maxiso3_SRMbra_INJfree_POLcba_IMPT10modified_IMPP10original_TSPR1") ) %>%
  group_by(n,file,value,Coefficient) %>%
  cross_join(data.frame(prec=seq(-3,+3,by=0.1))) %>%
  ungroup() %>% 
  inner_join(coef %>% filter(V1=="base_precip") %>% mutate(baseprec=value*12*1e-3) %>% select(-V1,-value)) %>%  
  inner_join(sd_prec) %>%
  inner_join(modulate_prec) %>%
  group_by(n,file,prec) %>%
  summarise(value=modprec*(value[Coefficient=="a"]+value[Coefficient=="b"]*(baseprec+baseprec*prec*sd)+value[Coefficient=="c"]*((baseprec+baseprec*prec*sd))^2)) %>%
  inner_join(countries_map) %>%
  group_by(prec,latitude,file) %>%
  summarise(max=quantile(value,0.66),
            min=quantile(value,0.33),
            med=median(value)) %>%
  unique() %>%
  inner_join(sanitized_names) %>%
  inner_join(coef %>% filter(V1=="alpha_precip") %>% rename(preind=value)) %>%
  ggplot() +
  geom_line(aes(x=prec,
                y=med*100,
                color=latitude),
            linewidth=1) +
  geom_ribbon(aes(x=prec,
                ymin=min*100,
                ymax=max*100,
                fill=latitude,
                group=interaction(latitude,file) ),
              alpha=0.2) +
  scale_linetype_manual(values=c(2,1),
                        name="Precipitation impacts",
                        labels=c("Low","High") ) + 
  theme_pubr() + xlab("Precipitation variation [std]") + ylab("% loss GDP/yr") +
  theme(text=element_text(size=12))
impacts <- ggarrange(impacts_temp,impacts_prec,common.legend=TRUE)  

ggsave("Impacts.png",impacts,width=18,height=9)

coef_P %>%
  filter(file %in% c("noncoop_maxiso3_SRMbra_INJfree_POLcba_IMPT10modified_IMPP10original_TSPR1") & 
           n %in% c("chn","bra","usa","ind") ) %>%
  group_by(n,file,value,Coefficient) %>%
  cross_join(data.frame(prec=seq(-3,+3,by=0.1))) %>%
  ungroup() %>% 
  inner_join(coef %>% filter(V1=="base_precip") %>% mutate(baseprec=value*12*1e-3) %>% select(-V1,-value)) %>%  
  inner_join(sd_prec) %>%
  inner_join(modulate_prec) %>%
  group_by(n,file,prec) %>%
  summarise(med=modprec*(value[Coefficient=="a"]+value[Coefficient=="b"]*(baseprec+baseprec*prec*sd)+value[Coefficient=="c"]*((baseprec+baseprec*prec*sd))^2)) %>%
  inner_join(countries_map) %>%
  unique() %>%
  inner_join(sanitized_names) %>%
  inner_join(coef %>% filter(V1=="alpha_precip") %>% rename(preind=value)) %>%
  ggplot() +
  geom_line(aes(x=prec,
                y=med*100,
                color=n,
                linetype=file),
            linewidth=1) +
  scale_linetype_manual(values=c(2,1),
                        name="Precipitation impacts",
                        labels=c("Low","High") ) + 
  theme_pubr() + xlab("Precipitation variation [std]") + ylab("% loss GDP/yr") +
  theme(text=element_text(size=12))

