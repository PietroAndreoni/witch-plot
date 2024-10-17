modulate_temp <- get_witch("modulate_damages") %>% 
  filter(d=="temp") %>% 
  select(-d) %>% 
  rename(modtemp=value)

impacts_temp <- coef_T %>%
  group_by(n,Coefficient) %>%
  filter(row_number()==1) %>%
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

modulate_prec <- get_witch("modulate_damages") %>% 
  filter(d=="prec") %>% 
  select(-d) %>% 
  rename(modprec=value)

impacts_prec <- coef_P %>%
  group_by(n,Coefficient) %>%
  filter(row_number()==1) %>%
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

coef_T_original <- coef %>% 
  filter(V1=="base_temp") %>%
  group_by(n) %>%
  summarise(a= - (0.0127184*value + -0.0004871*value**2 ),
         b = 0.0127184,
         c = -0.0004871 ) %>%
  pivot_longer(c(a,b,c),names_to="Coefficient") 

coef_P_original <- srm_regional_data["coefficients_kotz"] %>%
  filter(V2!="sym") %>%
  rename(spec=V2,coef=V3) %>% 
  inner_join(coef %>% filter(V1=="base_precip") %>% rename(base=value) %>% select(-V1,-file,-pathdir) %>% unique()) %>%
  inner_join(sd_prec) %>%
  group_by(n,spec) %>%
  summarise(a= -  value[coef=="c1"]/sd + value[coef=="c2"]/sd**2,
            b = value[coef=="c1"]/(sd*base*12*1e-3) - 2*base*12*1e-3*value[coef=="c2"]/(base*12*1e-3*sd)**2,
            c = value[coef=="c2"]/(sd*base*12*1e-3)**2) %>%
  pivot_longer(c(a,b,c),names_to="Coefficient") 

coef_P_pnas_original <-  coef %>% 
  filter(V1=="base_precip") %>%
  group_by(n) %>%
  summarise(a= - (0.0469*value*12*1e-3 + -0.0168 *(value*12*1e-3)**2 ),
            b = 0.0469,
            c = -0.0168 ) %>%
  pivot_longer(c(a,b,c),names_to="Coefficient") 

coef_P_ddz_original <-  coef %>% 
  filter(V1=="base_precip") %>%
  group_by(n) %>%
  summarise(a= - (0.01573*(value*12*1e-3) + -0.00251 *(value*12*1e-3)**2 ),
            b = 0.01573,
            c = -0.00251 ) %>%
  pivot_longer(c(a,b,c),names_to="Coefficient") 

figure_modtemp <- coef_T %>%
  group_by(n,Coefficient) %>%
  filter(row_number()==1) %>%
  cross_join(data.frame(temp=seq(-2,+4,by=0.1))) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  inner_join(modulate_temp) %>%
  group_by(n,file,temp) %>%
  summarise(value= modtemp*(value[Coefficient=="a"]+value[Coefficient=="b"]*(temp+preind)+value[Coefficient=="c"]*(temp+preind)^2),
            preind=preind) %>%
  inner_join(optimal_temperature) %>%
  inner_join(countries_map) %>%
  group_by(temp,latitude,file) %>%
  summarise(max=quantile(value,0.75),
            min=quantile(value,0.25),
            med=median(value),
            maxopt=quantile(opttemp-preind,0.75),
            minopt=quantile(opttemp-preind,0.25),
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
  geom_line(data=coef_T_original %>%
              cross_join(data.frame(temp=seq(-2,+4,by=0.1))) %>%
              inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
              inner_join(modulate_temp) %>%
              group_by(n,file,temp) %>%
              summarise(value= modtemp*(value[Coefficient=="a"]+value[Coefficient=="b"]*(temp+preind)+value[Coefficient=="c"]*(temp+preind)^2),
                        preind=preind) %>%
              inner_join(countries_map) %>%
              group_by(temp,latitude,file) %>%
              summarise(med=median(value) ),
            aes(x=temp,
                y=med*100,
                color=latitude),
            linewidth=1,linetype=2) +
  theme_pubr() + ylab("% loss GDP/yr") + xlab("Local temperature increase") +
  theme(text=element_text(size=12)) + facet_wrap(latitude~.,)
ggsave("Temperature.png",figure_modtemp,width=9,height=9)

figure_modprec <- coef_P %>%
  group_by(n,Coefficient) %>%
  filter(row_number()==1) %>%
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
            linewidth=1.3) +
  geom_line(data=coef_P_original %>%
      group_by(n,value,Coefficient) %>%
      cross_join(data.frame(prec=seq(-3,+3,by=0.1))) %>%
      ungroup() %>% 
      inner_join(coef %>% filter(V1=="base_precip") %>% mutate(baseprec=value*12*1e-3) %>% select(-V1,-value,-file,-pathdir)) %>%  
      inner_join(sd_prec) %>%
      inner_join(modulate_prec) %>%
      group_by(n,spec,prec) %>%
      summarise(value=modprec*(value[Coefficient=="a"]+value[Coefficient=="b"]*(baseprec+baseprec*prec*sd)+value[Coefficient=="c"]*((baseprec+baseprec*prec*sd))^2)) %>%
      inner_join(countries_map) %>%
      group_by(prec,latitude,spec) %>%
      summarise(med=median(value)),
              aes(x=prec,
                y=med*100,
                color=latitude,
                linetype=spec),
            linewidth=1) +
  geom_ribbon(data=coef_P_original %>%
              group_by(n,value,Coefficient) %>%
              cross_join(data.frame(prec=seq(-3,+3,by=0.1))) %>%
              ungroup() %>% 
              inner_join(coef %>% filter(V1=="base_precip") %>% mutate(baseprec=value*12*1e-3) %>% select(-V1,-value,-file,-pathdir)) %>%  
              inner_join(sd_prec) %>%
              inner_join(modulate_prec) %>%
              group_by(n,spec,prec) %>%
              summarise(value=modprec*(value[Coefficient=="a"]+value[Coefficient=="b"]*(baseprec+baseprec*prec*sd)+value[Coefficient=="c"]*((baseprec+baseprec*prec*sd))^2)) %>%
              inner_join(countries_map) %>%
                group_by(prec,latitude,spec) %>%
                summarise(max=quantile(value,0.75),
                          min=quantile(value,0.25)) %>%
                group_by(prec,latitude) %>%
                summarise(max=max(max),
                          min=min(min)),
            aes(x=prec,
                ymin=min*100,
                ymax=max*100,
                fill=latitude),
            alpha=0.2) +
  theme_pubr() + xlab("Precipitation variation [std]") + ylab("% loss GDP/yr") +
  theme(text=element_text(size=12)) + facet_wrap(latitude~.,) + 
  scale_linetype_manual(name="Precipitation/extremes correlation",values=c(2,3,4),labels=c("High","Low","Central"))
ggsave("Precipitation.png",figure_modprec,width=9,height=9)
