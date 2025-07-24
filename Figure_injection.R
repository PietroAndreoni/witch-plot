###### 
emulator_data <- gdx('C:/Users/pietr/OneDrive - Politecnico di Milano/RICE50/RICE50x/data_maxiso3sai/data_mod_sai.gdx')
temp  <- emulator_data["srm_temperature_response_area"] %>% 
  mutate(w="area") %>%
  bind_rows(emulator_data["srm_temperature_response_pop"] %>%
  mutate(w="pop")) %>%
  rename(inj=V2,se=V3) %>% 
  mutate(inj_lat=ifelse(str_detect(inj,"S"),as.numeric(paste0("-",str_remove(inj,"S"))),as.numeric(str_remove(inj,"N")))  ) %>%
  inner_join(countries_map)
prec  <- emulator_data["srm_precip_response_area"] %>% 
  mutate(w="area") %>%
  bind_rows(emulator_data["srm_precip_response_pop"] %>%
              mutate(w="pop")) %>%
  rename(inj=V2,se=V3) %>% 
  mutate(inj_lat=ifelse(str_detect(inj,"S"),as.numeric(paste0("-",str_remove(inj,"S"))),as.numeric(str_remove(inj,"N")))  ) %>%
  inner_join(countries_map)

# temp <- get_witch("sai_temp") %>% 
#   mutate(inj_lat=ifelse(str_detect(inj,"S"),as.numeric(paste0("-",str_remove(inj,"S"))),as.numeric(str_remove(inj,"N")))  ) %>%
#   inner_join(countries_map)
# prec <- get_witch("sai_precip") %>% 
#   mutate(inj_lat=ifelse(str_detect(inj,"S"),as.numeric(paste0("-",str_remove(inj,"S"))),as.numeric(str_remove(inj,"N")))  ) %>%
#   inner_join(countries_map)
tempvar <- clim %>% filter(V1=="beta_temp")
precvar <- clim %>% filter(V1=="beta_precip")

tglobal <- data.frame(inj_lat = c(-60,-45,-30,-15,0,15,30,45,60), tg = c(0.95,NA,1.3,1.12,0.93,1.09,1.28,NA,1.06) )
predict(loess(tg~inj_lat,tglobal),c(-45,45))
tglobal <- data.frame(inj_lat = c(-60,-45,-30,-15,0,15,30,45,60), tg = c(0.95,1.2,1.3,1.12,0.93,1.09,1.28,1.22,1.06) )

pop2020 <- pop %>% filter(t==2) %>% rename(pop2=value) %>% select(n,pop2)

########################### temperature plots
reaction_t_srm <- cross_join(clim %>% 
                               filter(V1 %in% c("alpha_temp","beta_temp")) %>% 
                               pivot_wider(names_from=V1,values_from=value),
                             tglobal) %>%
  mutate(t_eq = beta_temp * tg)

f1a <- ggplot(temp %>% filter(inj_lat %in% c(-30,0,30) ) ) +
  geom_point(data=.%>%
               filter(se=="best" & w=="area"),
             aes(x=meanlat,
                 y=value,
                 color=as.factor(inj_lat)),
             alpha=0.2) +
  geom_errorbar(data=.%>% group_by(n,inj_lat) %>% summarise(meanlat=mean(meanlat),lo=min(value),up=max(value)),
                  aes(x=meanlat, 
                    ymin=lo,
                    ymax=up,
                    color=as.factor(inj_lat)),
                alpha=0.2) +
  geom_point(data=unique(inner_join(reaction_t_srm %>% filter(inj_lat==0),temp %>% select(n,meanlat))),  
             aes(x=as.numeric(meanlat), y=t_eq ), 
             color="black", 
             alpha=0.1) +
  geom_smooth(data=.%>%
                inner_join(pop2020),
              aes(x=as.numeric(meanlat), 
                  y=value,
                  color=ordered(inj_lat,c(-60,-45,-30,-15,0,15,30,45,60))), 
              se = F, 
              linewidth=2)+
  geom_smooth(data=unique(inner_join(reaction_t_srm %>% filter(inj_lat==0),temp %>% select(n,meanlat))) %>%
                inner_join(pop2020),  
              aes(x=as.numeric(meanlat), 
                  y=t_eq,
                  weight=pop2), 
              color="black", 
              se = F, 
              linetype="dotted", 
              linewidth=2 ) +
  scale_color_viridis_d(name = "Injection latitude") + xlab("Average country latitude") + ylab("Cooling [°C]")

########################## precipitation response
reaction_p_srm <- cross_join(clim %>% 
                               filter(V1 %in% c("alpha_precip","beta_precip")) %>% 
                               pivot_wider(names_from=V1,values_from=value),
                             tglobal) %>%
  mutate(p_eq = - beta_precip * tg / alpha_precip)

f1b <- ggplot(prec %>% inner_join(sd_prec) %>%
         filter(inj_lat %in% c(-30,0,30)) %>% 
         inner_join(unique(inner_join(reaction_p_srm,prec %>% select(n,meanlat)))) ) +
  geom_hline(yintercept=0,color="grey",linewidth=1) +
  geom_ribbon(aes(x=meanlat,
              ymin=-1,
              ymax=1),
              color="grey",
              linewidth=1,
              alpha=0.2) +
  geom_point(data=unique(inner_join(reaction_p_srm %>% filter(inj_lat==0),prec %>% select(n,meanlat))) %>% inner_join(sd_prec),  
             aes(x=as.numeric(meanlat), 
                 y=p_eq/sd ), 
             color="black", 
             alpha=0.1) +
  geom_point(data=.%>%filter(se=="best" & w=="area"),aes(x=meanlat,
                                             y=value/sd,
                                             shape=w,
                                             color=ordered(inj_lat,c(-60,-45,-30,-15,0,15,30,45,60))),alpha=0.2) +
  geom_errorbar(data=.%>% group_by(n,inj_lat) %>% 
                  summarise(meanlat=mean(meanlat),lo=min(value),up=max(value), sd=mean(sd)) %>%
                  rowwise() %>% mutate(lo=max(-3.5,lo/sd),up=min(3.5,up/sd)),
                aes(x=meanlat,
                    ymin=lo,
                    ymax=up,
                    color=ordered(inj_lat,c(-60,-45,-30,-15,0,15,30,45,60))),alpha=0.2) +
  geom_smooth(data=unique(inner_join(reaction_p_srm %>% filter(inj_lat==0),prec %>% filter(se=="best" & w=="area") %>% select(n,meanlat))) %>% 
                inner_join(sd_prec)%>%
                inner_join(pop2020),  
              aes(x=as.numeric(meanlat), 
                  y=p_eq/sd ), 
              color="black", 
              se = F, 
              linetype="dotted", 
              linewidth=2) +
  geom_smooth(data=.%>%
                inner_join(pop2020),
              aes(x=meanlat, 
                  y=value/sd,
                  color=ordered(inj_lat,c(-60,-45,-30,-15,0,15,30,45,60))),
              se = F,
              linewidth=2) +
#  ylim(c(-3.51,+3.51)) +
  scale_color_viridis_d() +
  scale_color_viridis_d(name = "Injection latitude") + xlab("Average country latitude") + ylab("Variation of average daily precipitation [standard deviations]")

f1 <- ggarrange(f1a,f1b,nrow=1,common.legend = TRUE,labels=c("a","b"))
ggsave("Injection.png",plot=f1,dpi=320,width=18,height=9,units="cm")


inj_prec <- ggplot(prec %>% 
                     group_by(n,inj) %>%
                     filter(row_number()==1) %>%
         select(n,inj,value) %>%
         inner_join(sd_prec) %>%
         left_join(reg %>% filter(iso3!='ATA')) ) +
         geom_polygon(aes(x = long, y = lat,group = group, fill = value/sd),size=.1,color="black") +
  facet_wrap(ordered(inj,c("60N","45N","30N","15N","0","15S","30S","45S","60S"))~.,) + 
  scale_fill_gradient2(name="Precipitation variation [SD]")
ggsave("SI_injprec.png",plot=inj_prec,dpi=320,width=18,height=18,units="cm")


inj_temp <- ggplot(temp %>% 
                     group_by(n,inj) %>%
                     filter(row_number()==1) %>%
         select(n,inj,value) %>%
         left_join(reg %>% filter(iso3!='ATA')) ) +
  geom_polygon(aes(x = long, y = lat,group = group, fill = value),size=.1,color="black") +
  facet_wrap(ordered(inj,c("60N","45N","30N","15N","0","15S","30S","45S","60S"))~.,) + 
  scale_fill_gradient2(name="Temperature variation °C")
ggsave("SI_injtemp.png",plot=inj_temp,dpi=320,width=18,height=18,units="cm")
