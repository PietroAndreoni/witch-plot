###### 
maps <- map_data("world")
maps=data.table(maps)
maps$iso3 = countrycode(maps$region, origin = 'country.name', destination =  'iso3c')
maps=as_tibble(maps)
reg <- left_join(maps,witchtools::region_mappings$maxiso3) %>% rename(n=maxiso3)

srm_regional_data <- gdx('../RICE50x/data_maxiso3/data_mod_srm_regional.gdx')
climate_regional_data <- gdx('../RICE50x/data_maxiso3/data_mod_climate_regional.gdx')
sec_data <- gdx('../RICE50x/data_maxiso3/data_baseline.gdx')

temp <- srm_regional_data["srm_temp_full"] %>% mutate(inj_lat=as.numeric(inj_lat),country_lat=as.numeric(country_lat))
prec <- srm_regional_data["srm_precip_full"] %>% mutate(inj_lat=as.numeric(inj_lat),country_lat=as.numeric(country_lat))
clim <- climate_regional_data["climate_region_coef_cmip5"]
pop2 <- sec_data["ssp_l"] %>% filter(V1=="ssp2") %>% mutate(t=as.numeric(t)) %>% rename(pop2=value,ssp=V1)
gdp <- sec_data["ssp_ykali"] %>% filter(V1=="ssp2") %>% mutate(t=as.numeric(t)) %>% rename(gdp=value,ssp=V1)
sd_prec <- srm_regional_data["precipitation_hist"] %>% group_by(n) %>% pivot_wider() %>% summarise(sd=sd/mean) %>%
  bind_rows(data.frame(n=setdiff(unique(pop2$n),unique(srm_regional_data["precipitation_hist"]$n)),sd=NA)) %>%
  ungroup() %>%
  mutate(sd=ifelse(is.na(sd),mean(sd,na.rm=TRUE),sd))

tempvar <- clim %>% filter(V1=="beta_temp")
precvar <- clim %>% filter(V1=="beta_precip")

tglobal <- data.frame(inj_lat = c(-60,-45,-30,-15,0,15,30,45,60), tg = c(0.95,NA,1.3,1.12,0.93,1.09,1.28,NA,1.06) )
predict(loess(tg~inj_lat,tglobal),c(-45,45))
tglobal <- data.frame(inj_lat = c(-60,-45,-30,-15,0,15,30,45,60), tg = c(0.95,1.2,1.3,1.12,0.93,1.09,1.28,1.22,1.06) )

pop2020 <- pop2 %>% filter(ssp=="ssp2" & t==2) %>% select(n,pop2)

theme_set(theme_gray(base_size = 7))
theme_set(theme_pubr(base_size = 7))

########################### temperature plots
reaction_t_srm <- cross_join(clim %>% 
                               filter(V1 %in% c("alpha_temp","beta_temp")) %>% 
                               pivot_wider(names_from=V1,values_from=value),
                             tglobal) %>%
  mutate(t_eq = beta_temp * tg)

f1a <- ggplot(temp %>% filter(inj_lat %in% c(-30,0,30) )) +
  geom_point(aes(x=country_lat, 
                 y=value,
                 color=as.factor(inj_lat)),alpha=0.2) +
  geom_point(data=unique(inner_join(reaction_t_srm %>% filter(inj_lat==0),temp %>% select(n,country_lat))),  
             aes(x=as.numeric(country_lat), y=t_eq ), 
             color="black", 
             alpha=0.1) +
  geom_smooth(data=.%>%
                inner_join(pop2020),
              aes(x=as.numeric(country_lat), 
                  y=value,
                  color=ordered(inj_lat,c(-60,-45,-30,-15,0,15,30,45,60)),
                  weight=pop2), 
              se = F, 
              linewidth=2)+
  geom_smooth(data=unique(inner_join(reaction_t_srm %>% filter(inj_lat==0),temp %>% select(n,country_lat))) %>%
                inner_join(pop2020),  
              aes(x=as.numeric(country_lat), 
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
         inner_join(unique(inner_join(reaction_p_srm,temp %>% select(n,country_lat)))) ) +
  geom_hline(yintercept=0,color="grey",linewidth=1) +
  geom_ribbon(aes(x=country_lat,
              ymin=-1,
              ymax=1),
              color="grey",
              linewidth=1,
              alpha=0.2) +
  geom_point(data=unique(inner_join(reaction_p_srm %>% filter(inj_lat==0),temp %>% select(n,country_lat))) %>% inner_join(sd_prec),  
             aes(x=as.numeric(country_lat), y=p_eq/sd ), 
             color="black", 
             alpha=0.1) +
  geom_point(aes(x=country_lat,
                 y=value/sd,
                 color=ordered(inj_lat,c(-60,-45,-30,-15,0,15,30,45,60))),
             alpha=0.2) +
  geom_smooth(data=unique(inner_join(reaction_p_srm %>% filter(inj_lat==0),temp %>% select(n,country_lat))) %>% 
                inner_join(sd_prec)%>%
                inner_join(pop2020),  
              aes(x=as.numeric(country_lat), 
                  y=p_eq/sd,
                  weight=pop2 ), 
              color="black", 
              se = F, 
              linetype="dotted", 
              linewidth=2 ) +
  geom_smooth(data=.%>%
                inner_join(pop2020),
              aes(x=country_lat, 
                  y=value/sd,
                  color=ordered(inj_lat,c(-60,-45,-30,-15,0,15,30,45,60)),
                  weight=pop2),
              se = F,
              linewidth=2) +
  scale_color_viridis_d() +
  scale_color_viridis_d(name = "Injection latitude") + xlab("Average country latitude") + ylab("Variation of average daily precipitation [standard deviations]")

f1 <- ggarrange(f1a,f1b,nrow=1,common.legend = TRUE,labels=c("a","b"))
ggsave("figure1.png",plot=f1,dpi=320,width=18,height=9,units="cm")

