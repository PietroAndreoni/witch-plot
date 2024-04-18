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
pop <- sec_data["ssp_l"] %>% filter(V1=="ssp2") %>% mutate(t=as.numeric(t)) %>% rename(pop=value,ssp=V1)
gdp <- sec_data["ssp_ykali"] %>% filter(V1=="ssp2") %>% mutate(t=as.numeric(t)) %>% rename(gdp=value,ssp=V1)

tempvar <- clim %>% filter(V1=="beta_temp")
precvar <- clim %>% filter(V1=="beta_precip")

tglobal <- data.frame(inj_lat = c(-60,-45,-30,-15,0,15,30,45,60), tg = c(0.95,NA,1.3,1.12,0.93,1.09,1.28,NA,1.06) )
predict(loess(tg~inj_lat,tglobal),c(-45,45))
tglobal <- data.frame(inj_lat = c(-60,-45,-30,-15,0,15,30,45,60), tg = c(0.95,1.2,1.3,1.12,0.93,1.09,1.28,1.22,1.06) )


theme_set(theme_gray(base_size = 7))
theme_set(theme_pubr(base_size = 7))

########################### temperature plots
reaction_t_srm <- cross_join(clim %>% 
                               filter(V1 %in% c("alpha_temp","beta_temp")) %>% 
                               pivot_wider(names_from=V1,values_from=value),
                             tglobal) %>%
  mutate(t_eq = beta_temp * tg)

f1a <- ggplot(temp %>% filter(inj_lat < 31 & inj_lat > - 31)) +
  geom_point(aes(x=country_lat, 
                 y=value,
                 color=as.factor(inj_lat)),alpha=0.2) +
  geom_smooth(aes(x=as.numeric(country_lat), y=value,color=ordered(inj_lat,c(-60,-45,-30,-15,0,15,30,45,60))), 
              se = F, 
              linewidth=2)+
  geom_smooth(data=unique(inner_join(reaction_t_srm %>% filter(inj_lat==0),temp %>% select(n,country_lat))),  
              aes(x=as.numeric(country_lat), y=t_eq ), 
              color="black", 
              se = F, 
              linetype="dotted", 
              linewidth=2 ) +
  geom_point(data=unique(inner_join(reaction_t_srm %>% filter(inj_lat==0),temp %>% select(n,country_lat))),  
              aes(x=as.numeric(country_lat), y=t_eq ), 
              color="black", 
             alpha=0.1) +
  scale_color_viridis_d(name = "Injection latitude") + xlab("Average country latitude") + ylab("Cooling [°C]")

f1b <- ggplot(tglobal) +
  geom_point(aes(x=inj_lat,y=tg)) +
  geom_smooth(aes(x=inj_lat,y=tg)) + xlab("Injection latitude") + ylab("Global cooling [°C]")

f1c <- ggplot(temp %>% filter(inj_lat %in% c(-30,0,30)) ) +
  geom_smooth(aes(x=country_lat, 
                  y=value,
                  color=as.factor(inj_lat)))+
  geom_point(aes(x=country_lat, 
                 y=value,
                 color=as.factor(inj_lat)),alpha=0.5) +
  geom_smooth(data=inner_join(reaction_t_srm,temp %>% 
                                select(n,country_lat)) %>% 
                filter(inj_lat %in% c(-30,0,30)),  
              aes(x=as.numeric(country_lat), y=t_eq ), color="black" ) +
  geom_point(data=inner_join(reaction_t_srm,temp %>% 
                               select(n,country_lat))  %>% 
               filter(inj_lat %in% c(-30,0,30)),  
             aes(x=as.numeric(country_lat), y=t_eq ), color="black",alpha=0.5 ) +
  geom_hline(data=tglobal %>% filter(inj_lat %in% c(-30,0,30)),aes(yintercept=tg),linetype="dotted") +
  facet_grid(.~inj_lat,) +
  scale_color_viridis_d(name = "Injection latitude")  + xlab("Average country latitude") + ylab("Cooling [°C]")

empty <- ggplot() + theme_void()                      
f1 <- ggarrange(ggarrange(f1a,f1b,nrow=1,common.legend = TRUE,labels=c("a","b")),f1c + theme(legend.position="none"),nrow=2,labels = c("","c"))
ggsave("figure1.png",plot=f1,dpi=320,width=18,height=16,units="cm")

f2 <- ggplot(inner_join(reaction_t_srm,temp) %>% 
               mutate(discr = arules::discretize( value-t_eq, method="fixed",breaks=c(-2,-1,-0.5,-0.1,0.1,0.5,1,2)) ) %>%
               full_join(reg) %>% 
         filter(iso3!='ATA' & inj_lat %in% c(-30,0,30) ) ) +
  geom_polygon(aes(x = long, y = lat,group = group, fill = discr),color='black',size=.1)+
  geom_hline(data=data.frame(x=c(-60,-45,-30,-15,0,15,30,45,60)),aes(yintercept=x),linetype="dotted",color="grey") +
  geom_hline(data=data.frame(inj_lat=c(-30,0,30)),aes(yintercept=inj_lat),color="green",linewidth=1.5) +
  theme_void()+
  scale_fill_manual(values=c("#660000","#cc0000","#ea9999","white","#9fc5e8","#3d85c6","#073763") ) +
  facet_wrap(as.factor(inj_lat)~.,) +
  theme(legend.position = "bottom", strip.text.x = element_text(size=12, face="bold"),legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
ggsave("figure2.png",plot=f2,dpi=320,width=18,height=10,units="cm")

########################## precipitation response
reaction_p_srm <- cross_join(clim %>% 
                               filter(V1 %in% c("alpha_precip","beta_precip")) %>% 
                               pivot_wider(names_from=V1,values_from=value),
                             tglobal) %>%
  mutate(p_eq = beta_precip * tg / alpha_precip)

f3a <- ggplot(prec %>% 
         filter(inj_lat < 31 & inj_lat > - 31) %>% 
         inner_join(unique(inner_join(reaction_p_srm,temp %>% select(n,country_lat)))) ) +
  geom_hline(yintercept=0,color="grey",linewidth=1) +
  geom_smooth(aes(x=country_lat, y=value,color=ordered(inj_lat,c(-60,-45,-30,-15,0,15,30,45,60))), 
              se = F, 
              linewidth=2)+
  scale_color_viridis_d() +
  scale_color_viridis_d(name = "Injection latitude") + xlab("Average country latitude") + ylab("Variation of average daily precipitation [frac]")

f3b <- ggplot(prec %>% filter(inj_lat %in% c(-30,0,30))) +
  geom_smooth(aes(x=country_lat, y=value,color=as.factor(inj_lat)))+
  geom_point(aes(x=country_lat, y=value,color=as.factor(inj_lat)),alpha=0.5) +
  facet_grid(.~inj_lat,) +
  scale_color_viridis_d(name = "Injection latitude") + xlab("Average country latitude") + ylab("Variation of average daily precipitation [frac]")

f3c <- ggplot(inner_join(reaction_p_srm,prec) %>%  
                rowwise() %>%
                mutate(value=value*(1+p_eq)+p_eq) %>% 
         mutate(discr = arules::discretize( value, method="fixed",breaks=c(-1,-0.2,-0.05,-0.01,0.01,0.05,0.2,2)) ) %>%
         filter(inj_lat %in% c(-30,0,30)) %>%
         full_join(reg) %>% 
         filter(iso3!='ATA' & !is.na(value)) ) +
  geom_polygon(aes(x = long, y = lat,group = group, fill = discr),color='black',size=.1) +
  geom_hline(data=data.frame(x=c(-60,-45,-30,-15,0,15,30,45,60)),aes(yintercept=x),linetype="dotted",color="grey") +
  geom_hline(data=data.frame(inj_lat=c(-30,0,30)),aes(yintercept=inj_lat),color="green",linewidth=1.5) +
  theme_void()+
  scale_fill_manual(values=c("#660000","#cc0000","#ea9999","white","#9fc5e8","#3d85c6","#073763") ) +
  facet_wrap(as.factor(inj_lat)~.,) +
  theme(legend.position = "bottom", strip.text.x = element_text(size=12, face="bold"),legend.title = element_blank(),plot.title = element_text(hjust = 0.5))

f3 <- ggarrange(ggarrange(f3a,f3b,widths = c(0.4,0.6),common.legend=TRUE,labels=c("a","b")),f3c,nrow=2,labels=c("","c"))
ggsave("figure3.png",plot=f3,dpi=320,width=18,height=16,units="cm")

f4a <- ggplot(prec %>% 
         filter(n %in% c("usa","fra","gbr","bra","ind","chn","rus","nga","arg","mex","pak","zaf")) %>%
         inner_join(unique(inner_join(reaction_p_srm,prec %>% select(n,country_lat)))) ) +
  geom_smooth(aes(x=inj_lat, y=value,color=n),se = F)+
  geom_point(aes(x=inj_lat, y=value,color=n)) +
  facet_wrap(.~n,nrow=3) + theme(legend.position = "none") +
  scale_color_manual(values=c(region_palette_ed57,"row"="white",ind="green",pak="lightblue",nga="darkgreen")) +
  xlab("Injection point") + ylab("Variation of average daily precipitation [frac]")


f4b <- ggplot(reg %>% 
  mutate(nplot=ifelse(n %in% c("usa","fra","gbr","bra","ind","chn","rus","nga","arg","mex","pak","zaf"),n,"row")) %>%
  filter(iso3!='ATA')) +
  geom_polygon(aes(x = long, y = lat,group = group, fill = nplot),color='black',size=.1)+
  theme_void()+
  theme(legend.position = "none", strip.text.x = element_text(size=12, face="bold"),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +
  scale_fill_manual(values=c(region_palette_ed57,"row"="white",ind="green",pak="lightblue",nga="darkgreen")) 
f4 <- ggarrange(f4a,ggarrange(empty,f4b,empty,nrow=1,widths=c(0.2,0.6,0.2)),heights = c(0.6,0.4),nrow=2,labels=c("a","b"))
ggsave("figure4.png",plot=f4,dpi=320,width=18,height=16,units="cm")
