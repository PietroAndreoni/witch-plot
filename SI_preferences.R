sens_spread_coop <- sanitized_names %>% 
  filter(COOP=="coop" & tend==2200 & nsrm!="no SRM") 

coop_palette <- c("0.1"="#efbbff",
                  "1"="#0000FF",
                  "2"="#660066")

impacts_temp <- coef_T %>%
  inner_join(sens_spread_coop) %>%
  filter(pimp==1 & nsrm=="Cooperative") %>%
  cross_join(data.frame(temp=seq(-2,+3,by=0.1))) %>%
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
  theme_pubr() + ylab("% loss GDP/yr") + xlab("Local temperature increase to preindustrial [°C]") +
  facet_wrap(spread~.,) +
  theme(text=element_text(size=12))
ggsave("SI_spread_impacts.png",plot=impacts_temp,width=8.8, height=5, units="cm")

regtemp2100 <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(file,n) %>%
  mutate(temp=temp-preind) %>%
  inner_join(sens_spread_coop) %>%
  filter(ttoyear(t)==2100) %>% 
  inner_join(countries_map) %>% 
  inner_join(optimal_temperature) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  ggplot() +
  geom_point(aes(x=meanlat,
                  y=temp,
                  color=spread)) +
  stat_smooth(aes(x=meanlat,
                  y=temp,
                  color=spread,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  stat_smooth(data=.%>%filter(nsrm!="no SRM"),
              aes(x=meanlat,
                  y=opttemp-preind,
                  weight=pop, 
                  color=spread),
              linetype=2,
              se = FALSE,
              linewidth=2) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  scale_color_manual(values=coop_palette,
                     name="Temperature spread",
                     labels=c("0.1°C","1°C","2°C")) +
  xlab("") + ylab("Local temperature increase to preindustrial [°C]") + 
  theme_pubr() + theme(legend.position = "bottom",
                       text=element_text(size=12))

precip2100 <- PREC %>% rename(prec=value) %>% 
  inner_join(sens_spread_coop) %>%
  filter(ttoyear(t)==2100) %>%   
  inner_join(countries_map) %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sd_prec) %>%
  ggplot() +
  geom_hline(yintercept=0) +
  geom_ribbon(data=data.frame(lats=c(-50,75)),
              aes(x=lats,
                  ymin=-1,
                  ymax=1),
              color="grey",
              linewidth=1,
              alpha=0.2) +
  geom_point(aes(x=meanlat,
                 y=(prec-1)/sd,
                 color=spread),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=(prec-1)/sd,
                  color=spread,
                  weight=pop), 
              se = FALSE,
              linewidth=2 ) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  scale_color_manual(values=coop_palette,
                     name="Temperature spread",
                     labels=c("1°C","3°C","5°C")) +
  xlab("") + ylab("Precipitation variation [STD]") + 
  theme_pubr() + theme(legend.position = "bottom",
                       text=element_text(size=12))

damages2100 <- gdploss %>%  
  filter(ttoyear(t)==2100 ) %>% 
  inner_join(pop %>% rename(pop=value) ) %>%
  inner_join(sens_spread_coop) %>%
  inner_join(countries_map) %>%
  ggplot() + 
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point( aes(x=meanlat,
                  y=value*100,
                  color=spread),
              alpha=0.2) + 
  stat_smooth(data=.%>% filter(meanlat<=60),
              aes(x=meanlat,
                  y=value*100,
                  color=spread,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  scale_color_manual(values=coop_palette,
                     name="Temperature spread",
                     labels=c("0.1°C","1°C","2°C")) +
  theme_pubr() +   
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("GDP loss [%]") + theme(legend.position = "bottom",
                               text=element_text(size=12))

void <- ggplot() + theme_void() + theme(panel.background = element_rect(fill="white",color="white"))
fig2_coops <- ggarrange(ggarrange(regtemp2100,precip2100,nrow=1,common.legend=TRUE),
                        ggarrange(void,damages2100+theme(legend.position = "none"),void,nrow=1,widths=c(0.4,1,0.1)),
                        nrow=2,heights=c(1,1))
ggsave("SI_spread_coop.png",plot=fig2_coops,width=18, height=16, units="cm")



scoop <- Z_SRM %>% 
  inner_join(sens_spread_coop) %>%
  filter(ttoyear(t)<=2100  & !is.na(value) & !inj %in% c("60N","60S") ) %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),
                y=value,
                fill=ordered(inj,c("45S","30S","15S","0","15N","30N","45N"))),
            linewidth=1,
            color="black") +
  xlab("") + ylab("SAI [TgS/yr]") + 
  theme_pubr() + 
  facet_grid(spread~.) +
  scale_fill_manual(name="Injection latitude",
                    values=c("darkblue","#4a8dff","#CDDDFF","grey","#ffbaba","#ff5252","#a70000"))
ggsave("SI_spread_strategycoop.png",plot=scoop,width=9, height=5, units="cm")

##### 
main_scenarios_noncoop <- sanitized_names %>% 
filter(COOP=="noncoop" & tend==2200 & nsrm!="no SRM") 

regtemp2100 <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(file,n) %>%
  mutate(temp=temp-preind) %>%
  inner_join(main_scenarios_noncoop) %>%
  filter(ttoyear(t)==2100) %>% 
  inner_join(countries_map) %>% 
  inner_join(optimal_temperature) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  ggplot() +
  geom_point(aes(x=meanlat,
                  y=temp,
                  color=spread,
                  weight=pop))+ 
  stat_smooth(aes(x=meanlat,
                  y=temp,
                  color=spread,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  stat_smooth(data=.%>%filter(nsrm!="no SRM"),
              aes(x=meanlat,
                  y=opttemp-preind,
                  weight=pop, 
                  color=spread),
              linetype=2,
              se = FALSE,
              linewidth=2) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  scale_color_manual(values=coop_palette,
                     name="Temperature spread",
                     labels=c("0.1°C","1°C","2°C")) +
  xlab("") + ylab("Local temperature increase to preindustrial [°C]") + 
  facet_grid(.~nsrm,) +
  theme_pubr() + theme(legend.position = "bottom",
                       text=element_text(size=12))

precip2100 <- PREC %>% rename(prec=value) %>% 
  inner_join(main_scenarios_noncoop) %>%
  filter(ttoyear(t)==2100) %>%   
  inner_join(countries_map) %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sd_prec) %>%
  ggplot() +
  geom_hline(yintercept=0) +
  geom_ribbon(data=data.frame(lats=c(-50,75)),
              aes(x=lats,
                  ymin=-1,
                  ymax=1),
              color="grey",
              linewidth=1,
              alpha=0.2) +
  geom_point(aes(x=meanlat,
                 y=(prec-1)/sd,
                 color=spread),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=(prec-1)/sd,
                  color=spread,
                  weight=pop), 
              se = FALSE,
              linewidth=2 ) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  scale_color_manual(values=coop_palette,
                     name="Temperature spread",
                     labels=c("0.1°C","1°C","2°C")) +
  xlab("") + ylab("Precipitation variation [STD]") + 
  facet_grid(.~nsrm,) + 
  theme_pubr() + theme(legend.position = "bottom",
                       text=element_text(size=12))

damages2100 <- gdploss %>%  
  filter(ttoyear(t)==2100 ) %>% 
  inner_join(pop %>% rename(pop=value) ) %>%
  inner_join(main_scenarios_noncoop) %>%
  inner_join(countries_map) %>%
  ggplot() + 
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point( aes(x=meanlat,
                  y=value*100,
                  color=spread),
              alpha=0.2) + 
  stat_smooth(data=.%>% filter(meanlat<=60),
              aes(x=meanlat,
                  y=value*100,
                  color=spread,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  scale_color_manual(values=coop_palette,
                     name="Temperature spread",
                     labels=c("0.1°C","1°C","2°C")) +
  theme_pubr() +   
  facet_grid(.~nsrm,) +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("GDP loss [%]") + theme(legend.position = "bottom",
                               text=element_text(size=12))

void <- ggplot() + theme_void() + theme(panel.background = element_rect(fill="white",color="white"))
fig2_noncoops <- ggarrange(ggarrange(regtemp2100,precip2100,nrow=1,common.legend=TRUE),
                           ggarrange(void,damages2100+theme(legend.position = "none"),void,nrow=1,widths=c(0.4,1,0.1)),
                           nrow=2,heights=c(1,1))
ggsave("SI_spread_noncoop.png",plot=fig2_noncoops,width=18, height=16, units="cm")


snoncoop <- Z_SRM %>% 
  inner_join(main_scenarios_noncoop) %>%
  filter(ttoyear(t)<=2100  & !is.na(value) & !inj %in% c("60N","60S") ) %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),
                y=value,
                fill=ordered(inj,c("45S","30S","15S","0","15N","30N","45N"))),
            linewidth=1,
            color="black") +
  xlab("") + ylab("SAI [TgS/yr]") + 
  theme_pubr() + 
  facet_grid(spread~nsrm) +
  scale_fill_manual(name="Injection latitude",
                    values=c("darkblue","#4a8dff","#CDDDFF","grey","#ffbaba","#ff5252","#a70000"))
ggsave("SI_spread_strategynoncoop.png",plot=snoncoop,width=9, height=5, units="cm")
