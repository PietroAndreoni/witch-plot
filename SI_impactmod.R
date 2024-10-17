#run first plotgdx_rice_main_scenarios with 
# witch_folder = "../Results_srm/All161024/Impacts_mod" 

sens_prec_coop <- sanitized_names %>% 
  filter(COOP=="coop" & tend==2200 & nsrm!="no SRM") 

coop_palette <- c("0.2"="#add8e6",
                  "0.5"="#72bcd4", 
                  "1"="#0000FF",
                  "2"="#121B54")

modulate_prec <- get_witch("modulate_damages") %>% 
  filter(d=="prec") %>% 
  select(-d) %>% 
  rename(modprec=value) %>%
  inner_join(sens_prec_coop)

impacts_prec <- coef_P %>%
  group_by(n,file,value,Coefficient) %>%
  cross_join(data.frame(prec=seq(-3,+3,by=0.1))) %>%
  ungroup() %>% 
  inner_join(coef %>% filter(V1=="base_precip") %>% mutate(baseprec=value*12*1e-3) %>% select(-V1,-value)) %>%  
  inner_join(sd_prec) %>%
  inner_join(modulate_prec) %>%
  group_by(n,prec,pimp) %>%
  summarise(value=modprec*(value[Coefficient=="a"]+value[Coefficient=="b"]*(baseprec+baseprec*prec*sd)+value[Coefficient=="c"]*((baseprec+baseprec*prec*sd))^2)) %>%
  inner_join(countries_map) %>%
  group_by(prec,latitude,pimp) %>%
  summarise(max=quantile(value,0.66),
            min=quantile(value,0.33),
            med=median(value)) %>%
  unique() %>%
  inner_join(sens_prec_coop) %>%
  inner_join(coef %>% filter(V1=="alpha_precip") %>% rename(preind=value)) %>%
  ggplot() +
  geom_line(aes(x=prec,
                y=med*100,
                color=latitude,
                linetype=pimp),
            linewidth=1) +
  geom_ribbon(aes(x=prec,
                  ymin=min*100,
                  ymax=max*100,
                  fill=latitude,
                  group=interaction(latitude,file) ),
              alpha=0.2) +
  scale_linetype_manual(values=c(2,3,1,4),
                        name="Precipitation impacts",
                        labels=c("x0.2","x0.5","Central","x2") ) + 
  theme_pubr() + xlab("Precipitation variation [std]") + ylab("% loss GDP/yr") +
  theme(text=element_text(size=12))
ggsave("SI_impmod_impacts.png",plot=impacts_prec,width=5, height=5, units="cm")

regtemp2100 <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(file,n) %>%
  mutate(temp=temp-preind) %>%
  inner_join(sens_prec_coop) %>%
  filter(ttoyear(t)==2100) %>% 
  inner_join(countries_map) %>% 
  inner_join(optimal_temperature) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  ggplot() +
  stat_smooth(aes(x=meanlat,
                  y=temp,
                  color=pimp,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  stat_smooth(data=.%>%filter(nsrm!="no SRM"),
              aes(x=meanlat,
                  y=opttemp-preind,
                  weight=pop, 
                  color=pimp),
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
                     labels=c("x0.2","x0.5","x1","x2")) +
  xlab("") + ylab("Local temperature increase to preindustrial [°C]") + 
  theme_pubr() + theme(legend.position = "bottom",
                       text=element_text(size=12))

precip2100 <- PREC %>% rename(prec=value) %>% 
  inner_join(sens_prec_coop) %>%
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
                 color=pimp),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=(prec-1)/sd,
                  color=pimp,
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
                     labels=c("x0.2","x0.5","x1","x2")) +
  xlab("") + ylab("Precipitation variation [STD]") + 
  theme_pubr() + theme(legend.position = "bottom",
                       text=element_text(size=12))

damages2100 <- gdploss %>%  
  filter(ttoyear(t)==2100 ) %>% 
  inner_join(pop %>% rename(pop=value) ) %>%
  inner_join(sens_prec_coop) %>%
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
                  color=pimp),
              alpha=0.2) + 
  stat_smooth(data=.%>% filter(meanlat<=60),
              aes(x=meanlat,
                  y=value*100,
                  color=pimp,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  scale_color_manual(values=coop_palette,
                     name="Temperature spread",
                     labels=c("x0.2","x0.5","x1","x2")) +
  theme_pubr() +   
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("GDP loss [%]") + theme(legend.position = "bottom",
                               text=element_text(size=12))

void <- ggplot() + theme_void() + theme(panel.background = element_rect(fill="white",color="white"))
fig2_coops <- ggarrange(ggarrange(regtemp2100,precip2100,nrow=1,common.legend=TRUE),
                        ggarrange(void,damages2100+theme(legend.position = "none"),void,nrow=1,widths=c(0.4,1,0.1)),
                        nrow=2,heights=c(1,1))
ggsave("SI_impmod_coop.png",plot=fig2_coops,width=18, height=16, units="cm")


scoop <- Z_SRM %>% 
  inner_join(sens_prec_coop) %>%
  filter(ttoyear(t)<=2100  & !is.na(value) & !inj %in% c("60N","60S") ) %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),
                y=value,
                fill=ordered(inj,c("45S","30S","15S","0","15N","30N","45N"))),
            linewidth=1,
            color="black") +
  xlab("") + ylab("SAI [TgS/yr]") + 
  theme_pubr() + 
  facet_wrap(pimp~.) +
  scale_fill_manual(name="Injection latitude",
                    values=c("darkblue","#4a8dff","#CDDDFF","grey","#ffbaba","#ff5252","#a70000"))
ggsave("SI_impmod_strategycoop.png",plot=scoop,width=9, height=5, units="cm")

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
  stat_smooth(aes(x=meanlat,
                  y=temp,
                  color=pimp,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  stat_smooth(data=.%>%filter(nsrm!="no SRM"),
              aes(x=meanlat,
                  y=opttemp-preind,
                  weight=pop, 
                  color=pimp),
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
                     labels=c("x0.2","x0.5","x1","x2")) +
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
                 color=pimp),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=(prec-1)/sd,
                  color=pimp,
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
                     labels=c("x0.2","x0.5","x1","x2")) +
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
                  color=pimp),
              alpha=0.2) + 
  stat_smooth(data=.%>% filter(meanlat<=60),
              aes(x=meanlat,
                  y=value*100,
                  color=pimp,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  scale_color_manual(values=coop_palette,
                     name="Temperature spread",
                     labels=c("x0.2","x0.5","x1","x2")) +
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
ggsave("SI_impmod_noncoop.png",plot=fig2_noncoops,width=18, height=16, units="cm")

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
  facet_grid(pimp~nsrm) +
  scale_fill_manual(name="Injection latitude",
                    values=c("darkblue","#4a8dff","#CDDDFF","grey","#ffbaba","#ff5252","#a70000"))
ggsave("SI_impmod_strategynoncoop.png",plot=snoncoop,width=9, height=5, units="cm")
