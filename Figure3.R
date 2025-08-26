main_scenarios_coop <- sanitized_names %>% 
  filter(COOP=="coop" & impacts!="bhm")

coop_palette <- c("Mitigation + SAI"="#121B54",
                  "Mitigation"="#00A36C")

globtemp <- TATM %>%
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)<=2100 & !(Scenario=="Mitigation" & impacts %in% c("bhmbest","specbest"))) %>%
  ggplot(aes(x=ttoyear(t),
             y=value,
             color=Scenario,
             group=file)) +
  geom_line(linewidth=1) +
  geom_line(data=TATM_GHG %>%
              inner_join(main_scenarios_coop) %>%
              filter(ttoyear(t)<=2100 & nsrm!="no SRM"),
            aes(x=ttoyear(t),
                y=value,
                color=Scenario), 
            linetype=2,
            linewidth=1) +
  theme_pubr() + 
  ylab("Global temperature increase [°C]") +
  xlab("")+
  scale_color_manual(values=coop_palette,
                     name="Scenario") + 
  theme(legend.position = "top",
        text=element_text(size=7)) 

regtemp2100 <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(base_temp) %>%
  group_by(file,n) %>%
  mutate(temp=temp-temp0) %>%
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)==2100 & !(Scenario=="Mitigation" & impacts %in% c("BHM","SPEC")) & n!="row" ) %>% 
  inner_join(countries_map) %>% 
  inner_join(optimal_temp) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) %>% unique()) %>%
  ggplot() +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point(aes(x=meanlat,
                 y=temp,
                 color=Scenario),
             alpha=0.2) + 
  stat_smooth(data=.%>%filter(nsrm!="no SRM" & impacts=="MAIN"),
              aes(x=meanlat,
                  y=opttemp-temp0,
                  weight=pop),
              color="grey50",
              se = FALSE,
              method = "loess",
              linewidth=1,
              linetype=2) +
  stat_smooth(data=.%>%filter(nsrm!="no SRM" & impacts=="SPEC"),
              aes(x=meanlat,
                  y=opttemp-temp0,
                  weight=pop),
              color="grey50",
              se = FALSE,
              method = "loess",
              linewidth=1,
              linetype=2) +
  stat_smooth(aes(x=meanlat,
                  y=temp,
                  color=Scenario,
                  weight=pop), 
              se = FALSE,
              method = "loess",
              linewidth=2) +
  # geom_point(data=.%>%filter(nsrm!="no SRM" & impacts=="bhmspec"),
  #            aes(x=meanlat,
  #                y=opttemp-temp0),
  #            color="grey80",
  #            alpha=0.1) +
  theme(legend.position="bottom") +
  scale_color_manual(values=coop_palette) +
  scale_fill_manual(values=coop_palette) +
  xlab("") + ylab("Local temperature variation rtm 1980-2018 [°C]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=7)) + coord_flip()

precip2100 <- PREC %>% rename(prec=value) %>% 
  inner_join(main_scenarios_coop) %>%
  inner_join(base_prec) %>%
  filter(ttoyear(t)==2100 & !(Scenario=="Mitigation" & impacts %in% c("BHM","SPEC"))) %>%   
  inner_join(countries_map) %>% 
  inner_join(optimal_prec) %>%
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
              linewidth=0.5,
              alpha=0.1) +
  geom_point(aes(x=meanlat,
                 y=(prec-1)/sd,
                 color=Scenario),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=(prec-1)/sd,
                  color=Scenario,
                  weight=pop), 
              se = FALSE,
              linewidth=2 ) +
  # geom_point(data=.%>%filter(nsrm!="no SRM" & impacts=="bhmspec"),
  #            aes(x=meanlat,
  #                y=(prec-1)/sd),
  #            color="black",
  #            alpha=0.2) +
  stat_smooth(data=.%>%filter(nsrm!="no SRM" & impacts=="MAIN"),
              aes(x=meanlat,
                  y=(optprec-prec0)/sd,
                  weight=pop),
              color="grey50",
              se = FALSE,
              method = "loess",
              linewidth=1,
              linetype=2) +
  stat_smooth(data=.%>%filter(nsrm!="no SRM" & impacts=="SPEC"),
              aes(x=meanlat,
                  y=(optprec-prec0)/sd,
                  weight=pop),
              color="grey50",
              se = FALSE,
              method = "loess",
              linewidth=1,
              linetype=2) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="none") +
  scale_color_manual(values=coop_palette) +
  xlab("") + ylab("Precipitation variation rtm 1980-2018 [STD]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=7))+ coord_flip()

gdploss2100 <- gdploss %>% 
  inner_join(main_scenarios_coop) %>% 
  filter(ttoyear(t)==2100 & !(Scenario=="Mitigation" & impacts %in% c("BHM")) & n!="row" ) %>% 
  inner_join(countries_map) %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) %>% unique()) %>%
  ggplot() +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point(aes(x=meanlat,
                 y=value,
                 color=Scenario),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=value,
                  color=Scenario,
                  weight=pop), 
              se = FALSE,
              method = "loess",
              linewidth=2) +
  theme(legend.position="bottom") +
  scale_color_manual(values=coop_palette) +
  scale_fill_manual(values=coop_palette) +
  xlab("") + ylab("GDP loss [%]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=7)) + coord_flip()

gdploss_maps <- gdploss %>% 
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)==2100) %>%
  inner_join(gdploss_g %>% select(file,t,value) %>% rename(gloss=value)) %>%
  ungroup() %>% mutate(value=ifelse(n=="row",NA,value)) %>%
  group_by(n,impacts) %>%
  summarise(value=(value[Scenario=="Mitigation + SAI"]-value[Scenario=="Mitigation"]) ) %>%
  full_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = -value*100),size=.05,color="grey50") +
  geom_point(data=Z_SAI %>% 
               inner_join(main_scenarios_coop) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="black", fill=NA ) +
  geom_text(data= Z_SAI %>% 
              inner_join(main_scenarios_coop) %>%               
              mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
              filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding") & round(value,0) != 0),
            aes(x=-170, y = ninj-5, label= paste0(round(value,0)," TgS/yr")), color="black", size=3 ) +
  scale_fill_gradient2() +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),
        legend.position = "top") +
  scale_fill_gradient2(low="#780000",high="#003049",name="% GDP variation") 
  
#  facet_wrap(impacts~.,nrow=1)

void <- ggplot() + theme_void() + theme(panel.background = element_rect(fill="white",color="white"))
fig2_coops <- ggarrange(ggarrange(void,globtemp,void,nrow=1, labels=c("","a",""), widths=c(0.3,1,0.3)),
                        ggarrange(regtemp2100,precip2100,nrow=1, labels=c("b","c")),
                        ggarrange(void,gdploss_maps,void,nrow=1, labels=c("","d",""), widths=c(0.3,1,0.3)),
                        nrow=3,heights=c(1.2,1,1.2), labels=c("","",""))
ggsave("fig_coop.png",plot=fig2_coops,width=18, height=24, units="cm")
