main_scenarios_coop <- sanitized_names %>% 
  filter(COOP=="coop")

coop_palette <- c("Mitigation + SAI.bhmbest"="#121B54",
                  "Mitigation + SAI.bhmspecbest"="#0069a3",
                  "Mitigation + SAI.specbest"="#00babc",
                  "Mitigation.bhmspecbest"="#00A36C")


inj <- Z_SAI %>% 
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)<=2100 & !is.na(value) & Scenario!="Mitigation") %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),
                y=value,
                fill=ordered(inj,c("60S","45S","30S","15S","0","15N","30N","45N","60N"))),
            linewidth=1,
            color="black") +
  xlab("") + ylab("SAI [TgS/yr]") + 
  theme_pubr() + 
  facet_wrap(impacts~.,nrow=1) +
  scale_fill_viridis_d(name="Injection latitude") +
  # scale_fill_manual(name="Injection latitude",
  #                   values=c("","darkblue","#4a8dff","#CDDDFF","grey","#ffbaba","#ff5252","#a70000"))+
  theme(text=element_text(size=7))


globtemp <- TATM %>%
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)<=2100 & !(Scenario=="Mitigation" & impacts %in% c("bhmbest","specbest"))) %>%
  ggplot(aes(x=ttoyear(t),
             y=value,
             color=interaction(Scenario,impacts),
             group=file)) +
  geom_line(linewidth=1) +
  geom_line(data=TATM_GHG %>%
              inner_join(main_scenarios_coop) %>%
              filter(ttoyear(t)<=2100 & nsrm!="no SRM"),
            aes(x=ttoyear(t),y=value,color=interaction(Scenario,impacts)), 
            linetype=2,
            linewidth=1) +
  theme_pubr() + 
  ylab("Global temperature increase [°C]") +
  xlab("")+
  scale_color_manual(values=coop_palette,
                     labels=c("Mitigation + SAI.bhmbest"="BHM",
                              "Mitigation + SAI.bhmspecbest"="BHM+SPEC",
                              "Mitigation + SAI.specbest"="SPEC",
                              "Mitigation.bhmspecbest"="Mitigation"),
                     name="Scenario") + 
  theme(legend.position = "top",
        text=element_text(size=7))

regtemp2100 <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(base_temp) %>%
  group_by(file,n) %>%
  mutate(temp=temp-temp0) %>%
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)==2100 & !(Scenario=="Mitigation" & impacts %in% c("bhmbest","specbest")) ) %>% 
  inner_join(countries_map) %>% 
  inner_join(optimal_temp) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  ggplot() +
  geom_point(aes(x=meanlat,
        y=temp,
        color=interaction(Scenario,impacts), 
        shape=impacts),
    alpha=0.2) + 
  # geom_point(data=.%>%filter(nsrm!="no SRM"),
  #   aes(x=meanlat,
  #       y=opttemp-temp0),
  #   color="black",
  #   alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
        y=temp,
        color=interaction(Scenario,impacts),
        weight=pop), 
    se = FALSE,
    method = "loess",
    linewidth=2) +
  # stat_smooth(data=.%>%filter(nsrm!="no SRM"),
  #   aes(x=meanlat,
  #       y=opttemp-temp0,
  #       weight=pop),
  #   color="black",
  #   se = FALSE,
  #   linewidth=2,
  #   linetype=2) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=coop_palette) +
  xlab("") + ylab("Local temperature increase [°C]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=7))

precip2100 <- PREC %>% rename(prec=value) %>% 
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)==2100 & !(Scenario=="Mitigation" & impacts %in% c("bhmbest","specbest"))) %>%   
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
        color=interaction(Scenario,impacts),
        shape=impacts),
    alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
        y=(prec-1)/sd,
        color=interaction(Scenario,impacts),
        weight=pop), 
    se = FALSE,
    linewidth=2 ) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=coop_palette) +
  xlab("") + ylab("Precipitation variation [STD]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=7))


gdploss_maps <- gdploss %>% 
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)==2100) %>%
  ungroup() %>% mutate(value=ifelse(n=="row",NA,value)) %>%
  group_by(n,impacts) %>%
  summarise(value=value[Scenario=="Mitigation + SAI"]-value[Scenario=="Mitigation"]) %>%
  full_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = -value*100),size=.1,color="black") +
  geom_point(data=Z_SAI %>% 
               inner_join(main_scenarios_coop) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="red", fill=NA ) +
  scale_fill_gradient2(name="% GDP variation") +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),
        legend.position = "top") +
  facet_wrap(impacts~.,nrow=1)

void <- ggplot() + theme_void() + theme(panel.background = element_rect(fill="white",color="white"))
fig2_coops <- ggarrange(inj,
                        ggarrange(globtemp,ggarrange(regtemp2100,precip2100,nrow=2, labels=c("c","d")), nrow=1, labels=c("b",""), common.legend = TRUE),
                        gdploss_maps,
                        nrow=3,heights=c(1.3,1.8,1.4), labels=c("a","","f"))
ggsave("fig_coop.png",plot=fig2_coops,width=18, height=24, units="cm")

