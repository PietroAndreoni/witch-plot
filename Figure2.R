###### 
emulator_data <- gdx('../data_maxiso3sai/data_mod_sai.gdx')
temp  <- emulator_data["srm_temperature_response_area"] %>% 
  mutate(w="area") %>%
  bind_rows(emulator_data["srm_temperature_response_area"] %>%
              as_tibble() %>% 
              mutate(w="area")) %>%
  rename(inj=V2,se=V3) %>% 
  mutate(inj_lat=ifelse(str_detect(inj,"S"),as.numeric(paste0("-",str_remove(inj,"S"))),as.numeric(str_remove(inj,"N")))  ) %>%
  inner_join(countries_map)
prec  <- emulator_data["srm_precip_response_area"] %>% 
  mutate(w="area") %>%
  bind_rows(emulator_data["srm_precip_response_area"] %>%
              mutate(w="area")) %>%
  as_tibble() %>% 
  rename(inj=V2,se=V3) %>% 
  mutate(inj_lat=ifelse(str_detect(inj,"S"),as.numeric(paste0("-",str_remove(inj,"S"))),as.numeric(str_remove(inj,"N")))  ) %>%
  inner_join(countries_map)

temp2 <- get_witch("sai_temp") %>%
  mutate(inj_lat=ifelse(str_detect(inj,"S"),as.numeric(paste0("-",str_remove(inj,"S"))),as.numeric(str_remove(inj,"N")))  ) %>%
  inner_join(countries_map)
prec2 <- get_witch("sai_precip") %>%
  mutate(inj_lat=ifelse(str_detect(inj,"S"),as.numeric(paste0("-",str_remove(inj,"S"))),as.numeric(str_remove(inj,"N")))  ) %>%
  inner_join(countries_map)
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
               filter(se=="obs" & w=="area"),
             aes(x=meanlat,
                 y=value,
                 color=as.factor(inj_lat)),
             alpha=0.2) +  
  geom_hline(yintercept=0,color="grey80",linewidth=1) +
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
              method = "loess",
              linewidth=2)+
  geom_smooth(data=unique(inner_join(reaction_t_srm %>% filter(inj_lat==0),temp %>% filter(se=="obs" & w=="area") %>% select(n,meanlat))) %>%
                inner_join(pop2020),  
              aes(x=as.numeric(meanlat), 
                  y=t_eq,
                  weight=pop2), 
              color="black", 
              se = F,  
              method = "loess",
              linetype="dotted", 
              linewidth=2 ) +
  scale_color_manual(values=c("-30"="#440154FF","0"="#21908CFF","30"="#FDE725FF"),name = "Injection latitude") + 
  xlab("Average country latitude") + ylab("Cooling [°C]")+
  coord_flip()

########################## precipitation response
reaction_p_srm <- cross_join(clim %>% 
                               filter(V1 %in% c("alpha_precip","beta_precip")) %>% 
                               pivot_wider(names_from=V1,values_from=value),
                             tglobal) %>%
  mutate(p_eq = - beta_precip * tg / alpha_precip)

f1b <- ggplot(prec %>% inner_join(sd_prec) %>%
                filter(inj_lat %in% c(-30,0,30)) %>% 
                inner_join(unique(inner_join(reaction_p_srm,prec %>% select(n,meanlat)))) ) +
  geom_hline(yintercept=0,color="grey80",linewidth=1) +
  geom_ribbon(aes(x=meanlat,
                  ymin=-1,
                  ymax=1),
              color="grey80",
              linewidth=0.5,
              alpha=0.2) +
  geom_point(data=unique(inner_join(reaction_p_srm %>% filter(inj_lat==0),prec %>% select(n,meanlat))) %>% inner_join(sd_prec),  
             aes(x=as.numeric(meanlat), 
                 y=p_eq/sd ), 
             color="black", 
             alpha=0.1) +
  geom_point(data=.%>%filter(se=="obs" & w=="area"),aes(x=meanlat,
                                                        y=value/sd,
                                                        color=ordered(inj_lat,c(-60,-45,-30,-15,0,15,30,45,60))),alpha=0.2) +
  geom_smooth(data=unique(inner_join(reaction_p_srm %>% filter(inj_lat==0),prec %>% filter(se=="obs" & w=="area") %>% select(n,meanlat))) %>% 
                inner_join(sd_prec)%>%
                inner_join(pop2020),  
              aes(x=as.numeric(meanlat), 
                  y=p_eq/sd ), 
              color="black", 
              se = F, 
              method = "loess",
              linetype="dotted", 
              linewidth=2) +
  geom_smooth(data=.%>%
                inner_join(pop2020),
              aes(x=meanlat, 
                  y=value/sd,
                  color=ordered(inj_lat,c(-60,-45,-30,-15,0,15,30,45,60))),
              se = F,
              method = "loess",
              linewidth=2) +
  #  ylim(c(-3.51,+3.51)) +
  scale_color_manual(values=c("-30"="#440154FF","0"="#21908CFF","30"="#FDE725FF"),name = "Injection latitude") + 
  xlab("") + ylab("Precipitation variation [SD]") +
  coord_flip()

ci_sel<-"best"
a <- optimal_temp %>% 
  cross_join(data.frame(tvar=seq(-2,+2,0.1))) %>% 
  mutate(dg = TM * (tvar) + TM_2 * ((tvar+base_temp)^2-base_temp^2) + dev_TM_all_2 * (tvar/sd_temp)^2   ) %>% 
  inner_join(sanitized_names) %>% 
  select(n,impacts,ci_imp,base_temp,tvar,dg) %>% unique() %>% 
  filter(impacts==imp_select & ci_imp==ci_sel) %>% 
  inner_join(countries_map) %>% 
  group_by(latitude,impacts,ci_imp,tvar) %>% 
  summarise(med=median(dg), min = quantile(dg,0.05), max=quantile(dg,0.95)) %>% 
  ggplot() +
  geom_smooth(aes(x=tvar,
                y=med*100,
                color=latitude),
            linewidth=1) +
  geom_ribbon(aes(x=tvar,
                  ymin=min*100,
                  ymax=max*100,
                  fill=latitude),
              alpha=0.1) +
#   geom_vline(aes(xintercept=medopt,color=latitude)) +
#  facet_wrap(impacts~.,) + 
  scale_color_manual(values=c("Low latitudes"="#21908CFF",
                              "High latitudes"="#3B528BFF",
                              "Subtropical"="#FDE725FF",
                              "Mid latitudes"="#5DC863FF"),name = "Country latitude") + 
  scale_fill_manual(values=c("Low latitudes"="#21908CFF",
                             "High latitudes"="#3B528BFF",
                             "Subtropical"="#FDE725FF",
                             "Mid latitudes"="#5DC863FF"),name = "Country latitude") + 
#  guides(color=guide_legend(nrow = 2, byrow = TRUE)) +
  theme_pubr() + ylab("Growth variation [% GDP/yr]") + xlab("Local temperature variation [°C]") + 
  theme(text=element_text(size=7), legend.box = "vertical") + coord_cartesian(ylim=c(-4,1))


b <- optimal_prec %>% 
  cross_join(data.frame(tvar=seq(-3,+3,0.05))) %>% 
  mutate(dg = RR * (tvar*sd_prec*1e-3) + RR_2 * ( ((tvar*sd_prec+base_precip)*1e-3)^2-(base_precip*1e-3)^2) + dev_RR_all_2 * (tvar)^2   ) %>% 
  inner_join(sanitized_names) %>% select(n,impacts,ci_imp,base_temp,tvar,dg) %>% unique() %>% 
  inner_join(countries_map) %>% 
  filter(impacts==imp_select & ci_imp==ci_sel) %>% 
  group_by(latitude,impacts,ci_imp,tvar) %>% 
  summarise(med=median(dg), min = quantile(dg,0.05), max=quantile(dg,0.95)) %>% 
  ggplot() +
  geom_smooth(aes(x=tvar,
                y=med*100,
                color=latitude),
            linewidth=1) +
  geom_ribbon(aes(x=tvar,
                  ymin=min*100,
                  ymax=max*100,
                  fill=latitude),
              alpha=0.2) +
  #  geom_vline(aes(xintercept=medopt,color=latitude)) +
#  facet_wrap(impacts~.,) + 
  scale_color_manual(values=c("Low latitudes"="#21908CFF",
                              "High latitudes"="#3B528BFF",
                              "Subtropical"="#FDE725FF",
                              "Mid latitudes"="#5DC863FF"),name = "Country latitude") + 
  scale_fill_manual(values=c("Low latitudes"="#21908CFF",
                             "High latitudes"="#3B528BFF",
                             "Subtropical"="#FDE725FF",
                             "Mid latitudes"="#5DC863FF"),name = "Country latitude") + 
  theme_pubr() + ylab("") + xlab("Local precipitation variation [SD]") + 
#  guides(color=guide_legend(nrow = 2, byrow = TRUE)) +
  theme(text=element_text(size=7), legend.box = "vertical") + coord_cartesian(ylim=c(-4,1))

c <- gdploss %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & Scenario %in% c("Free-riding", "Mitigation") & impacts==imp_select & ci_imp==ci_sel) %>%
  inner_join(TATM %>% rename(tatm=value) %>% select(-n)) %>% 
  inner_join(gdploss_g %>% select(file,t,value) %>% rename(gloss=value)) %>%
  ungroup() %>% mutate(value=ifelse(n=="row",NA,value)) %>%
  full_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = -value*100),size=.05,color="grey50") +
  geom_text(aes(x = 0, y = -60, label=paste0("GMT INCREASE ", round(tatm,1), "°C, GDP LOSS ", round(gloss*100,0), "% by 2100")),size=2) +
  scale_fill_gradient2(low="#780000",high="#003049",name="% GDP variation") +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),
        legend.position = "top") +
  facet_grid(.~Scenario)

fig_impacts <- ggarrange(ggarrange(f1a,f1b,nrow=1, labels=c("a",""),common.legend = TRUE),
                         ggarrange(a,b,nrow=1, labels=c("b",""),common.legend = TRUE),
                         c,nrow=3,heights=c(1,1,1), labels=c("","","c"))
ggsave("fig_impacts_cons.png",plot=fig_impacts,width=18, height=18*1.2, units="cm")
