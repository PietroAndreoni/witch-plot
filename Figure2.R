a <- optimal_temp %>% 
  cross_join(data.frame(tvar=seq(-2,+2,0.1))) %>% 
  mutate(dg = TM * (tvar) + TM_2 * ((tvar+base_temp)^2-base_temp^2) + dev_TM_all_2 * (tvar/sd_temp)^2   ) %>% 
  inner_join(sanitized_names) %>% select(n,impacts,ci_imp,base_temp,tvar,dg) %>% unique() %>% 
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
  theme_pubr() + ylab("% loss GDP/yr") + xlab("Local temperature variation rtm [°C]") + 
#  guides(color=guide_legend(nrow = 2, byrow = TRUE)) +
  theme(text=element_text(size=12), legend.box = "vertical") + coord_cartesian(ylim=c(-4,1))


b <- optimal_prec %>% 
  cross_join(data.frame(tvar=seq(-3,+3,0.05))) %>% 
  mutate(dg = RR * (tvar*sd_prec*1e-3) + RR_2 * ( ((tvar*sd_prec+base_precip)*1e-3)^2-(base_precip*1e-3)^2) + dev_RR_all_2 * (tvar)^2   ) %>% 
  inner_join(sanitized_names) %>% select(n,impacts,ci_imp,base_temp,tvar,dg) %>% unique() %>% 
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
              alpha=0.2) +
  #  geom_vline(aes(xintercept=medopt,color=latitude)) +
#  facet_wrap(impacts~.,) + 
  theme_pubr() + ylab("") + xlab("Local precipitation variation [SD]") + 
#  guides(color=guide_legend(nrow = 2, byrow = TRUE)) +
  theme(text=element_text(size=12), legend.box = "vertical") + coord_cartesian(ylim=c(-4,1))

c <- gdploss %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & Scenario %in% c("Free-riding", "Mitigation") ) %>%
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

fig_impacts <- ggarrange(ggarrange(a,b,nrow=1, labels=c("a",""),common.legend = TRUE),
                         c,nrow=2,heights=c(1,1), labels=c("","b"))
ggsave("fig_impacts.png",plot=fig_impacts,width=18, height=16, units="cm")
