main_scenarios_coop <- sanitized_names %>% 
  filter(COOP=="coop")

figa <- Z_SAI %>% 
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)<=2100 & !is.na(value) & Scenario!="Mitigation" & !inj %in% c("60N","60S")) %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),
                y=value,
                fill=ordered(inj,c("45S","30S","15S","0","15N","30N","45N"))),
            linewidth=1,
            color="black") +
  xlab("") + ylab("SAI [TgS/yr]") + 
  theme_pubr() + 
  #facet_wrap(pimp~.,nrow=2) +
  scale_fill_manual(name="Injection latitude",
                    values=c("darkblue","#4a8dff","#CDDDFF","grey","#ffbaba","#ff5252","#a70000"))

figb <- TATM %>%
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)<=2100) %>%
  ggplot(aes(x=ttoyear(t),
             y=value,
             color=nsrm,
             group=file)) +
  geom_line(linewidth=1) +
  geom_line(data=TATM_GHG %>%
              inner_join(main_scenarios_coop) %>%
              filter(ttoyear(t)<=2100 & nsrm!="no SRM"),
            aes(x=ttoyear(t),y=value,color=nsrm), 
            linetype=2,
            linewidth=1) +
  theme_pubr() + 
  ylab("Average land temperature increase [°C]") +
  xlab("")+
  scale_color_manual(values=c("#121B54","#00A36C"),
                     labels=c("Mitigation + SAI","Mitigation"),
                     name="Scenario") 

fig <- ggarrange(figa,figb)
ggsave("fig_TS.png",plot=fig,width=18, height=9, units="cm")
