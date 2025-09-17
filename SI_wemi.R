gwp <- c("ch4"=25*1e-3,"n2o"=298*1e-3,"co2"=1)
W_EMI <- get_witch("W_EMI")
wemi <- W_EMI %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & impacts==imp_select) %>%
#  group_by(file,Scenario,t,impacts) %>%
#  summarise(value=sum(value*gwp[ghg])) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=Scenario),
            linewidth=1) +
  ggrepel::geom_text_repel(data=.%>% 
                             inner_join(abatefrac_g %>% select(-value) %>% 
                                          filter(ttoyear(t)==2100) %>% mutate(ghg="co2")),
            aes(x=2105,
                y=value,
                color=Scenario, 
                label=paste0(round(valuefrac*100,1), " %")) ) +
  geom_hline(yintercept=0) +
  facet_wrap(ghg~.,scales="free") +
  scale_color_manual(values=regpalette_srm,name="Scenario") +
  xlab("") + ylab("Global GHGs emissions [Gt-Mt GHG/yr]")

CONC <- get_witch("CONC")
conc <- CONC %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & impacts==imp_select) %>%
  #  group_by(file,Scenario,t,impacts) %>%
  #  summarise(value=sum(value*gwp[ghg])) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=Scenario),
            linewidth=1) +
  ggrepel::geom_text_repel(data=.%>% inner_join(abatefrac_g %>% rename(abfrac=valuefrac) %>% filter(ttoyear(t)==2100)),
                           aes(x=2105,
                               y=value,
                               color=Scenario, 
                               label=paste0(round(abfrac*100,1), " %")) ) +
  geom_hline(yintercept=0) +
  facet_wrap(ghg~.,scales="free") +
  scale_color_manual(values=regpalette_srm,name="Scenario") +
  theme(legend.position = "none") +
  xlab("") + ylab("Global GHGs concentrations [ppm/ppb]")
require(patchwork)
ggsave("fig_wemi.png",plot=wemi/conc,width=18, height=14, units="cm",dpi=400)

abatefrac_g %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & impacts==imp_select) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=valuefrac*100,
                color=Scenario, 
                linetype=impacts),
            linewidth=1) +
  geom_hline(yintercept=0) +
  scale_color_manual(values=regpalette_srm,name="Scenario") +
  xlab("") + ylab("Total abatement costs [% GDP/yr]")


fig_tatm <- TATM %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & impacts==imp_select ) %>%
  ggplot(aes(x=ttoyear(t),
             y=value,
             color=Scenario)) +
  geom_line(linewidth=1) +
  geom_line(data=TATM_GHG %>%
              inner_join(sanitized_names) %>%
              filter(ttoyear(t)<=2100 & nsrm!="no SRM"& impacts==imp_select & pers_p=="inf" & pers_t=="inf" ),
            aes(x=ttoyear(t),y=value,color=Scenario), 
            linetype=2,
            linewidth=1) +
#s  facet_wrap(.~impacts) +
  theme_pubr() + 
  ylab("") +
  xlab("Global mean temperature increase relative to preindustrial [°C]")+
  scale_color_manual(values=regpalette_srm,
                     name="Scenario") 
ggsave("fig_tatm.png",plot=fig_tatm,width=14, height=12, units="cm",dpi=400)
