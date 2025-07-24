gwp <- c("ch4"=25*1e-3,"n2o"=298*1e-3,"co2"=1)
W_EMI <- get_witch("W_EMI")
wemi <- W_EMI %>% 
  inner_join(sanitized_names) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Mitigation",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Mitigation + SAI",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm) ) %>%
  filter(ttoyear(t)<=2150) %>%
  group_by(file,Scenario,t) %>%
  summarise(value=sum(value*gwp[ghg])) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=Scenario),
            linewidth=1) +
  geom_hline(yintercept=0) +
  scale_color_manual(values=regpalette_srm,name="Scenario") +
  xlab("") + ylab("Global GHGs emissions [GtCO2eq/yr]")
ggsave("fig_wemi.png",plot=wemi,width=8.8, height=7, units="cm")
