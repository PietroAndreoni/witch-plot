
W_EMI <- get_witch("W_EMI")
wemi <- W_EMI %>% 
  inner_join(sanitized_names) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm) ) %>%
  filter(ttoyear(t)<=2100 & ghg=="co2" & pimp %in% c(5)) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value*3.66,
                color=Scenario),
            linewidth=1) +
  geom_hline(yintercept=0) +
  scale_color_manual(values=regpalette_srm,name="Scenario") +
  xlab("") + ylab("Global emissions [GtCO2/yr]")
ggsave("fig_wemi.png",plot=wemi,width=8.8, height=7, units="cm")
