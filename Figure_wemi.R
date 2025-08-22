gwp <- c("ch4"=25*1e-3,"n2o"=298*1e-3,"co2"=1)
W_EMI <- get_witch("W_EMI")
wemi <- W_EMI %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100) %>%
  group_by(file,Scenario,t,impacts) %>%
  summarise(value=sum(value*gwp[ghg])) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=Scenario, 
                linetype=impacts),
            linewidth=1) +
  ggrepel::geom_text_repel(data=.%>% inner_join(abatefrac_g %>% rename(abfrac=value) %>% filter(ttoyear(t)==2100)),
            aes(x=2105,
                y=value,
                color=Scenario, 
                label=paste0(round(abfrac*100,1), " %")) ) +
  geom_hline(yintercept=0) +
  scale_color_manual(values=regpalette_srm,name="Scenario") +
  xlab("") + ylab("Global GHGs emissions [GtCO2eq/yr]")
ggsave("fig_wemi.png",plot=wemi,width=8.8, height=7, units="cm")

abatefrac_g %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=Scenario, 
                linetype=impacts),
            linewidth=1) +
  geom_hline(yintercept=0) +
  scale_color_manual(values=regpalette_srm,name="Scenario") +
  xlab("") + ylab("Total abatement costs [% GDP/yr]")


figb <- TATM %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100) %>%
  ggplot(aes(x=ttoyear(t),
             y=value,
             color=Scenario)) +
  geom_line(linewidth=1) +
  geom_line(data=TATM_GHG %>%
              inner_join(sanitized_names) %>%
              filter(ttoyear(t)<=2100 & nsrm!="no SRM"),
            aes(x=ttoyear(t),y=value,color=Scenario), 
            linetype=2,
            linewidth=1) +
  facet_wrap(.~impacts) +
  theme_pubr() + 
  ylab("Average land temperature increase [°C]") +
  xlab("")+
  scale_color_manual(values=regpalette_srm,
                     name="Scenario") 

Z_SAI %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & !is.na(value) & Scenario!="Mitigation" & !inj %in% c("60N","60S")) %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),
                y=value,
                fill=ordered(inj,c("45S","30S","15S","0","15N","30N","45N"))),
            linewidth=1,
            color="black") +
  xlab("") + ylab("SAI [TgS/yr]") + 
  theme_pubr() + 
  facet_grid(Scenario~impacts) +
  scale_fill_manual(name="Injection latitude",
                    values=c("darkblue","#4a8dff","#CDDDFF","grey","#ffbaba","#ff5252","#a70000"))
