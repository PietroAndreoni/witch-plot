precipitation_sel <- 0.2
tspread_sel <- 1

figa <- W_SRM %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & pimp==precipitation_sel & spread==tspread_sel & nsrm != "no SRM") %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=Scenario),
            linewidth=1) +
  ggrepel::geom_text_repel(data=.%>% inner_join(Z_SRM %>% 
    inner_join(sanitized_names) %>%
    filter(ttoyear(t)==2100 & 
           pimp %in% c(1) & 
           !nsrm %in% c("no SRM","Cooperative") & value!=0) %>%
    rename(zonalinj=value) ),
  aes(x=2100,
      y= value,
      label=paste0(round(zonalinj,0),"tgS/yr at ",inj), 
      color=nsrm ) ) +
  scale_color_manual(values=regpalette_srm, name="Scenario") +
  guides(text="none") +
  xlab("") + ylab("SAI [TgS/yr]") + theme_pubr()

  

figb <- land_temp %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & pimp==precipitation_sel & spread==tspread_sel) %>%
  ggplot(aes(x=ttoyear(t),y=value-land_temp0,color=Scenario,group=file)) +
  geom_line(linewidth=1) +
  geom_line(data=land_temp_nogeong %>%
              inner_join(sanitized_names) %>%
              mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                                        nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                                        nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                                        .default=nsrm) ) %>%
              filter(ttoyear(t)<=2100 & pimp==1 & nsrm!="no SRM" & COOP=="noncoop"),
            aes(x=ttoyear(t),y=value-land_temp0,color=Scenario,group=file), linetype=2, linewidth=1) +
  scale_color_manual(values=regpalette_srm,name="Scenario") +
  xlab("") + ylab("Average land temperature increase [°C]")

fig <- ggarrange(figa+theme(legend.position="none"),figb)
ggsave("fig_TSnoncoop.png",plot=fig,width=18, height=9, units="cm")

