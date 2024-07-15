
W_EMI <- get_witch("W_EMI")
W_EMI %>% 
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)<=2250 & ghg=="co2" & nsrm %in% c("no SRM","Cooperative") & pimp %in% c(5)) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=nsrm),
            alpha=0.3) +
  scale_color_viridis_d(name="Precipitation impacts") +
  xlab("") + ylab("SAI [TgS/yr]") 

W_SRM %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & pimp %in% c(5) & nsrm != "no SRM") %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=nsrm),
            linewidth=1) +
  ggrepel::geom_text_repel(data=.%>% inner_join(Z_SRM %>% 
    inner_join(sanitized_names) %>%
    filter(ttoyear(t)==2100 & 
           pimp %in% c(5) & 
           !nsrm %in% c("no SRM","Cooperative") & value!=0) %>%
    rename(zonalinj=value) ),
  aes(x=2100,
      y= value,
      label=paste0(round(zonalinj,0),"tgS/yr at ",inj), 
      color=nsrm ) ) +
  scale_color_manual(values=regpalette_srm, name="SAI deployer") +
  xlab("") + ylab("SAI [TgS/yr]") + theme_pubr()

  

  

land_temp %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & pimp==1) %>%
  ggplot(aes(x=ttoyear(t),y=value-land_temp0,color=nsrm,linetype=pimp,group=file)) +
  geom_line() +
  geom_line(data=land_temp_nogeong %>%
              inner_join(main_scenarios_coop) %>%
              filter(ttoyear(t)<=2100 & pimp==1),
            aes(x=ttoyear(t),y=value-land_temp0,color=nsrm,linetype=pimp,group=file)) +
  scale_color_manual(values=regpalette_srm)


