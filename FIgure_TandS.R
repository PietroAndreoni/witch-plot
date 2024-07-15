TATM <- get_witch("TATM")
land_temp_nogeong <- TATM %>% 
  select(-n) %>%
  inner_join(coef %>% 
               filter(V1 %in% c("alpha_temp","beta_temp")) %>% 
               pivot_wider(names_from=V1)) %>%
  inner_join(area) %>%
  group_by(file,t) %>%
  summarise(value=weighted.mean(alpha_temp+beta_temp*value,area))

figa <- Z_SRM %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & nsrm %in% c("Cooperative") & pimp %in% c(1,5) & !is.na(value)) %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),
                y=value,
                fill=ordered(inj,c("45S","30S","15S","0","15N","30N","45N"))),
            linewidth=1,
            color="black") +
  xlab("") + ylab("SAI [TgS/yr]") + 
  theme_pubr() + 
  facet_wrap(pimp~.,nrow=2) +
  scale_fill_manual(name="Injection latitude",
                    values=c("darkblue","#4a8dff","#CDDDFF","#FFFFFF","#ffbaba","#ff5252","#a70000"))

figb <- land_temp %>%
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)<=2100) %>%
  ggplot(aes(x=ttoyear(t),
             y=value-land_temp0,
             color=nsrm,
             group=file)) +
  geom_line(linewidth=1) +
  geom_line(data=land_temp_nogeong %>%
              inner_join(main_scenarios_coop) %>%
              filter(ttoyear(t)<=2100 & nsrm!="no SRM"),
            aes(x=ttoyear(t),y=value-land_temp0,color=nsrm), 
            linetype=2,
            linewidth=1) +
  theme_pubr() + 
  ylab("Temperature increase [°C]") +
  xlab("")+
  scale_color_manual(values=c("#72bcd4","#121B54","#00A36C"),
                     labels=c("SAI, low prec","SAI, high prec","2°C"),
                     name="Scenario") 

fig <- ggarrange(figa,figb)
ggsave("fig_TS.png",plot=fig,width=18, height=9, units="cm")
