regtemp2100 <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(file,n) %>%
  mutate(temp=temp-preind) %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100) %>% 
  inner_join(N_SRM %>% rename(srm=value))  %>% 
  inner_join(countries_map) %>% 
  inner_join(optimal_temperature) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  filter(pimp %in% c("1") & 
           nsrm %in% c("no SRM","Cooperative","India","Brazil","China","USA") ) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  ggplot() +
  geom_point(
    #    data=.%>%filter(nsrm!="no SRM"),
    aes(x=meanlat,
        y=temp,
        color=Scenario),
    alpha=0.2) + 
  stat_smooth(
    #    data=.%>%filter(nsrm!="no SRM"),
    aes(x=meanlat,
        y=temp,
        color=Scenario,
        weight=pop), 
    se = FALSE,
    linewidth=2) +
geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=regpalette_scenarios,
                     name="Precipitation impacts") +
  xlab("") + ylab("Distance from local optimal temperature [°C]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=7))

precip2100 <- PREC %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100) %>%   
  inner_join(countries_map) %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sd_prec) %>%
  filter(pimp %in% c("1") & 
           nsrm %in% c("no SRM","Cooperative","India","Brazil","China","USA") ) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  ggplot() +
  geom_hline(yintercept=0) +
  geom_ribbon(aes(x=meanlat,
                  ymin=-1,
                  ymax=1),
              color="grey",
              linewidth=1,
              alpha=0.2) +
  geom_point(
    #data=.%>%filter(nsrm !="no SRM"),
    aes(x=meanlat,
        y=(prec-1)/sd,
        color=Scenario),
    alpha=0.2) + 
  stat_smooth(
    #data=.%>%filter(nsrm !="no SRM"),
    aes(x=meanlat,
        y=(prec-1)/sd,
        color=Scenario,
        weight=pop), 
    se = FALSE,
    linewidth=2 ) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=regpalette_scenarios,
                     name="Precipitation impacts") +
  xlab("") + ylab("Precipitation variation [standard deviations]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=7))
