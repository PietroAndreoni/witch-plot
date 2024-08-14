
temperature_maps <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(sanitized_names) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  inner_join(optimal_temperature) %>%
  group_by(n) %>%
  mutate(temp=temp-opttemp) %>%
  filter(ttoyear(t)==2100 & pimp==5) %>% 
  inner_join(countries_map) %>% 
  filter(!Scenario %in% c("Optimal, no SAI","Free-riding") ) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize(temp,method="fixed",
                                 breaks=c(-5,-1,-0.5,-0.1,0.1,0.5,1,5),
                                 labels=c("Extreme overcooling","Significant overcooling","Moderate overcooling","Optimal","Moderate undercooling","Significant undercooling","Extreme undercooling"))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),color='black',size=.1) +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = disc),color='red',size=.4) +
  scale_fill_manual(values=c("Extreme overcooling"="#4575B4","Significant overcooling"="#74ADD1","Moderate overcooling"="lightblue","Optimal"="white","Moderate undercooling"="#FDAE61","Significant undercooling"="#D73027","Extreme undercooling"="#800000"),name="Temperature") +
  #scale_fill_gradient2() +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(Scenario,c("Optimal","Brazil","India","China","USA"))~.,nrow=1)


precipitation_maps <- PREC %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp==5) %>%
  inner_join(countries_map) %>% 
  inner_join(sd_prec) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  filter(!Scenario %in% c("Optimal, no SAI","Free-riding") ) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize((prec-1)/sd,method="fixed",
                                 breaks=c(-5,-2,-1,-0.2,0.2,1,2,5),
                                 labels=c("Extreme decrease","Significant decrease","Moderate decrease","No variation","Moderate increase","Significant increase","Extreme increase"))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = disc),color='red',size=.4) +
  geom_polygon(aes(x = long, y = lat,group = group, color = latitude),size=.1) +
  scale_fill_manual(values=c("#800000","#D73027","#FDAE61","white","lightblue","#74ADD1","#4575B4"),
                    name="Precipitation") +
  scale_color_viridis_d() +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(Scenario,c("Optimal","Brazil","India","China","USA"))~.,nrow=1)

a <- countries_map %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  group_by(latitude,long,lat) %>%
  mutate(count=n()) %>% 
  ungroup() %>% 
  filter(count==1 & 
           group-lag(group) == 0 &
           ((lat-lag(lat))^2+ (long-lag(long))^2)^(1/2) < 1 ) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, color=latitude),fill="white",size=.4) +
  theme_void()

fig_maps <- ggarrange(temperature_maps,precipitation_maps, nrow=2)
ggsave("fig_maps.png",plot=fig_maps,width=18, height=10, units="cm")
