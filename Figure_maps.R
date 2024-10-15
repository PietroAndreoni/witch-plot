precipitation_sel <- 1
n_to_name <- c("USA"="usa","India"="ind","China"="chn","Brazil"="bra")

temperature_maps <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(sanitized_names) %>%
  inner_join(optimal_temperature) %>%
  group_by(n) %>%
  mutate(temp=temp-opttemp) %>%
  filter(ttoyear(t)==2100 & pimp==precipitation_sel & !Scenario %in% c("Free-riding","2°C") ) %>% 
  inner_join(countries_map) %>% 
  ungroup() %>% 
  mutate(disc=arules::discretize(temp,method="fixed",
                                 breaks=c(-5,-2,-1,-0.2,0.2,1,2,5),
                                 labels=c("Extreme overcooling","Significant overcooling","Moderate overcooling","Optimal","Moderate undercooling","Significant undercooling","Extreme undercooling"))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),color='black',size=.1) +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = disc),color='red',size=.4) +
  scale_fill_manual(values=c("Extreme overcooling"="#4575B4",
                             "Significant overcooling"="#74ADD1",
                             "Moderate overcooling"="lightblue",
                             "Optimal"="white",
                             "Moderate undercooling"="#FDAE61",
                             "Significant undercooling"="#D73027",
                             "Extreme undercooling"="#800000"),
                    name="Temperature") +
  #scale_fill_gradient2() +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(Scenario,c("2°C","Cooperative","Brazil","India","China","USA","Free-riding"))~.,nrow=1)


precipitation_maps <- PREC %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp==precipitation_sel & !Scenario %in% c("Free-riding","2°C")) %>%
  inner_join(countries_map) %>% 
  inner_join(sd_prec) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize((prec-1)/sd,method="fixed",
                                 breaks=c(-5,-2.5,-1,-0.5,0.5,1.5,2.5,5),
                                 labels=c("Extreme decrease","Significant decrease","Moderate decrease","No variation","Moderate increase","Significant increase","Extreme increase"))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),size=.1,color="black") +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = disc),color='red',size=.4) +
#  geom_polygon(aes(x = long, y = lat,group = group, color = latitude),size=.1, fill=NA) +
  scale_fill_manual(values=c("Extreme decrease"="#800000",
                             "Significant decrease"="#D73027",
                             "Moderate decrease"="#FDAE61",
                             "No variation"="white",
                             "Moderate increase"="lightblue",
                             "Significant increase"="#74ADD1",
                             "Extreme increase"="#4575B4"),
                    name="Precipitation") +
  scale_color_viridis_d() +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(Scenario,c("2°C","Cooperative","Brazil","India","China","USA","Free-riding"))~.,nrow=1)

fig_maps <- ggarrange(temperature_maps,precipitation_maps, nrow=2)
ggsave("fig_maps.png",plot=fig_maps,width=18, height=10, units="cm")
