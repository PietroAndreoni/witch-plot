n_to_name <- c("USA"="usa","India"="ind","China"="chn","Brazil"="bra")

temperature_maps <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(sanitized_names) %>%
  inner_join(optimal_temp) %>%
  group_by(n) %>%
  mutate(temp=temp-opttemp) %>%
  filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding") ) %>% 
  inner_join(countries_map) %>% 
  ungroup() %>% 
  mutate(disc=arules::discretize(temp,method="fixed",
                                 breaks=c(-5,-2,-1,-0.2,0.2,1,2,5),
                                 labels=c("Extreme overcooling","Significant overcooling","Moderate overcooling","Optimal","Moderate undercooling","Significant undercooling","Extreme undercooling"))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = temp),color='black',size=.1) +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = temp),color='red',size=.4) +
  geom_point(data=Z_SAI %>% 
               inner_join(sanitized_names) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="red", fill=NA ) +
  # scale_fill_manual(values=c("Extreme overcooling"="#4575B4",
  #                            "Significant overcooling"="#74ADD1",
  #                            "Moderate overcooling"="lightblue",
  #                            "Optimal"="white",
  #                            "Moderate undercooling"="#FDAE61",
  #                            "Significant undercooling"="#D73027",
  #                            "Extreme undercooling"="#800000"),
  #                   name="Temperature") +
  scale_fill_gradient2() +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(Scenario,c("Mitigation","China","USA","Mitigation + SAI","Brazil","India"))~.,nrow=2)


precipitation_maps <- PREC %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding")) %>%
  inner_join(countries_map) %>% 
  inner_join(sd_prec) %>%
  inner_join(optimal_prec) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize((prec-optprec)/sd,method="fixed",
                                 breaks=c(-5,-2.5,-1,-0.5,0.5,1.5,2.5,5),
                                 labels=c("Extreme decrease","Significant decrease","Moderate decrease","No variation","Moderate increase","Significant increase","Extreme increase"))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = (prec-optprec)/sd),size=.1,color="black") +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = (prec-optprec)/sd),color='red',size=.4) +
  geom_point(data=Z_SAI %>% 
               inner_join(sanitized_names) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="red", fill=NA ) +
  # scale_fill_manual(values=c("Extreme decrease"="#800000",
  #                            "Significant decrease"="#D73027",
  #                            "Moderate decrease"="#FDAE61",
  #                            "No variation"="white",
  #                            "Moderate increase"="lightblue",
  #                            "Significant increase"="#74ADD1",
  #                            "Extreme increase"="#4575B4"),
  #                   name="Precipitation") +
  scale_fill_gradient2() +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(Scenario,c("Mitigation","China","USA","Mitigation + SAI","Brazil","India"))~.,nrow=2)

fig_maps <- ggarrange(temperature_maps,precipitation_maps, nrow=2, labels = c("a","b"))
ggsave("fig_maps.png",plot=fig_maps,width=18, height=10, units="cm")
