n_to_name <- c("USA"="usa","India"="ind","China"="chn","Brazil"="bra")

temperature_maps <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(sanitized_names) %>%
  inner_join(optimal_temp) %>%
  group_by(n) %>%
  mutate(temp=temp-opttemp) %>%
  filter(ttoyear(t)==2100 & impacts!="bhm" & Scenario %in% c("Mitigation","Mitigation + SAI","USA","India") ) %>% 
  inner_join(countries_map) %>% 
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = -temp),color='black',size=.1) +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = -temp),color='red',size=.4) +
  geom_point(data=Z_SAI %>% 
               inner_join(sanitized_names) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & impacts!="bhm" & Scenario %in% c("Mitigation","Mitigation + SAI","USA","India") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="red", fill=NA ) +
  scale_fill_gradient2() +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_grid(ordered(Scenario,c("Mitigation","China","USA","Mitigation + SAI","Brazil","India","Free-riding"))~impacts)


precipitation_maps <- PREC %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & impacts!="bhm" & Scenario %in% c("Mitigation","Mitigation + SAI","USA","India")) %>%
  inner_join(countries_map) %>% 
  inner_join(sd_prec) %>%
  inner_join(optimal_prec) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = (prec-optprec)/sd),size=.1,color="black") +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = (prec-optprec)/sd),color='red',size=.4) +
  geom_point(data=Z_SAI %>% 
               inner_join(sanitized_names) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & impacts!="bhm" & Scenario %in% c("Mitigation","Mitigation + SAI","USA","India") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="red", fill=NA ) +
  scale_fill_gradient2() +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_grid(ordered(Scenario,c("Mitigation","Mitigation + SAI","China","USA","Brazil","India"))~impacts)

fig_maps <- ggarrange(temperature_maps,precipitation_maps, nrow=2, labels = c("a","b"))
ggsave("fig_maps.png",plot=fig_maps,width=18, height=10, units="cm")
