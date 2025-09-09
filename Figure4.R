n_to_name <- c("USA"="usa","India"="ind","China"="chn","Brazil"="bra")

temperature_maps <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(sanitized_names) %>%
  inner_join(optimal_temp) %>%
  filter(ttoyear(t)==2100 & impacts==imp_select &  Scenario %in% c("Mitigation","Mitigation + SAI","USA","India","Brazil","China")  ) %>% 
  inner_join(countries_map) %>% 
  left_join(reg %>% filter(iso3!='ATA')) %>% 
#  mutate(val=ifelse(abs(temp-opttemp)>3,sign(abs(temp-opttemp))*3,temp-opttemp )) %>% 
  mutate(val=ifelse(abs(temp-base_temp)>2,sign(temp-base_temp)*2,temp-base_temp )) %>% 
#  mutate(val=temp-base_temp) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = val),size=.05,color="grey50") +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = val),color='black',size=.2) +
  geom_point(data=Z_SAI %>% 
               inner_join(sanitized_names) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & impacts==imp_select & Scenario %in% c("Mitigation","Mitigation + SAI","USA","India","Brazil","China") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="black", fill=NA ) +
  scale_fill_gradient2(low="#003049",high="#780000",name="T variation [°C]") +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(Scenario,c("Mitigation","Mitigation + SAI","China","USA","Brazil","India"))~., nrow=2)


precipitation_maps <- PREC %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & impacts==imp_select & Scenario %in% c("Mitigation","Mitigation + SAI","USA","India","Brazil","China")) %>%
  inner_join(countries_map) %>%
  inner_join(optimal_prec) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  rowwise() %>%  
#  mutate(val=ifelse(abs((prec*base_precip-optprec*1000)/sd_prec)>3,sign((prec*base_precip-optprec*1000)/sd_prec)*3,(prec*base_precip-optprec*1000)/sd_prec )) %>% 
  mutate(val=ifelse(abs((prec*base_precip-base_precip)/sd_prec)>3,sign((prec*base_precip-base_precip)/sd_prec)*3,(prec*base_precip-base_precip)/sd_prec )) %>% 
#  mutate(val=(prec*base_precip-base_precip)/sd_prec) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = val),size=.05,color="grey50") +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = val),color='black',size=.2) +
  geom_point(data=Z_SAI %>% 
               inner_join(sanitized_names) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & impacts==imp_select & Scenario %in% c("Mitigation","Mitigation + SAI","USA","India","Brazil","China") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="black", fill=NA ) +
  scale_fill_gradient2(low="#780000",high="#003049",name="P variation [SD]") +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(Scenario,c("Mitigation","Mitigation + SAI","China","USA","Brazil","India"))~.,nrow=2)

fig_maps <- ggarrange(temperature_maps,precipitation_maps, nrow=2, labels = c("a","b"))
ggsave("fig4.png",
       plot=fig_maps,
       width=18, 
       height=24, 
       units="cm",
       dpi=400)
