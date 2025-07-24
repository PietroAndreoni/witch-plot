n_to_name <- c("USA"="usa","India"="ind","China"="chn","Brazil"="bra")

temp_impact <- IMPACT %>%
  mutate(source=ifelse(str_detect(d,"temp"),"temp","prec")) %>%
  group_by(t,n,file,source) %>%
  summarise(value=sum(value)) %>% filter(source=="temp" & t==18)

temperature_maps <- temp_impact %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding") ) %>% 
  inner_join(countries_map) %>% 
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = value),color='black',size=.1) +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = value),color='red',size=.4) +
  geom_point(data=Z_SAI %>% 
               inner_join(sanitized_names) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="red", fill=NA ) +
  scale_fill_gradient2() +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(Scenario,c("Mitigation","China","USA","Mitigation + SAI","Brazil","India"))~.,nrow=2)


prec_impact <- IMPACT %>%
  mutate(source=ifelse(str_detect(d,"temp"),"temp","prec")) %>%
  group_by(t,n,file,source) %>%
  summarise(value=sum(value)) %>% filter(source=="prec" & t==18)

precipitation_maps <- prec_impact %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding")) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = value),size=.1,color="black") +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = value),color='red',size=.4) +
  geom_point(data=Z_SAI %>% 
               inner_join(sanitized_names) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="red", fill=NA ) +
  scale_fill_gradient2() +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(Scenario,c("Mitigation","China","USA","Mitigation + SAI","Brazil","India"))~.,nrow=2)


gdploss_maps <- gdploss %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding")) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = -value),size=.1,color="black") +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = -value),color='red',size=.4) +
  geom_point(data=Z_SAI %>% 
               inner_join(sanitized_names) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="red", fill=NA ) +
  scale_fill_gradient2() +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(Scenario,c("Mitigation","China","USA","Mitigation + SAI","Brazil","India"))~.,nrow=2)
