clim_cmip <- climate_regional_data["climate_region_coef_cmip5"] %>% 
  mutate(cmip="5pop") %>%
  bind_rows(climate_regional_data["climate_region_coef_cmip6_pop"]  %>% 
               mutate(cmip="6pop") ) %>%
  bind_rows(climate_regional_data["climate_region_coef_cmip6_area"]  %>% 
              mutate(cmip="6area") ) 


clim_cmip %>% filter(n=="ind" & V1 %in% c("alpha_temp","beta_temp","base_temp") )


ggplot(reg %>% 
         mutate(yes=ifelse(n=="row","yes","no")) %>%
         filter(iso3!='ATA')) +
  geom_polygon(aes(x = long, y = lat,group = group, fill = yes),color='black',size=.1)+
  theme_void()+
  theme(legend.position = "bottom", strip.text.x = element_text(size=12, face="bold"),legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
