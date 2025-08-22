n_to_name <- c("Brazil"="bra","India"="ind","China"="chn","USA"="usa","Mitigation + SAI"="all")

PREC %>%
  inner_join(sanitized_names) %>% 
  filter(impacts!="bhm" & ttoyear(t)<=2100 & Scenario %in% c("Brazil","USA","China","India")) %>% 
  inner_join(optimal_prec %>% mutate(optprec=optprec*1000/base_precip)) %>% 
  inner_join(countries_map) %>% 
  inner_join(pop %>% rename(pop=value)) %>%
  mutate(latitude=ifelse(n==n_to_name[Scenario],Scenario,as.character(latitude) ),
         hemishpere=ifelse(n==n_to_name[Scenario],Scenario, "row" ),
         value=((value-1))/(sd_prec/base_precip)) %>% 
  group_by(file,t,impacts,Scenario,hemishpere) %>%
  reframe(value=weighted_quantile( value, c(0,0.33,0.5,0.66,1), pop, na.rm = TRUE), name=c("min","sdlo","med","sdhi","max") ) %>%
  pivot_wider() %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),y=med,color=hemishpere,linetype=impacts)) +
  geom_ribbon(aes(x=ttoyear(t),ymin=sdlo,ymax=sdhi,fill=hemishpere,linetype=impacts),alpha=0.1) +
#  geom_ribbon(aes(x=ttoyear(t),ymin=min,ymax=max,fill=hemishpere),alpha=0.1) +
#  geom_hline(data=.%>%filter(Scenario==latitude),aes(yintercept=optprec)) +
  facet_wrap(Scenario~.,) +
  scale_color_manual(values=c("ROW"="grey",
                             "Northern"="#3E4A89FF",
                             "Southern"="#DCE318FF",
                             "USA"="#c71585",
                             "China"="#377EB8",
                             "India"="#E41A1C",
                             "Brazil"="#FF7F00")) +
  scale_fill_manual(values=c("ROW"="grey",
                              "Northern"="#3E4A89FF",
                              "Southern"="#DCE318FF",
                              "USA"="#c71585",
                              "China"="#377EB8",
                              "India"="#E41A1C",
                              "Brazil"="#FF7F00")) 


TEMP %>%
  inner_join(sanitized_names) %>% 
  filter(impacts!="bhm" & ttoyear(t)<=2100 & Scenario %in% c("Brazil","USA","China","India")) %>% 
  inner_join(optimal_temp) %>% 
  inner_join(countries_map) %>% 
  inner_join(pop %>% rename(pop=value)) %>%
  mutate(latitude=ifelse(n==n_to_name[Scenario],Scenario,as.character(latitude) ),
         hemishpere=ifelse(n==n_to_name[Scenario],Scenario, "row" ),
         value=value-base_temp) %>% 
  group_by(file,t,impacts,Scenario,hemishpere) %>%
  reframe(value=weighted_quantile( value, c(0,0.33,0.5,0.66,1), pop, na.rm = TRUE), name=c("min","sdlo","med","sdhi","max") ) %>%
  pivot_wider() %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),y=med,color=hemishpere,linetype=impacts)) +
  geom_ribbon(aes(x=ttoyear(t),ymin=sdlo,ymax=sdhi,fill=hemishpere,linetype=impacts),alpha=0.1) +
  #  geom_ribbon(aes(x=ttoyear(t),ymin=min,ymax=max,fill=hemishpere),alpha=0.1) +
  geom_hline(aes(yintercept=0)) +
  facet_wrap(Scenario~.,) +
  scale_color_manual(values=c("ROW"="grey",
                              "Northern"="#3E4A89FF",
                              "Southern"="#DCE318FF",
                              "USA"="#c71585",
                              "China"="#377EB8",
                              "India"="#E41A1C",
                              "Brazil"="#FF7F00")) +
  scale_fill_manual(values=c("ROW"="grey",
                             "Northern"="#3E4A89FF",
                             "Southern"="#DCE318FF",
                             "USA"="#c71585",
                             "China"="#377EB8",
                             "India"="#E41A1C",
                             "Brazil"="#FF7F00")) 
