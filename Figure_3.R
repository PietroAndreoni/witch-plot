regpalette_scenarios <- c("Optimal, no SAI"="#00A36C",
                      "USA"="#E41A1C",
                      "China"="#85467B",
                      "India"="#d8e6ad",
                      "Brazil"="#FF7F00",
                      "Optimal"="#72bcd4",
                      "Free-riding"="black") 

fig3a <- gdploss %>% 
  filter(ttoyear(t)==2100 & pimp==5) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(countries_map) %>%
  inner_join(sanitized_names) %>%
  filter(pimp %in% c("0.2","1","5") & 
           nsrm %in% c("no SRM","Cooperative","China","USA","Brazil","India") ) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm)  ) %>%
  filter(!Scenario %in% c("Optimal, no SAI","Optimal","Free-riding") ) %>%
  ggplot() +
  geom_point(aes(x=ykali/pop*1e6,
                 y=valuerel,
                 color=latitude,
                 size=pop)) +
  stat_smooth(aes(x=ykali/pop*1e6,
                 y=valuerel,
                 color=latitude,
                 size=pop),
              se=FALSE) +
  geom_hline(yintercept=0) +
  geom_hline(data=gdploss_g %>%
               filter(ttoyear(t)==2100) %>%
               inner_join(countries_map) %>%
               inner_join(sanitized_names) %>%
               filter(pimp %in% c("0.2","1","5") & 
                        nsrm %in% c("no SRM","Cooperative","China","USA","Brazil","India") ) %>%
               mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                                         nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                                         nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                                         .default=nsrm)  ) %>%
               filter(!Scenario %in% c("Optimal, no SAI","Optimal","Free-riding") & pimp %in% c("5") ),
             aes(yintercept=valuerel),
             linetype=2) +
  facet_wrap(ordered(nsrm,c("USA","China","India","Brazil"))~.,ncol=1) +
  scale_x_log10() + xlab("Income per capita") + ylab("Free-driving [% GDP loss]")

perc_impact <- IMPACT %>% 
  filter(ttoyear(t) %in% c(2090,2095)) %>%
  pivot_wider(names_from=d) %>%
  group_by(t,n) %>%
  mutate(temp = temp - temp[file=="coop_maxiso3_SRMusa_INJfree_POLcba_IMPT10modified_IMPP50modified_TSPR1"],
         prec = prec - prec[file=="coop_maxiso3_SRMusa_INJfree_POLcba_IMPT10modified_IMPP50modified_TSPR1"]) %>%
  group_by(n,file) %>%
  mutate(perc = sum(temp)/(prec[ttoyear(t)==2095]+sum(temp)) ) %>%
  select(n,file,perc)

fig3b <- DAMFRAC %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp==5) %>%
  inner_join(pop %>% rename(pop=value)) %>%
  inner_join(countries_map) %>%
  group_by(n) %>%
  mutate(valueerel=(value-value[nsrm=="Cooperative" & COOP=="coop"])/(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop"])) %>%
  filter(!nsrm %in% c("no SRM","Cooperative") ) %>%
  inner_join(perc_impact) %>%
  group_by(latitude,file,t) %>%
  summarise(max=modi::weighted.quantile(-valueerel*100,pop,0.75),
            min=modi::weighted.quantile(-valueerel*100,pop,0.25),
            med=modi::weighted.quantile(-valueerel*100,pop,0.5),
            perc=modi::weighted.quantile(perc,pop,0.5)) %>%
  mutate(prec=med*(1-perc),temp=med*perc ) %>%
  pivot_longer(c(prec,temp)) %>%
  inner_join(sanitized_names) %>%
  ggplot() +
  geom_hline(yintercept=100,color="grey",linewidth=0.7) +
  geom_hline(yintercept=0,color="grey",linewidth=0.7) +
  geom_bar(aes(x=latitude,
               y=value,
               fill=latitude,
               alpha=name),
           color="black",
           stat="identity",
           position="stack") +
#  geom_errorbar(aes(x=latitude, ymin=min, ymax=max,group=latitude), width=0.25) +
  geom_point(data=DAMFRAC %>% 
               inner_join(sanitized_names) %>%
               filter(ttoyear(t)==2100 & pimp==5) %>%
               inner_join(pop %>% rename(pop=value)) %>%
               inner_join(countries_map) %>%
               group_by(n) %>%
               mutate(valueerel=(value-value[nsrm=="Cooperative" & COOP=="coop"])/(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop"])) %>%
               filter(!nsrm %in% c("no SRM","Cooperative") ) %>%
               group_by(file) %>%
               mutate(shapeyes=ifelse(ntosrm[n]==nsrm,"yes","no")) %>%
               mutate(shapeyes=ifelse(is.na(shapeyes),"no",shapeyes)),
             aes(x=latitude,
                 y=-valueerel*100,
                 fill=latitude,
                 size=pop,
                 shape=shapeyes,
                 color=shapeyes) ) +
  geom_hline(data= damfrac_g %>% 
               inner_join(sanitized_names) %>%
               filter(ttoyear(t)==2100 & pimp==5) %>% 
               mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                                         nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                                         nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                                         .default=nsrm),
                      pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>% ungroup() %>%
               mutate(valueerel=(value-value[nsrm=="Cooperative" & COOP=="coop"])/(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop"])) %>%
               filter(!Scenario %in% c("Optimal, no SAI","Optimal","Free-riding") ),
             aes(yintercept=-valueerel*100),color="black",linewidth=1 ) +
  facet_wrap(ordered(nsrm,c("USA","China","India","Brazil"))~.,ncol=1) +
  coord_flip()+ 
  xlab("") + ylab("%")+
  scale_shape_manual(values=c(21,22)) +
  scale_color_manual(values=c("black","red")) +
  theme_pubr() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 7)) +
  ylim(c(-200,+500))
ggsave("fig3.png",plot=fig3b,width=8.8, height=8, units="cm")


fig3b <- gdploss %>% 
  filter(ttoyear(t)==2100 & pimp==5) %>%
  inner_join(pop %>% rename(pop=value)) %>%
  inner_join(countries_map) %>%
  group_by(n) %>%
  mutate(valueerel=valuerel/(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop"])/100) %>%
  filter(!nsrm %in% c("no SRM","Cooperative") ) %>%
  select(-value) %>%
  inner_join(perc_impact) %>%
  mutate(prec=valueerel*(1-perc),temp=valueerel*perc ) %>%
  pivot_longer(c(prec,temp)) %>%
  ggplot() +
  geom_bar(data=.%>%group_by(latitude,file,t,name) %>%
             summarise(med=modi::weighted.quantile(-value*100,pop,0.5)) %>%
             inner_join(sanitized_names),
           aes(x=latitude,
               y=med,
               fill=latitude,
               color=latitude,
               alpha=name),
           stat="identity",
           position="stack") +
  geom_errorbar(data=.%>%
                  group_by(latitude,file,t) %>%
                  summarise(max=modi::weighted.quantile(-value*100,pop,0.75),
                            min=modi::weighted.quantile(-value*100,pop,0.25)) %>%
                  inner_join(sanitized_names),
                aes(x=latitude, ymin=min, ymax=max,group=latitude), width=0.25) +
  facet_wrap(ordered(nsrm,c("USA","China","India","Brazil"))~.,ncol=2) +
  geom_hline(data= gdploss_g %>% 
               inner_join(sanitized_names) %>%
               filter(ttoyear(t)==2100 & pimp==5) %>% 
               mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                                         nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                                         nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                                         .default=nsrm),
                      pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>% ungroup() %>%
               mutate(valueerel=valuerel/(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop"])/100) %>%
               filter(!Scenario %in% c("Optimal, no SAI","Optimal","Free-riding") ),
             aes(yintercept=-valueerel*100) ) +
  coord_flip()+ 
  xlab("") + ylab("%")+
  theme_pubr() +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        text = element_text(size = 7)) 




precipitation_maps <- PREC %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp==5) %>%
  inner_join(countries_map) %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sd_prec) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  filter(!Scenario %in% c("Optimal, no SAI","Optimal","Free-riding") ) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize((prec-1)/sd,method="fixed",breaks=c(-5,-2,-1,-0.5,0.5,1,2,5))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),color='black',size=.1) +
  scale_fill_manual(values=c("#D73027","#F46D43","#FEE090","white","#E0F3F8","#74ADD1","#4575B4")) +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white")) +
  facet_wrap(Scenario~.,)
ggsave("suppl_fig3.png",plot=precipitation_maps,width=13, height=12, units="cm")


temperature_maps <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  inner_join(optimal_temperature) %>%
  group_by(file,n) %>%
  mutate(temp=temp-opttemp) %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp==5) %>% 
  inner_join(countries_map) %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sd_prec) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  filter(!Scenario %in% c("Optimal, no SAI","Optimal","Free-riding") ) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize(-temp,method="fixed",breaks=c(-5,-2,-1,-0.5,-0.1,0.1,0.5,1,2,5))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),color='black',size=.1) +
  scale_fill_manual(values=c("#D73027","#F46D43","#FDAE61","#FEE090","white","#E0F3F8","#ABD9E9","#74ADD1","#4575B4")) +
  #scale_fill_gradient2() +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white")) +
  facet_wrap(Scenario~.,)
ggsave("suppl_fig4.png",plot=temperature_maps,width=13, height=12, units="cm")

DAMFRAC %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp==5) %>%
  inner_join(countries_map) %>%
  group_by(n) %>%
  mutate(valueerel=(value-value[nsrm=="Cooperative" & COOP=="coop"])/(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop"])) %>%
  inner_join(sanitized_names) %>% 
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  filter(!Scenario %in% c("Optimal, no SAI","Optimal","Free-riding") ) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize(-valueerel*100,method="fixed",breaks=c(-10000000,0,+100,100000000),labels=c("Better-off","In between","Worst-off") )) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),color='black',size=.1) +
  scale_fill_manual(values=c("#74ADD1","white","#F46D43")) +
#  scale_fill_gradient2() +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white")) +
  facet_wrap(Scenario~.,)


 gdploss_g %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & pimp==5) %>% 
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  filter(!Scenario %in% c("Optimal","Free-riding") ) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),y=valuerel,color=Scenario) ) +
  scale_color_manual(values=regpalette_srm)  + theme_pubr()
  
  
  