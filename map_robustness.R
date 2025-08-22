files <- list.files("../Results_secondround/Impacts",pattern=".gdx")
y_impacts <- batch_extract(c("Y"),paste0("../Results_secondround/Impacts/",files ) )$Y %>%
  mutate(file=str_remove_all(gdx,"../Results_secondround/Impacts/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble()
ygross_impacts <- batch_extract(c("YGROSS"),paste0("../Results_secondround/Impacts/",files ) )$Y %>%
  mutate(file=str_remove_all(gdx,"../Results_secondround/Impacts/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble() 

sanitized_names_imp <- y_impacts %>% select(file) %>% unique() %>% sanitize() %>% filter(!(nsrm=="no SRM" & COOP=="noncoop"))
gdploss_imp <- y_impacts %>%
  full_join(ygross_impacts %>% rename(ykali=value)) %>%
  mutate(value=(ykali-value)/ykali )  %>%
  inner_join(sanitized_names_imp) %>%
  filter(ttoyear(t)==2100) %>%
  complete(t,n,impacts,ci_imp,COOP,nsrm) %>%
  filter(!(COOP=="coop" & !nsrm %in% c("Cooperative","no SRM") ) ) %>%
  group_by(t,n,impacts,ci_imp) %>% 
  mutate(valuerel=(value-value[nsrm=="Cooperative" & COOP=="coop"])/(value[nsrm=="no SRM" & COOP=="coop"]-value[nsrm=="Cooperative" & COOP=="coop"]) )

ranks_imp <-  gdploss_imp %>% 
  inner_join(sanitized_names_imp) %>%
  filter(ttoyear(t)==2100) %>%
  inner_join(countries_map) %>%
  group_by(t,n,impacts,ci_imp) %>%
  mutate(valuerel1=(value-value[nsrm=="no SRM" & COOP=="noncoop"]),
         valuerel2=(value-value[nsrm=="no SRM" & COOP=="coop"]),
         valuerel3=(value-value[nsrm=="Cooperative" & COOP=="coop"]) ) %>%
  mutate(disc = case_when(valuerel3 < 0 ~ "Laissez-faire",
                          valuerel3 > 0 & valuerel2 < 0 ~ "Push to cooperation",
                          valuerel2 > 0 & valuerel1 < 0 ~ "Non-use",
                          valuerel1 > 0 ~ "Non-use") ) 

max_impacts <- 5*5
freedrivers <- 4
agreement <- ranks_imp %>% 
  filter(!is.na(disc)) %>%
  group_by(n,disc,nsrm) %>% 
  summarise(count=n()) %>%
  group_by(n,nsrm) %>% 
  filter(count==max(count)) %>% 
  ungroup() %>% 
  mutate(agr=case_when(count>=0.8*max_impacts~"high",
                       count<0.8*max_impacts & count>=0.5*max_impacts~"medium",
                       count<0.5*max_impacts~"low"))

damages_maps <-  agreement %>%
  ungroup() %>% mutate(disc=ifelse(n=="row","NA",as.character(disc) )) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  #  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),size=.1,color="black") +
  ggpattern::geom_polygon_pattern(aes(x = long, y = lat,group = group, fill = disc, pattern=agr),
                                  color="black",
                                  size=.1,
                                  pattern_density=0.02,
                                  pattern_colour="grey20",
                                  pattern_fill="grey20",
                                  pattern_size=0.25) +
  ggpattern::scale_pattern_manual(values=c("high" = "none",
                                           "medium" = "circle",
                                           "low" = "stripe"),
                                  name="Agreement") +
  scale_fill_manual(values=c("Laissez-faire"="#4575B4",
                             "Push to cooperation"="white",
                             "Non-use"="#a2231D",
                             "NA"="grey20"),
                    name="Preferred strategy") +
  scale_color_viridis_d() +
  theme_void()+ 
  guides(color="none") +
  facet_wrap(nsrm~.,nrow=1) +
  theme(panel.background = element_rect(fill="white",color="white"),
        legend.position = "top") +
  theme(text = element_text(size = 7))
ggsave("agrement_maps.png",damages_maps,width=18,height=9)

z_sai_impacts <- gdxtools::batch_extract("Z_SAI",
                                     files=paste0("../Results_secondround/Impacts/",list.files(path="../Results_secondround/Impacts",pattern="results_")))$Z_SAI %>%
  mutate(file=str_remove_all(gdx,"../Results_secondround/Impacts/|.gdx"),
         t=as.numeric(t)) %>% select(-gdx) %>% as_tibble()

injections <- z_sai_impacts %>% 
  inner_join(sanitized_names_imp) %>%
  filter(t==18 & Scenario!="Mitigation" & value>0) %>%
  group_by(t,Scenario,impacts,ci_imp) %>% 
  summarise(bar=weighted.mean(injton(inj),value), sd=sqrt(Hmisc::wtd.var(injton(inj),value)), ninj=n()  ) %>%
  pivot_longer(c(bar,sd,ninj))


ggplot(injections %>% pivot_wider() %>% filter(Scenario=="Mitigation + SAI")) +
  geom_point(aes(x=impacts,y=bar,color=ci_imp))


preferred_strategy <- z_sai_impacts %>% 
  inner_join(sanitized_names_imp) %>%
  group_by(t,Scenario,impacts,ci_imp) %>% 
  filter(t==18 & Scenario!="Mitigation" & value>0.05*sum(value)) %>%
  group_by(t,Scenario,impacts,ci_imp) %>% 
  summarise(strategy=paste0(inj,collapse="+")) %>% 
  group_by(t,Scenario,impacts,strategy) %>% 
  mutate(n=n()) %>%   
  group_by(t,impacts,Scenario) %>% 
  filter(n==max(n)) %>% 
  select(Scenario,impacts,strategy,n) %>%  
  unique()

preferred_strategy <- z_sai_impacts %>% 
  inner_join(sanitized_names_imp) %>%
  group_by(t,Scenario,impacts,ci_imp) %>% 
  filter(t==18 & Scenario!="Mitigation" & value>0 & !is.na(value)) %>%
  mutate(emishpere=sign(injton(inj)) ) %>% 
  group_by(t,Scenario,impacts,ci_imp) %>% 
  mutate(tot=sum(value)) %>%   
  group_by(t,Scenario,impacts,ci_imp,emishpere) %>% 
  summarise(perc=sum(value)/mean(tot)) %>% 
  group_by(t,Scenario,impacts,ci_imp) %>% 
  filter(perc==max(perc))

ggplot(injections %>% pivot_wider) +
  geom_vline(xintercept=c(-45,-30,-15,0,15,30,45),color="grey") +
  geom_segment(data=.%>% mutate(Scenario=as.factor(Scenario)) %>%
                 group_by(Scenario) %>%
                 summarise(nscen=as.numeric(Scenario)/300,
                           xmin=median(bar,na.rm=TRUE)-mean(sd,na.rm=TRUE),
                           xmax=median(bar,na.rm=TRUE)+mean(sd,na.rm=TRUE),
                           ninj=round(median(ninj))),
               aes(x=xmin,
                   xend=xmax,
                   y=-0.02+nscen,yend=-0.02+nscen,
                   color=Scenario),linewidth=1) +
  geom_text(data=.%>% mutate(Scenario=as.factor(Scenario)) %>%
              group_by(Scenario) %>%
              summarise(nscen=as.numeric(Scenario)/300,
                        xmin=median(bar,na.rm=TRUE)-mean(sd,na.rm=TRUE),
                        xmax=median(bar,na.rm=TRUE)+mean(sd,na.rm=TRUE),
                        ninj=round(median(ninj))),
            aes(x=xmin-2,y=-0.02+nscen,label=ninj,color=Scenario)) +
  geom_vline(data=.%>% 
               group_by(Scenario) %>%
               summarise(med=median(bar,na.rm=TRUE)),
             aes(xintercept=med,color=Scenario), linewidth=0.5, linetype=2 ) +
  geom_density(aes(x=bar,color=Scenario,weight=impton[ci_imp]),fill=NA,linewidth=1.5) +
    scale_color_manual(values=regpalette_srm) + theme(legend.position = "none") +  facet_grid(Scenario~.) 

impton <- c("hi"=0.05,"lo"=0.05,"mhi"=0.33,"mlo"=0.33,"best"=0.5)
ggplot(gdploss_imp %>%        
       inner_join(pop %>% select(t,n,value) %>% rename(pop=value)%>% unique()) %>% 
         inner_join(countries_map) %>% 
         inner_join(preferred_strategy) %>%
         filter(ttoyear(t)==2100 & !Scenario %in% c("Mitigation","Mitigation + SAI","Free-riding")) %>% 
         ungroup() %>% 
         filter(!is.na(valuerel) & valuerel<=quantile(valuerel,0.95,na.rm=TRUE) & valuerel>=quantile(valuerel,0.05,na.rm=TRUE)))+
  geom_vline(xintercept=0,linetype=2,color="grey") +
  geom_vline(xintercept=1,linetype=3,color="grey") +
ggridges::geom_density_ridges(aes(x = valuerel, y = latitude, color=latitude,weight=pop*impton[ci_imp]),
                                rel_min_height = 0.005,fill=NA,
                                quantile_lines = TRUE, 
                              jittered_points = TRUE,
                              position = ggridges::position_points_jitter(width = 0.05, height = 0),
                              point_shape = '|', point_size = 1, point_alpha = 0.7) +
  ylab('') + xlab(' (SCEN - COOP)/(MITIGATION - COOP)') +
#  facet_grid(as.factor(emishpere)~.) + 
  coord_cartesian(xlim=c(-1,3))
