main_scenarios_noncoop <- sanitized_names 

abatefrac <- get_witch("ABATECOST") %>%
  group_by(file,t,n) %>%
  summarise(value=sum(value)) %>%
  inner_join(YGROSS %>% rename(ykali=value)) %>%
  mutate(value = value/ykali,source="ab") %>%
  select(file,n,value,source,t) %>%
  complete()

saifrac <- get_witch("COST_SAI") %>%
  group_by(file,t,n) %>%
  summarise(value=sum(value)) %>%
  inner_join(YGROSS %>% rename(ykali=value)) %>%
  mutate(value = value/ykali,source="sai") %>%
  select(file,n,value,source,t) %>%
  complete()

perc_impact <- get_witch("damfrac_type") %>%
  mutate(source=case_when(str_detect(d,"temp")~"temp",
                          str_detect(d,"prec")~"prec",
                          str_detect(d,"spill")~"spill",
                          .default="others")) %>%
  group_by(t,n,file,source) %>%
  summarise(value=sum(value)) %>%
  ungroup() %>%
  bind_rows(abatefrac) %>%
  bind_rows(saifrac) %>%
  filter(ttoyear(t)==2100) %>%
  group_by(file,n,t) %>%
  mutate(perc=value/sum(value)) %>% 
  complete()


gdploss_barplot <- gdploss %>% 
  inner_join(main_scenarios_noncoop) %>%
  inner_join(perc_impact %>% select(-value)) %>%
  inner_join(pop %>% rename(pop=value)) %>%
  inner_join(countries_map) %>%
  group_by(n,source,impacts) %>%
  mutate(valueerel=-(perc*value-perc[nsrm=="Cooperative" & COOP=="coop"]*value[nsrm=="Cooperative" & COOP=="coop"]) /(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop"])) %>%
  filter(!nsrm %in% c("no SRM","Cooperative") ) %>%
  ggplot() +
  geom_hline(yintercept=100) +
  ggpattern::geom_bar_pattern(data=. %>% group_by(latitude,file,t,source) %>%
                                summarise(med=modi::weighted.quantile(valueerel*100,pop,0.5) ) %>%
                                inner_join(sanitized_names),
                              aes(x=interaction(impacts, latitude ),
                                  y=med,
                                  fill=latitude,
                                  pattern=source,
                                  alpha=source),
                              color="black",
                              stat="identity",
                              position="stack") +
  # geom_errorbar(data=.%>% 
  #                 group_by(latitude,file,t,n) %>%
  #                 summarise(value=sum(valueerel*100),
  #                           pop=sum(pop) ) %>%
  #                 group_by(latitude,file,t) %>%
  #                 summarise(max=quantile(value,0.75),
  #                           med=median(value),
  #                           min=quantile(value,0.25))%>%
  #                 inner_join(sanitized_names),
  #               aes(x=interaction(impacts, latitude ), 
  #                   ymin=min, 
  #                   y=med,
  #                   ymax=max,
  #                   group=latitude), 
  #               width=0.25) +
  # geom_point(data=.%>% 
  #              group_by(latitude,file,t,n) %>%
  #              summarise(value=sum(valueerel*100),
  #                        pop=sum(pop) ) %>%
  #              group_by(latitude,file,t) %>%
  #              summarise(med=quantile(value,0.5)) %>%
  #              inner_join(sanitized_names),
  #            aes(x=interaction(impacts, latitude ), 
  #                y=med,
  #                group=latitude), 
  #            size=2,color="black") +
  coord_flip()+
  xlab("") + ylab("%")+
  ggpattern::scale_pattern_manual(labels=c("Mitigation"="ab",
                                           "Precipitations"="prec",
                                           "Temperature"="temp",
                                           "Spillover"="spill",
                                           "SAI"="sai"),
                                  values=c(ab = "none", 
                                           temp = "none",
                                           prec = "circle",
                                           sai = "none",
                                           spill = "stripe"),
                                  name="Source") +
  scale_alpha_manual(labels=c("Mitigation"="ab",
                              "Precipitations"="prec",
                              "Temperature"="temp",
                              "Spillover"="spill",
                              "SAI"="sai"),
                     values=c("ab"=0.1,
                              "sai"=0.5,
                              "temp"=1,
                              "prec"=1,
                              "spill"=1),
                     name="Source") +
  theme_pubr() +
  theme(text = element_text(size = 7)) +
  facet_wrap(ordered(nsrm,c("USA","China","India","Brazil"))~.,nrow=1) +
  scale_x_discrete(guide = ggh4x::guide_axis_nested(delim="."))


impact <- get_witch("damfrac_type") %>%
  mutate(source=case_when(str_detect(d,"temp")~"temp",
                          str_detect(d,"prec")~"prec",
                          str_detect(d,"spill")~"spill",
                          .default="others")) %>%
  select(-d,-pathdir) %>%
  inner_join(countries_map %>% select(n,latitude)) %>%
  bind_rows(get_witch("ABATECOST") %>% 
               as_tibble() %>%
               group_by(file,t,n) %>%
               summarise(value=sum(value)) %>% 
               mutate(source="ab")) %>%
  bind_rows(get_witch("COST_SAI") %>% 
               as_tibble() %>%
               mutate(source="sai")) %>%
  filter(ttoyear(t)==2100) %>%
  group_by(t,latitude,file,source) %>%
  summarise(value=sum(value,na.rm=TRUE)) %>% 
  complete()
  

gdploss_barplot_v2 <- impact %>%
  inner_join(Y %>%
               full_join(YGROSS %>% rename(ykali=value)) %>%
               inner_join(countries_map %>% select(n,latitude)) %>% 
               group_by(t,latitude,file) %>%
               summarise(gdploss=sum(value-ykali)) ) %>%
  inner_join(sanitized_names) %>%
  group_by(t,latitude,source) %>%
  mutate(valueerel=(value-value[nsrm=="Cooperative" & COOP=="coop"]) /(gdploss[nsrm=="Cooperative" & COOP=="coop"]-gdploss[nsrm=="no SRM" & COOP=="coop"])) %>%
  filter(!nsrm %in% c("no SRM","Cooperative") ) %>%
  ggplot() +
  geom_hline(yintercept=100) +
  geom_bar(aes(x=interaction(impacts, latitude ),
                                  y=valueerel*100,
                                  fill=latitude,
                                  alpha=source),
                              color="black",
                              stat="identity",
                              position="stack") +
  coord_flip()+
  xlab("") + ylab("%")+
  # ggpattern::scale_pattern_manual(labels=c("Mitigation"="ab",
  #                                          "Precipitations"="prec",
  #                                          "Temperature"="temp",
  #                                          "Spillover"="spill",
  #                                          "SAI"="sai"),
  #                                 values=c(ab = "none", 
  #                                          temp = "none",
  #                                          prec = "circle",
  #                                          sai = "none",
  #                                          spill = "stripe"),
  #                                 name="Source") +
  scale_alpha_manual(labels=c("Mitigation"="ab",
                              "Precipitations"="prec",
                              "Temperature"="temp",
                              "Spillover"="spill",
                              "SAI"="sai"),
                     values=c("ab"=0.1,
                              "sai"=0.1,
                              "temp"=1,
                              "prec"=0.5,
                              "spill"=1),
                     name="Source") +
  theme_pubr() +
  theme(text = element_text(size = 7)) +
  facet_wrap(ordered(nsrm,c("USA","China","India","Brazil"))~.,nrow=1) +
  scale_x_discrete(guide = ggh4x::guide_axis_nested(delim="."))


map <- countries_map %>%
  mutate(latitude=ifelse(n=="row","ROW",as.character(latitude))) %>%
  full_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = latitude),color='black',size=.1) +
  #  scale_fill_manual(values=rev(c("#8B0000","#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#92C5DE", "#4393C3", "#2166AC"))) + 
  scale_fill_manual(values=c("ROW"="grey",
                                "High latitudes"="#440154FF",
                                "Mid latitudes"="#3E4A89FF",
                                "Equatorial"="#DCE318FF",
                                "Tropical"="#56C667FF",
                                "Subtropical"="#1F9E89FF")) +
  theme_void() + theme(legend.position="none")

#cowplot::ggdraw(gdploss_barplot) + cowplot::draw_plot(plot=map,x=.7,y=.6,width=.3,height=.2) 
n_to_name <- c("Brazil"="bra","India"="ind","China"="chn","USA"="usa")

damages_maps <- gdploss %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100) %>%
  inner_join(countries_map) %>%
  group_by(t,n,impacts,ci_imp) %>%
  mutate(valuerel1=(value-value[nsrm=="no SRM" & COOP=="noncoop"]),
         valuerel2=(value-value[nsrm=="no SRM" & COOP=="coop"]),
         valuerel3=(value-value[nsrm=="Cooperative" & COOP=="coop"]) ) %>%
  mutate(disc = case_when(valuerel3 < 0 ~ "Laissez-faire",
                          valuerel3 > 0 & valuerel2 < 0 ~ "Push to cooperation",
                          valuerel2 > 0 & valuerel1 < 0 ~ "Push to mitigation",
                          valuerel1 > 0 ~ "Non-use") ) %>%
  filter(!nsrm %in% c("no SRM","Cooperative")) %>%
  mutate(disc=ifelse(n=="row","NA",as.character(disc) )) %>%
  ggplot() +
  geom_polygon(data=.%>% left_join(reg %>% filter(iso3!='ATA')),
               aes(x = lat, y = long-180,group = group, fill = disc),size=.1,color="black") +
  geom_polygon(data= .%>% 
                 left_join(reg %>% filter(iso3!='ATA')) %>% 
                 filter(n==n_to_name[nsrm]), 
               aes(x = lat, y = long-180,group = group),fill=NA,color='red',size=.4) +
  geom_point(data= Z_SAI %>% 
               inner_join(sanitized_names) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding","Mitigation","Mitigation + SAI") & value != 0),
             aes(x=ninj, y = 20-360, size= value ), shape=21, color="red", fill=NA ) +
  geom_bar(data= .%>%
              inner_join(pop %>% rename(pop=value)) %>%
              inner_join(ykali %>% rename(y0=value)) %>%
              group_by(file,disc) %>%
              summarise(pop=sum(pop),y=sum(y0)) %>%
              group_by(file) %>%
              mutate(pop=pop/sum(pop), y = y/sum(y)) %>%
              pivot_longer(c(pop,y)) %>%
              filter(name=="pop") %>%
              inner_join(sanitized_names),
    aes(x=-70, 
        y=-value*360, 
        fill=ordered(disc,c("Laissez-faire","Push to cooperation","Push to mitigation","Non-use","NA"))), 
          color="black",stat="identity",position="stack",width=10) +
  scale_fill_manual(values=c("Laissez-faire"="#4575B4",
                             "Push to cooperation"="white",
                             "Push to mitigation"="orange",
                             "Non-use"="#a2231D",
                             "NA"="grey20"),
                    name="Preferred strategy") +
  scale_color_viridis_d() +
  theme_void()+ 
  guides(color="none") +
  facet_grid(impacts~nsrm,) +
  theme(panel.background = element_rect(fill="white",color="white"),
        legend.position = "top") +
  coord_flip() +
  theme(text = element_text(size = 7))


ggplot(gdploss %>% 
         inner_join(pop %>% rename(pop=value)) %>% 
         inner_join(Y %>% rename(gdp=value)) %>% 
         mutate(gdpc=gdp/pop) %>%
         inner_join(countries_map) %>% 
         filter(ttoyear(t)==2100 & !Scenario %in% c("Mitigation","Mitigation + SAI","Free-riding") & ci_imp=="best") %>% 
         ungroup() %>% filter(!is.na(valuerel) 
#                              & valuerel<=quantile(valuerel,0.95,na.rm=TRUE) & valuerel>=quantile(valuerel,0.05,na.rm=TRUE))
       )) +
  geom_vline(xintercept=0,linetype=2,color="grey") +
  geom_vline(xintercept=1,linetype=3,color="grey") +
  ggridges::geom_density_ridges(aes(x = valuerel, y = latitude, color=latitude, weight=pop),
                      rel_min_height = 0.005,fill=NA,
                      quantile_lines = TRUE, jittered_points=TRUE) +
  # geom_vline(data=.%>%group_by(latitude,Scenario,impacts) %>%
  #              summarise(mean=modi::weighted.quantile(valuerel,pop,0.5)),
  #            aes(xintercept=mean,
  #                color=Scenario), 
  #            linetype=2) +
  #scale_color_manual(values=regpalette_srm) +
  ylab('') + xlab(' (SCEN - COOP)/(MITIGATION - COOP)') +
  facet_wrap(Scenario~.,nrow=2) + 
  coord_cartesian(xlim=c(-1,3))

gdploss_maps <- gdploss %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding","Mitigation + SAI","Mitigation")) %>%
  ungroup() %>% mutate(valuerel=ifelse(n=="row",NA,valuerel)) %>%
  full_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = -valuerel),size=.1,color="black") +
  geom_point(data=Z_SAI %>% 
               inner_join(sanitized_names) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding","Mitigation + SAI","Mitigation") & value != 0),
             aes(x=-170, y = ninj, size= value ), shape=21, color="red", fill=NA ) +
  scale_fill_gradient2(name="% GDP variation") +
  theme_void()+ 
  guides(size = FALSE) +
  theme(panel.background = element_rect(fill="white",color="white"),
        legend.position = "top") +
  facet_grid(Scenario~impacts)


fig_noncoop <- ggarrange(damages_maps,gdploss_barplot,
                         heights=c(5,3.5),
                         nrow=2, 
                         labels=c("a","b"))
ggsave("Fig_noncoop.png",fig_noncoop,width=18,height=14)



