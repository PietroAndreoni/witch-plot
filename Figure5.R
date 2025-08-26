#main_scenarios_noncoop <- sanitized_names %>% filter(impacts!='bhm' & (zinj=="free" & !nsrm %in% c("Cooperative","no SRM")) | ((zinj=="free" & nsrm %in% c("Cooperative","no SRM")))  )
main_scenarios_noncoop <- sanitized_names %>% filter(zinj=="symmetric"  | (zinj=="free" & nsrm %in% c("Cooperative","no SRM"))  )
main_scenarios_noncoop <- sanitized_names 

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
  inner_join(main_scenarios_noncoop) %>%
  filter(ttoyear(t)==2100) %>%
  inner_join(countries_map) %>%
  group_by_at(c("t","n",setdiff(colnames(sanitized_names),c("nsrm","COOP","Scenario","file","pathdir","zinj"))) ) %>%
  mutate(betteroff_nash=(value-value[nsrm=="no SRM" & COOP=="noncoop" & zinj=="free"]),
         betteroff_paris=(value-value[nsrm=="no SRM" & COOP=="coop" & zinj=="free"]),
         betteroff_coop=(value-value[nsrm=="Cooperative" & COOP=="coop" & zinj=="free"]) ) %>%
  mutate(disc = case_when(betteroff_coop < 0 & betteroff_paris < 0  ~ "Laissez-faire",
                          betteroff_coop > 0 & betteroff_paris < 0 ~ "Push to cooperation",
                          betteroff_paris > 0 & betteroff_nash < 0 ~ "Non-use",
                          betteroff_paris > 0 & betteroff_nash > 0 ~ "Non-use (strong)")) %>%
  filter(!nsrm %in% c("no SRM","Cooperative")) %>%
  mutate(disc=ifelse(n=="row","NA",as.character(disc) )) %>%
  inner_join(perc_impact %>% select(file,n,Main_source) ) %>%
  ggplot() +
  geom_polygon(data=.%>% left_join(reg %>% filter(iso3!='ATA')),
                                   aes(x = lat, y = long-180,group = group, fill = disc), color="grey50",size=.05) +
  # ggpattern::geom_polygon_pattern(data= .%>% 
  #                                   left_join(reg %>% filter(iso3!='ATA')),
  #                                 aes(x = lat, y = long-180,group = group, fill = disc, pattern = Main_source),
  #                                 color="black",
  #                                 size=.1,
  #                                 pattern_density=0.02,
  #                                 pattern_colour="grey20",
  #                                 pattern_fill="grey20",
  #                                 pattern_size=0.25) +
  # ggpattern::scale_pattern_manual(values=c("temp" = "none",
  #                                          "prec" = "circle",
  #                                          "mixed" = "stripe"),
  #                                 name="Agreement") +
  geom_polygon(data= .%>% 
                 left_join(reg %>% filter(iso3!='ATA')) %>% 
                 filter(n==n_to_name[nsrm]), 
               aes(x = lat, y = long-180,group = group),fill=NA,color='black',size=.4) +
  geom_point(data= Z_SAI %>% 
               inner_join(main_scenarios_noncoop) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding","Mitigation","Mitigation + SAI") & value != 0),
             aes(x=ninj, y = 20-360, size= value ), shape=21, color="black", fill=NA ) +
  geom_text(data= Z_SAI %>% 
               inner_join(main_scenarios_noncoop) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & !Scenario %in% c("Free-riding","Mitigation","Mitigation + SAI") & value != 0),
             aes(x=ninj-5, y = 20-360, label= paste0(round(value,0)," TgS/yr")), color="black", size=3 ) +
  geom_point(data= Z_SAI %>% 
               inner_join(main_scenarios_noncoop) %>%
               mutate(ninj=ifelse(str_detect(inj,"S"),- as.numeric(str_remove(inj,"S")), as.numeric(str_remove(inj,"N") ))) %>%
               filter(ttoyear(t)==2100 & Scenario=="Mitigation + SAI" & zinj=="free" & value != 0) %>% 
               select(-Scenario,-nsrm),
             aes(x=ninj, y = 20-360, size= value ), shape=21, color="grey", fill=NA ) +
  geom_text(data= W_SAI %>% 
               inner_join(main_scenarios_noncoop) %>%
               filter(ttoyear(t)==2100 & Scenario=="Mitigation + SAI" & zinj=="free" & value != 0) %>% 
               select(-Scenario,-nsrm),
             aes(x=-60, y = 20-360, label= paste0(round(value,0)," TgS/yr")), color="grey", size=3 ) +
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
        fill=ordered(disc,c("Laissez-faire","Push to cooperation","Push to mitigation","Non-use","Non-use (strong)","NA"))),
          color="grey50",stat="identity",position="stack",width=10) +
  scale_fill_manual(values=c("Laissez-faire"="#003049",
                             "Push to cooperation"="#EFF0F2",
                             "Non-use"="#C98C7E",
                             "Non-use (strong)"="#780000",
                             "NA"="white"),
                    name="Preferred strategy") +
  scale_color_viridis_d() +
  theme_void()+ 
  guides(color="none",size="none") +
  facet_wrap(nsrm~.,nrow=2) +
  theme(legend.position = "top") +
  coord_flip() +
  theme(text = element_text(size = 12))

ggsave("Fig_noncoop.png",damages_maps,width=18,height=12,units="cm")


ggplot(gdploss %>%        
         inner_join(pop %>% select(t,n,value) %>% rename(pop=value) %>% unique()) %>% 
         inner_join(countries_map) %>% 
         filter(ttoyear(t)==2100 & !Scenario %in% c("Mitigation","Mitigation + SAI","Free-riding")) %>% 
         ungroup() %>% 
         filter(!is.na(valuerel_paris) & quantile(valuerel_paris,0.95,na.rm=TRUE) & valuerel_paris>=quantile(valuerel_paris,0.05,na.rm=TRUE)))+
  geom_vline(xintercept=0,linetype=2,color="grey") +
#  geom_vline(xintercept=1,linetype=3,color="grey") +
  ggridges::geom_density_ridges(aes(x = valuerel_paris, y = latitude, color=latitude,weight=pop),
                                rel_min_height = 0.005,fill=NA,
                                quantile_lines = TRUE, 
                                jittered_points = TRUE,
                                position = ggridges::position_points_jitter(width = 0.05, height = 0),
                                point_shape = '|', point_size = 1, point_alpha = 0.7) +
  ylab('') + xlab(' (SCEN - COOP)/(MITIGATION - COOP)') +
  coord_cartesian(xlim=c(-1,5))


gdploss2100 <- gdploss %>% 
  inner_join(main_scenarios_noncoop) %>% 
  filter(ttoyear(t)==2100 & !(Scenario=="Mitigation" & impacts %in% c("BHM")) & n!="row" ) %>% 
  inner_join(countries_map) %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) %>% unique()) %>%
  ggplot() +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point(aes(x=meanlat,
                 y=value,
                 color=Scenario),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=value,
                  color=Scenario,
              weight=pop), 
se = FALSE,
method = "loess",
linewidth=2) +
  theme(legend.position="bottom") +
  scale_color_manual(values=regpalette_srm) +
  scale_fill_manual(values=regpalette_srm) +
  facet_wrap(impacts~.,nrow=1) +
  xlab("") + ylab("Local temperature variation rtm 1980-2018 [°C]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=12)) + coord_flip()

