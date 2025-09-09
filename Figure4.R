#main_scenarios_noncoop <- sanitized_names %>% filter(impacts!='bhm' & (zinj=="free" & !nsrm %in% c("Cooperative","no SRM")) | ((zinj=="free" & nsrm %in% c("Cooperative","no SRM")))  )
#main_scenarios_noncoop <- sanitized_names %>% filter(zinj=="symmetric"  | (zinj=="free" & nsrm %in% c("Cooperative","no SRM"))  )
main_scenarios_noncoop <- sanitized_names %>% filter(impacts==imp_select & ci_imp==ci_sel & dsc_p==downscaling_sel)

#cowplot::ggdraw(gdploss_barplot) + cowplot::draw_plot(plot=map,x=.7,y=.6,width=.3,height=.2) 
n_to_name <- c("Brazil"="bra","India"="ind","China"="chn","USA"="usa")

preferred_position <- gdploss %>% 
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
  mutate(disc=ifelse(n=="row","NA",as.character(disc) )) 

damages_maps <- preferred_position %>%
  ggplot() +
  geom_polygon(data= . %>% left_join(reg %>% filter(iso3!='ATA')),
                                   aes(x = lat, y = long-180,group = group, fill = disc), color="grey50",size=.05) +
  geom_polygon(data= . %>% 
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
                             "Push to cooperation"="#b4cded",
                             "Non-use"="#C98C7E",
                             "Non-use (strong)"="#780000",
                             "NA"="grey80"),
                    name="Preferred strategy") +
  scale_color_viridis_d() +
  theme_void()+ 
  guides(color="none",size="none") +
  facet_wrap(nsrm~.,nrow=4,, strip.position = "left") +
  theme(legend.position = "top") +
  scale_x_continuous(expand=c(0,0),limits = c(-80,84)) +
  coord_flip() +
  theme(text = element_text(size = 7),   
        plot.margin = margin(l = 0, r = 0, t = 0, b = 0))

dist_impacts_bytype <- get_witch("damfrac_type") %>%
  mutate(source=case_when(str_detect(d,"temp")~"Temperature impacts",
                          str_detect(d,"prec")~"Precipitation impacts",
                          str_detect(d,"spill")~"Spillover effects",
                          .default="others")) %>%
  bind_rows(abatefrac) %>%
  bind_rows(saifrac) %>%
  mutate(source=ifelse(source %in% c("ab","sai"), "Climate policy (incl. SAI)", source )) %>% 
  group_by(t,n,file,source) %>% 
  summarise(value=sum(value)) %>% 
  inner_join(main_scenarios_noncoop) %>%  
#  inner_join(YGROSS %>% rename(ykali=value)) %>%
#  group_by(t,n,file,source) %>%
#  summarise(value=sum(value*ykali),ykali=mean(ykali)) %>%
#  ungroup() %>% mutate(value=value/ykali) %>% 
  inner_join(countries_map %>% 
               mutate(latitude2=case_when(latitude %in% c("Equatorial","Tropical","Subtropical") ~ "low",
                                          latitude %in% c("Mid latitudes") ~ "mid",
                                          latitude %in% c("High latitudes") ~ "high"),
                      latitude_n=ifelse(latitude_n==75,60,latitude_n)) ) %>% 
  inner_join(pop %>% rename(pop=value)) %>% 
  filter(ttoyear(t)==2100) %>% 
  group_by(t,file,latitude_n,source) %>%
  summarise(max=ggdist::weighted_quantile(value,0.95,pop),
            sdup=ggdist::weighted_quantile(value,0.66,pop),
            sddo=ggdist::weighted_quantile(value,0.33,pop),
            min=ggdist::weighted_quantile(value,0.05,pop),
            med=ggdist::weighted_quantile(value,0.5,pop))


bars <- ggplot(dist_impacts_bytype %>% 
              inner_join(preferred_position %>% 
                           select(n,file,disc) %>% 
                           inner_join(pop %>% rename(pop=value)) %>% 
                           inner_join(countries_map) %>% 
                           group_by(file,latitude_n,disc) %>% 
                           summarise(n=n(),pop=sum(pop)) %>% 
                           group_by(file,latitude_n) %>%
                           filter(pop==max(pop)) %>%  
                           select(-n)) %>% 
              inner_join(sanitized_names) %>% 
              filter(Scenario %in% c("Brazil","India","China","USA") & source != "Climate policy (incl. SAI)")) +
  geom_hline(yintercept=0,color="grey50",linewidth=0.5) +
  geom_bar(aes(x=latitude_n,y=med,alpha=source,group=source,fill=disc),stat="identity",position="dodge",color="grey20",width=10,linewidth=0.1) +
  geom_errorbar(aes(x=latitude_n,ymin=sddo,ymax=sdup,group=source),position=position_dodge(),width=10,linewidth=0.1,color="grey20") +
#  geom_errorbar(aes(x=latitude_n,ymin=min,ymax=max,color=disc,alpha=source,group=source),position=position_dodge(),linewidth=0.1,width=0.1) +
  scale_fill_manual(values=c("Laissez-faire"="#003049",
                             "Push to cooperation"="#b4cded",
                             "Non-use"="#C98C7E",
                             "Non-use (strong)"="#780000",
                             "NA"="grey80"),
                    name="Preferred strategy") +
  scale_color_manual(values=c("Laissez-faire"="#003049",
                             "Push to cooperation"="#b4cded",
                             "Non-use"="#C98C7E",
                             "Non-use (strong)"="#780000",
                             "NA"="grey80"),
                    name="Preferred strategy") +
#  scale_alpha_manual(values=c("Temperature impacts"=1,"Precipitation impacts"=0.3)) +
  facet_wrap(Scenario~.,ncol=1) + 
  scale_x_continuous(expand=c(0,0),limits = c(-80,84)) +
  coord_flip() +   theme_minimal(base_size = 7) +
  guides(alpha="none",fill="none") +
  theme(axis.title.y = element_blank(),
        axis.title.x = element_blank(),
        axis.text.y  = element_blank(),
        axis.ticks.y = element_blank(),
#        axis.text.x  = element_blank(),
#        axis.ticks.x = element_blank(),
        strip.text = element_blank(),
        panel.grid.major.y = element_blank(), # cleaner
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(), # cleaner
        plot.margin = margin(l = 0, r = 0, t = 0, b = 0))

require(patchwork)
ggsave("fig4.png",
       plot=damages_maps + bars + plot_layout(widths = c(1, 0.4), guides = "collect"),
       width=18, 
       height=24, 
       units="cm",
       dpi=400)
