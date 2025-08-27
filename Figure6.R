main_scenarios_noncoop <- sanitized_names %>% filter(impacts!='bhm' & 
                                                       zinj %in% c("symmetric","free") | 
                                                        (zinj=="sovereign" & !nsrm %in% c("Cooperative","no SRM")))  


fig_inj <- ggplot(gdploss %>% 
         inner_join(main_scenarios_noncoop) %>% 
         mutate(zinj=ifelse(nsrm=="no SRM", "no SRM", zinj),
                Scenario=ordered(Scenario,c("Brazil","India","China","USA","Mitigation + SAI","Mitigation","Free-riding")),
                zinj=ordered(zinj,c("symmetric","sovereign","free","no SRM"))) %>%
         filter(ttoyear(t)==2100 & valuerel_saicoop!=0) %>% 
         inner_join(pop %>% rename(pop=value)) %>%
         inner_join(countries_map) )+
  geom_hline(yintercept=0,linetype=1,color="grey") +
  geom_errorbar(data=.%>% 
                  group_by(file,Scenario,zinj,t) %>%
                  summarise(max=ggdist::weighted_quantile(valuerel_saicoop,0.95,pop),
                            sdup=ggdist::weighted_quantile(valuerel_saicoop,0.66,pop),
                            sddo=ggdist::weighted_quantile(valuerel_saicoop,0.33,pop),
                            min=ggdist::weighted_quantile(valuerel_saicoop,0.05,pop)),
                aes(x=zinj,
                    ymin=sddo,
                    ymax=sdup,
                    color=Scenario, 
                    group=interaction(zinj,Scenario) ),
                width=0.4,position=position_dodge(width=0.5)) +
  geom_errorbar(data=.%>%
                  group_by(file,Scenario,zinj,t) %>%
                  summarise(max=ggdist::weighted_quantile(valuerel_saicoop,0.95,pop),
                            sdup=ggdist::weighted_quantile(valuerel_saicoop,0.66,pop),
                            sddo=ggdist::weighted_quantile(valuerel_saicoop,0.33,pop),
                            min=ggdist::weighted_quantile(valuerel_saicoop,0.05,pop)),
                aes(x=zinj ,
                    ymin=min,
                    ymax=max,
                    color=Scenario, 
                    group=interaction(zinj,Scenario) ),
                width=0.1,position=position_dodge(width=0.5),alpha=0.2) +
  geom_point(data=. %>% 
               group_by(file,Scenario,zinj,t) %>%
               summarise(med=ggdist::weighted_quantile(valuerel_saicoop,0.5,pop)) ,
             aes(x=zinj,
                 y=med,
                 fill=Scenario,
                 group=interaction(zinj,Scenario)),
             color="black",
             size=2,shape=21,position=position_dodge(width=0.5)) +
  geom_point(aes(x=zinj,
                 y=valuerel_saicoop,
                 color=Scenario,
                 group=interaction(zinj,Scenario)),
             size=1,position=position_dodge(width=0.5),alpha=0.5,shape=108) +
  geom_point(data=gdploss_g %>%
               inner_join(main_scenarios_noncoop) %>% 
               mutate(zinj=ifelse(nsrm=="no SRM", "no SRM", zinj),
                      Scenario=ordered(Scenario,c("Brazil","India","China","USA","Mitigation + SAI","Mitigation","Free-riding")),
                      zinj=ordered(zinj,c("symmetric","sovereign","free","no SRM"))) %>% 
               filter(ttoyear(t)==2100& valuerel_saicoop!=0),
             aes(x=zinj,
                 y=valuerel_saicoop,
                 fill=Scenario,
                 group=interaction(zinj,Scenario)),
             color="black",
             size=1,shape=23,position=position_dodge(width=0.5)) +
  scale_color_manual(values=regpalette_srm) +
  scale_fill_manual(values=regpalette_srm) +  
  scale_y_continuous(limits = c(-0.1,0.4)) + 
  ylab('') + xlab('') + coord_flip()
ggsave("fig_injection.png",plot=fig_inj,width=12, height=8, units="cm")

ggplot(Z_SAI %>% 
         inner_join(main_scenarios_noncoop) %>% 
         mutate(zinj=ifelse(nsrm=="no SRM", "no SRM", zinj),
                Scenario=ordered(Scenario,c("Brazil","India","China","USA","Mitigation + SAI","Mitigation","Free-riding")),
                zinj=ordered(zinj,c("symmetric","sovereign","free","no SRM"))) %>%
         filter(ttoyear(t)==2100 & value!=0))+ 
  geom_point(aes(x=zinj, 
                 y=injton(inj), 
                 size= value,
                 color=Scenario,
                 shape=zinj,
                 group=interaction(zinj,Scenario)),
             position=position_dodge(width=0.5),shape=21,fill=NA) +
  scale_color_manual(values=regpalette_srm) +
  scale_fill_manual(values=regpalette_srm)
