main_scenarios_noncoop <- sanitized_names %>% filter(impacts!='bhm' & (zinj=="sovereign" & !nsrm %in% c("Cooperative","no SRM")) | ((zinj=="free" & nsrm %in% c("Cooperative","no SRM")))  )


ggplot(gdploss %>%  
#         inner_join(gdploss_g %>% rename(gvalue=value))  %>% 
#         group_by_at(c("t","n",setdiff(colnames(sanitized_names),c("nsrm","COOP","Scenario","file","pathdir","zinj"))) ) %>%
#         mutate(valuerel_saicoop=valuerel_saicoop/gvalue[nsrm=="no SRM" & COOP=="coop" & zinj=="free"]) %>% 
         filter(ttoyear(t)==2100 & !Scenario %in% c("Mitigation","Free-riding") ) %>% 
         filter(!is.na(valuerel_saicoop)) %>% 
         inner_join(pop %>% rename(pop=value)) %>%
         inner_join(countries_map) )+
  geom_hline(yintercept=0,linetype=1,color="grey") +
#  geom_hline(yintercept=1,linetype=3,color="grey") +
  # geom_hline(data=.%>%filter(Scenario=="Free-riding"),aes(yintercept=valuerel,color=Scenario)) +
  geom_errorbar(data=.%>% 
                  group_by(file,t) %>%
                  summarise(max=ggdist::weighted_quantile(valuerel_saicoop,0.95,pop),
                            sdup=ggdist::weighted_quantile(valuerel_saicoop,0.66,pop),
                            sddo=ggdist::weighted_quantile(valuerel_saicoop,0.33,pop),
                            min=ggdist::weighted_quantile(valuerel_saicoop,0.05,pop)) %>%
                  inner_join(sanitized_names),
                aes(x=interaction(impacts,zinj),
                    ymin=sddo,
                    ymax=sdup,
                    color=Scenario, linetype=zinj ),
                width=0.4,position=position_dodge(width=0.5)) +
  geom_errorbar(data=.%>%
                  group_by(file,t) %>%
                  summarise(max=ggdist::weighted_quantile(valuerel_saicoop,0.95,pop),
                            sdup=ggdist::weighted_quantile(valuerel_saicoop,0.66,pop),
                            sddo=ggdist::weighted_quantile(valuerel_saicoop,0.33,pop),
                            min=ggdist::weighted_quantile(valuerel_saicoop,0.05,pop)) %>%
                  inner_join(sanitized_names),
                aes(x=interaction(impacts,zinj) ,
                    ymin=min,
                    ymax=max,
                    color=Scenario, 
                    linetype=zinj ),
                width=0.1,position=position_dodge(width=0.5),alpha=0.2) +
  geom_point(data=. %>% 
               group_by(file,t) %>%
               summarise(med=ggdist::weighted_quantile(valuerel_saicoop,0.5,pop)) %>%
               inner_join(sanitized_names),
             aes(x=interaction(impacts,zinj),
                 y=med,fill=Scenario,shape=zinj),
             color="black",
             size=2,position=position_dodge(width=0.5)) +
  # geom_point(data=gdploss_g %>%        
  #              filter(ttoyear(t)==2100 & !Scenario %in% c("Mitigation","Mitigation + SAI","Free-riding")),
  #            aes(x=impacts,
  #                y=valuerel,fill=Scenario),
  #            color="black",
  #            size=2,shape=22,position=position_dodge(width=0.5)) +
  #  facet_wrap(Scenario~.,) +
  scale_shape_manual(values=c(21,22,23)) +
  scale_color_manual(values=regpalette_srm) +
  scale_fill_manual(values=regpalette_srm) +
  ylab('') + xlab(' (SCEN - COOP)/(MITIGATION - COOP)') + coord_flip() 
