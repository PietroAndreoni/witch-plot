main_scenarios_noncoop <- sanitized_names %>% filter(impacts==imp_select) 


fig_inj <- ggplot(gdploss %>% 
         inner_join(main_scenarios_noncoop) %>% 
         mutate(zinj=ifelse(nsrm=="no SRM", "no SRM", zinj),
                Scenario=ordered(Scenario,c("Brazil","India","China","USA","Mitigation + SAI","Mitigation","Free-riding")),
                zinj=ordered(zinj,c("symmetric","sovereign","free","no SRM"))) %>%
         filter(ttoyear(t)==2100) %>% 
         inner_join(pop %>% rename(pop=value)) %>%
         inner_join(countries_map) )+
  geom_hline(yintercept=0,linetype=1,color="grey") +
  geom_errorbar(data=.%>% 
                  group_by(file,Scenario,zinj,t) %>%
                  summarise(max=ggdist::weighted_quantile(value,0.95,pop),
                            sdup=ggdist::weighted_quantile(value,0.66,pop),
                            sddo=ggdist::weighted_quantile(value,0.33,pop),
                            min=ggdist::weighted_quantile(value,0.05,pop)),
                aes(x=zinj,
                    ymin=sddo,
                    ymax=sdup,
                    color=Scenario, 
                    group=interaction(zinj,Scenario) ),
                width=0.4,position=position_dodge(width=0.5)) +
  geom_errorbar(data=.%>%
                  group_by(file,Scenario,zinj,t) %>%
                  summarise(max=ggdist::weighted_quantile(value,0.95,pop),
                            sdup=ggdist::weighted_quantile(value,0.66,pop),
                            sddo=ggdist::weighted_quantile(value,0.33,pop),
                            min=ggdist::weighted_quantile(value,0.05,pop)),
                aes(x=zinj ,
                    ymin=min,
                    ymax=max,
                    color=Scenario, 
                    group=interaction(zinj,Scenario) ),
                width=0.1,position=position_dodge(width=0.5),alpha=0.2) +
  geom_point(data=. %>% 
               group_by(file,Scenario,zinj,t) %>%
               summarise(med=ggdist::weighted_quantile(value,0.5,pop)) ,
             aes(x=zinj,
                 y=med,
                 fill=Scenario,
                 group=interaction(zinj,Scenario)),
             color="black",
             size=2,shape=21,position=position_dodge(width=0.5)) +
  geom_point(aes(x=zinj,
                 y=value,
                 color=Scenario,
                 group=interaction(zinj,Scenario)),
             size=1,position=position_dodge(width=0.5),alpha=0.5,shape=108) +
  geom_point(data=gdploss_g %>%
               inner_join(main_scenarios_noncoop) %>% 
               mutate(zinj=ifelse(nsrm=="no SRM", "no SRM", zinj),
                      Scenario=ordered(Scenario,c("Brazil","India","China","USA","Mitigation + SAI","Mitigation","Free-riding")),
                      zinj=ordered(zinj,c("symmetric","sovereign","free","no SRM"))) %>% 
               filter(ttoyear(t)==2100),
             aes(x=zinj,
                 y=value,
                 fill=Scenario,
                 group=interaction(zinj,Scenario)),
             color="black",
             size=1,shape=23,position=position_dodge(width=0.5)) +
  scale_color_manual(values=regpalette_srm) +
  scale_fill_manual(values=regpalette_srm) +  
  scale_y_continuous(limits = c(-0.1,0.6)) + 
  ylab('') + xlab('') + coord_flip()

impacts_by_type <- get_witch("damfrac_type") %>%
  mutate(source=case_when(str_detect(d,"temp")~"Temperature impacts",
                          str_detect(d,"prec")~"Precipitation impacts",
                          str_detect(d,"spill")~"Spillover effects",
                          .default="others")) %>%
  bind_rows(abatefrac) %>%
  bind_rows(saifrac) %>%
  mutate(source=ifelse(source %in% c("ab","sai"), "Climate policy (incl. SAI)", source )) %>% 
  group_by(t,n,file,source) %>% 
  summarise(value=sum(value)) %>% 
#  inner_join(YGROSS %>% rename(ykali=value)) %>%
#  group_by(t,n,file,source) %>%
#  summarise(value=sum(value*ykali),ykali=mean(ykali)) %>%
#  ungroup() %>% mutate(value=value/ykali) %>% 
  inner_join(countries_map) %>% 
  inner_join(pop %>% rename(pop=value)) 

fig_injbytype <- ggplot(impacts_by_type %>% 
         inner_join(main_scenarios_noncoop) %>% 
         group_by_at(c("t","n","source",setdiff(colnames(sanitized_names),c("nsrm","COOP","Scenario","file","pathdir","zinj"))) ) %>%
         mutate(valuerel_saicoop=(value-value[nsrm=="Cooperative" & COOP=="coop" & zinj=="free"])) %>% 
         mutate(zinj=ifelse(nsrm=="no SRM", "no SRM", zinj),
                Scenario=ordered(Scenario,c("Brazil","India","China","USA","Mitigation + SAI","Mitigation","Free-riding")),
                zinj=ordered(zinj,c("symmetric","sovereign","free","no SRM"))) %>%
         filter(ttoyear(t)==2100 & source != "Climate policy (incl. SAI)") )+
  geom_hline(yintercept=0,linetype=1,color="grey") +
  geom_errorbar(data=.%>% 
                  group_by(file,Scenario,zinj,source,t) %>%
                  summarise(max=ggdist::weighted_quantile(value,0.95,pop),
                            sdup=ggdist::weighted_quantile(value,0.66,pop),
                            sddo=ggdist::weighted_quantile(value,0.33,pop),
                            min=ggdist::weighted_quantile(value,0.05,pop)),
                aes(x=zinj,
                    ymin=sddo,
                    ymax=sdup,
                    color=Scenario, 
                    group=interaction(zinj,Scenario) ),
                width=0.4,position=position_dodge(width=0.5)) +
  geom_errorbar(data=.%>%
                  group_by(file,Scenario,zinj,source,t) %>%
                  summarise(max=ggdist::weighted_quantile(value,0.95,pop),
                            sdup=ggdist::weighted_quantile(value,0.66,pop),
                            sddo=ggdist::weighted_quantile(value,0.33,pop),
                            min=ggdist::weighted_quantile(value,0.05,pop)),
                aes(x=zinj ,
                    ymin=min,
                    ymax=max,
                    color=Scenario, 
                    group=interaction(zinj,Scenario) ),
                width=0.1,position=position_dodge(width=0.5),alpha=0.2) +
  geom_point(data=. %>% 
               group_by(file,Scenario,zinj,source,t) %>%
               summarise(med=ggdist::weighted_quantile(value,0.5,pop)) ,
             aes(x=zinj,
                 y=med,
                 fill=Scenario,
                 group=interaction(zinj,Scenario)),
             color="black",
             size=2,shape=21,position=position_dodge(width=0.5)) +
  geom_point(aes(x=zinj,
                 y=value,
                 color=Scenario,
                 group=interaction(zinj,Scenario)),
             size=1,position=position_dodge(width=0.5),alpha=0.5,shape=108) +
  scale_color_manual(values=regpalette_srm) +
  scale_fill_manual(values=regpalette_srm) +  
  scale_y_continuous(limits = c(-0.1,0.6)) + 
  facet_wrap(source~.,) +
  ylab('') + xlab('') + coord_flip()

strategy <- ggplot(Z_SAI %>% 
         inner_join(main_scenarios_noncoop) %>% 
         mutate(zinj=ifelse(nsrm=="no SRM", "no SRM", zinj),
                Scenario=ordered(Scenario,c("Brazil","India","China","USA","Mitigation + SAI","Mitigation","Free-riding")),
                zinj=ordered(zinj,c("symmetric","sovereign","free","no SRM")),
                inj=ordered(inj,c("60S","45S","30S","15S","0","15N","30N","45N","60N")) ) %>%
         filter(ttoyear(t)==2100 & value!=0))+ 
  geom_point(aes(x=Scenario, 
                 y=inj, 
                 size= value,
                 color=Scenario,
                 shape=zinj,
                 group=interaction(zinj,Scenario)),
             position=position_dodge(width=1),shape=21,fill=NA) +
  geom_vline(aes(xintercept=Scenario),color="grey80") +
  scale_color_manual(values=regpalette_srm) +
  scale_fill_manual(values=regpalette_srm) +
  labs(size="Injection [TgS/yr]") +
  guides(color="none") +
  facet_wrap(zinj~.,) + 
  theme(axis.text.x = element_blank()) +
  ylab('') + xlab('') + theme()

ggsave("fig_injection.png",plot=strategy / fig_inj / fig_injbytype + theme(legend.position = "none") + plot_layout(widths = c(0.7, 1.5, 1)),width=18, height=18*1.2, units="cm", dpi=400)
