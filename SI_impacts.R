files <- list.files("../Results_final/Impacts",pattern=".gdx")
y_impacts <- batch_extract(c("Y"),paste0("../Results_final/Impacts/",files ) )$Y %>%
  mutate(file=str_remove_all(gdx,"../Results_final/Impacts/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble()
ygross_impacts <- batch_extract(c("ykali"),paste0("../Results_final/Impacts/",files ) )$ykali %>%
  mutate(file=str_remove_all(gdx,"../Results_final/Impacts/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble() 

sanitized_names_imp <- y_impacts %>% select(file) %>% unique() %>% sanitize()
gdploss_imp <- y_impacts %>%
  full_join(ygross_impacts %>% rename(ykali=value)) %>%
  mutate(value=(ykali-value)/ykali )  %>%
  inner_join(sanitized_names_imp) %>%
  filter(ttoyear(t)==2100) %>%
  group_by(t,n,impacts,ci_imp) %>% 
  mutate(valuerel=(value-value[nsrm=="Cooperative" & COOP=="coop"])/(value[nsrm=="no SRM" & COOP=="coop"]-value[nsrm=="Cooperative" & COOP=="coop"]) )

z_sai_impacts <- gdxtools::batch_extract("Z_SAI",
                                         files=paste0("../Results_final/Impacts/",list.files(path="../Results_final/Impacts",pattern="results_")))$Z_SAI %>%
  mutate(file=str_remove_all(gdx,"../Results_final/Impacts/|.gdx"),
         t=as.numeric(t)) %>% select(-gdx) %>% as_tibble()

preferred_position <-  gdploss_imp %>% 
  inner_join(sanitized_names_imp) %>%
  filter(ttoyear(t)==2100) %>%
  inner_join(countries_map) %>%  group_by_at(c("t","n",setdiff(colnames(sanitized_names),c("nsrm","COOP","Scenario","file","pathdir","zinj"))) ) %>%
  mutate(betteroff_nash=(value-value[nsrm=="no SRM" & COOP=="noncoop" & zinj=="free"]),
         betteroff_paris=(value-value[nsrm=="no SRM" & COOP=="coop" & zinj=="free"]),
         betteroff_coop=(value-value[nsrm=="Cooperative" & COOP=="coop" & zinj=="free"]) ) %>%
  mutate(disc = case_when(betteroff_coop < 0 & betteroff_paris < 0  ~ "Laissez-faire",
                          betteroff_coop > 0 & betteroff_paris < 0 ~ "Push to cooperation",
                          betteroff_paris > 0 & betteroff_nash < 0 ~ "Non-use",
                          betteroff_paris > 0 & betteroff_nash > 0 ~ "Non-use (strong)")) %>%
  filter(!nsrm %in% c("no SRM","Cooperative")) %>%
  mutate(disc=ifelse(n=="row","NA",as.character(disc) )) 

imp_strategy <- ggplot() +
  geom_point(data=z_sai_impacts %>% 
               inner_join(sanitized_names_imp) %>%
               filter(t==18 & value>0) %>%  
               mutate(ci_imp=ordered(ci_imp,c("lo","mlo","best","mhi","hi")),
                      inj=ordered(inj,c("60S","45S","30S","15S","0","15N","30N","45N","60N"))) %>% 
               filter(Scenario=="Mitigation + SAI") %>% select(-Scenario,-nsrm),
             aes(x=ci_imp,
                 y=inj,
                 size=value), shape=21, color="grey80", fill=NA ) +
  # geom_text(data=z_sai_impacts %>% 
  #             inner_join(sanitized_names_imp) %>%
  #             filter(t==18 & value>0) %>%  
  #             mutate(ci_imp=ordered(ci_imp,c("lo","mlo","best","mhi","hi")),
  #                    inj=ordered(inj,c("60S","45S","30S","15S","0","15N","30N","45N","60N"))) %>% 
  #             filter(Scenario=="Mitigation + SAI") %>% select(-Scenario,-nsrm),
  #           aes(x=ci_imp,
  #               y=inj,
  #               label=str_replace_all(as.character(round(value,0)), " ", "") ), 
  #           hjust=-2, 
  #           size=2.5, 
  #           color="grey80") +
  geom_point(data=z_sai_impacts %>% 
               inner_join(sanitized_names_imp) %>%
               filter(t==18 & value>0) %>%  
               mutate(ci_imp=ordered(ci_imp,c("lo","mlo","best","mhi","hi")),
                      inj=ordered(inj,c("60S","45S","30S","15S","0","15N","30N","45N","60N"))) %>% 
               filter(Scenario!="Mitigation + SAI"),
             aes(x=ci_imp,
                 y=inj,
                 size=value, color=Scenario), shape=21, fill=NA ) +
  # geom_text(data=z_sai_impacts %>% 
  #             inner_join(sanitized_names_imp) %>%
  #             filter(t==18 & value>0) %>%  
  #             mutate(ci_imp=ordered(ci_imp,c("lo","mlo","best","mhi","hi")),
  #                    inj=ordered(inj,c("60S","45S","30S","15S","0","15N","30N","45N","60N"))) %>% 
  #             filter(Scenario!="Mitigation + SAI"),
  #           aes(x=ci_imp,
  #               y=inj,
  #               label=str_replace_all(as.character(round(value,0)), " ", ""), 
  #               color=Scenario), 
  #           hjust=2, 
  #           size=2.5 ) +
  scale_alpha_manual(values=c("mlo"=0.7,"mhi"=0.7,"best"=1)) +
  facet_grid(Scenario~impacts,)+
  scale_color_manual(values=regpalette_srm) +
  scale_fill_manual(values=regpalette_srm) +  
  labs(size="Injection [TgS/yr]") +
  guides(text="none",color="none",fill="none") +
  ylab('') + xlab('') +
  theme(text = element_text(size = 7),    
        panel.border = element_rect(color = "black", fill = NA, linewidth = 0.5))



fig_inj <- ggplot(gdploss_imp %>% 
                    filter(t==18) %>%  
                    mutate(ci_imp=ordered(ci_imp,c("lo","mlo","best","mhi","hi")),
                           Scenario=ordered(Scenario,c("Brazil","India","China","USA","Mitigation + SAI","Mitigation","Free-riding"))) %>% 
                    inner_join(pop %>% select(t,n,value) %>% rename(pop=value)%>% unique()) %>% 
                    inner_join(countries_map) )+
  geom_hline(yintercept=0,linetype=1,color="grey") +
  geom_errorbar(data=.%>% 
                  group_by(file,Scenario,impacts,ci_imp,t) %>%
                  summarise(max=ggdist::weighted_quantile(value,0.95,pop),
                            sdup=ggdist::weighted_quantile(value,0.66,pop),
                            sddo=ggdist::weighted_quantile(value,0.33,pop),
                            min=ggdist::weighted_quantile(value,0.05,pop)),
                aes(x=ci_imp,
                    ymin=sddo,
                    ymax=sdup,
                    color=Scenario, 
                    group=interaction(impacts,ci_imp,Scenario) ),
                width=0.4,position=position_dodge(width=0.8)) +
  geom_errorbar(data=.%>%
                  group_by(file,Scenario,impacts,ci_imp,t) %>%
                  summarise(max=ggdist::weighted_quantile(value,0.95,pop),
                            sdup=ggdist::weighted_quantile(value,0.66,pop),
                            sddo=ggdist::weighted_quantile(value,0.33,pop),
                            min=ggdist::weighted_quantile(value,0.05,pop)),
                aes(x=ci_imp,
                    ymin=min,
                    ymax=max,
                    color=Scenario, 
                    group=interaction(impacts,ci_imp,Scenario) ),
                width=0.1,position=position_dodge(width=0.8),alpha=0.2) +
  geom_point(data=. %>% 
               group_by(file,Scenario,impacts,ci_imp,t) %>%
               summarise(med=ggdist::weighted_quantile(value,0.5,pop)) ,
             aes(x=ci_imp,
                 y=med,
                 fill=Scenario,
                 group=interaction(impacts,ci_imp,Scenario)),
             color="black",
             size=2,shape=21,position=position_dodge(width=0.8)) +
  geom_point(aes(x=ci_imp,
                 y=value,
                 color=Scenario,
                 group=interaction(impacts,ci_imp,Scenario)),
             size=1,position=position_dodge(width=0.8),alpha=0.5,shape=108) +
  scale_color_manual(values=regpalette_srm) +
  scale_fill_manual(values=regpalette_srm) +  
  scale_y_continuous(limits = c(-1,1)) + 
  facet_wrap(impacts~.,ncol=1) +
  ylab('') + xlab('') + coord_flip()

ggsave("fig_SIinjection.png",plot=imp_strategy,width=18, height=16, units="cm", dpi=400)
ggsave("fig_SIwelfare.png",plot=fig_inj,width=18, height=22, units="cm", dpi=400)
