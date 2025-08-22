files <- list.files("../Results_secondround/Persistency",pattern=".gdx")
tatm_pers <- batch_extract(c("TATM"),paste0("../Results_secondround/Persistency/",files ) )$TATM %>%
  mutate(file=str_remove_all(gdx,"../Results_secondround/Persistency/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble()
tatmghg_pers <- batch_extract(c("TATM_GHG"),paste0("../Results_secondround/Persistency/",files ) )$TATM_GHG %>%
  mutate(file=str_remove_all(gdx,"../Results_secondround/Persistency/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble() 
zsai_pers <- batch_extract(c("Z_SAI"),paste0("../Results_secondround/Persistency/",files ) )$Z_SAI %>%
  mutate(file=str_remove_all(gdx,"../Results_secondround/Persistency/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble() 
y_pers <- batch_extract(c("Y"),paste0("../Results_secondround/Persistency/",files ) )$Y %>%
  mutate(file=str_remove_all(gdx,"../Results_secondround/Persistency/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble()
ygross_pers <- batch_extract(c("YGROSS"),paste0("../Results_secondround/Persistency/",files ) )$YGROSS %>%
  mutate(file=str_remove_all(gdx,"../Results_secondround/Persistency/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble() 

abemi_pers <- batch_extract(c("ABATEDEMI"),paste0("../Results_secondround/Persistency/",files ) )$ABATEDEMI %>%
  mutate(file=str_remove_all(gdx,"../Results_secondround/Persistency/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble() 

sanitized_names_pers <- tatm_pers %>% select(file) %>% unique() %>% sanitize() %>% filter(!(nsrm=="no SRM" & COOP=="noncoop"))

gdplossg_pers <- y_pers %>%
  full_join(ygross_pers %>% rename(ykali=value)) %>%
  group_by(t,file) %>%
  summarise(damfrac= ( sum(ykali)-sum(value) )/sum(ykali) )  %>%
  inner_join(sanitized_names_pers) 

inner_join(tatm_pers,
           tatmghg_pers %>% rename(tatmghg=value)) %>%
  inner_join(sanitized_names_pers) %>%  
  inner_join(gdplossg_pers %>% filter(COOP=="coop" & nsrm=="no SRM") %>% select(t,damfrac,impacts,pers_p,pers_t)) %>%
  group_by(t,pers_t,pers_p,impacts) %>%
  mutate(sub=tatmghg-value[COOP=="coop" & nsrm=="no SRM"],
         other=value[COOP=="coop" & nsrm=="no SRM"]-value,
         pers_p=paste0("x ",as.character(as.numeric(pers_p)/as.numeric(pers_t)))) %>%
  filter(ttoyear(t)==2100 & nsrm!="no SRM") %>% mutate(pers_p=ifelse(pers_p=="x NaN","x 1",pers_p)) %>%
  ggplot() +
  geom_point(aes(x=pers_t,color=pers_p,y=sub,shape=impacts)) +
  geom_line(aes(x=pers_t,y=sub,group=interaction(impacts,pers_p) )) +
  ggrepel::geom_text_repel(aes(x=pers_t,color=pers_p,y=sub,label=paste0(round(damfrac*100,1),"%"))) +
  geom_point(aes(x=pers_t,color=pers_p,y=-other,shape=impacts)) +
  geom_line(aes(x=pers_t,y=-other,group=interaction(impacts,pers_p) )) 
  
  

injections_pers <- zsai_pers %>% 
  inner_join(sanitized_names_pers) %>%
  filter(t==18 & Scenario!="Mitigation" & value>0) %>%
  group_by(t,Scenario,impacts,pers_p,pers_t) %>% 
  summarise(bar=weighted.mean(injton(inj),value), sd=sqrt(Hmisc::wtd.var(injton(inj),value)), ninj=n()  ) 

ggplot(injections_pers %>%
       mutate(pers_p=paste0("x ",as.character(as.numeric(pers_p)/as.numeric(pers_t)))) %>%
  filter(ttoyear(t)==2100) %>% mutate(pers_p=ifelse(pers_p=="x NaN","x 1",pers_p))) +
  geom_point(aes(x=pers_t,y=bar,color=pers_p,shape=impacts))
  
