
perc_impact <- IMPACT %>% 
  filter(ttoyear(t) %in% c(2090,2095)) %>%
  pivot_wider(names_from=d) %>%
  group_by(t,n) %>%
  mutate(temp = temp - temp[file=="coop_maxiso3_SRMusa_INJfree_POLcba_IMPT10modified_IMPP50modified_TSPR1"],
         prec = prec - prec[file=="coop_maxiso3_SRMusa_INJfree_POLcba_IMPT10modified_IMPP50modified_TSPR1"]) %>%
  group_by(n,file) %>%
  mutate(perc = sum(temp)/(prec[ttoyear(t)==2095]+sum(temp)) ) %>%
  select(n,file,perc)

v1 <- DAMFRAC %>% 
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
  geom_bar(aes(x=interaction(ordered(nsrm,c("USA","China","India","Brazil")), latitude ),
               y=value,
               fill=latitude,
               alpha=name),
           color="black",
           stat="identity",
           position="stack") +
  geom_errorbar(aes(x=interaction(ordered(nsrm,c("USA","China","India","Brazil")), latitude ), ymin=min, ymax=max,group=latitude), width=0.25) +
coord_flip()+ 
  xlab("") + ylab("%")+
  scale_shape_manual(values=c(21,22)) +
  scale_color_manual(values=c("black","red")) +
  theme_pubr() +
  theme(text = element_text(size = 10)) +
  scale_x_discrete(guide = ggh4x::guide_axis_nested(delim="."))
ggsave("fig_nonccop1.png",v1,width=8.8,height=6)

ntosrm <- c("ind"="India","usa"="USA","bra"="Brazil","chn"="China")

v2 <- DAMFRAC %>% 
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




gdploss %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100) %>%
  inner_join(countries_map) %>%
  group_by(n) %>%
  mutate(valueerel=(value-value[nsrm=="Cooperative" & COOP=="coop"])/(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop" & pimp=="5"])) %>%
  filter(pimp==5) %>%
  inner_join(sanitized_names) %>% 
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  filter(!Scenario %in% c("Optimal, no SAI","Optimal","Free-riding") ) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize(-valueerel*100,method="fixed",breaks=c(-10000000,0,100000000),labels=c("Unilateral","Cooperation") )) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),color='black',size=.1) +
  scale_fill_manual(name="Preferred scenario",values=c("#F46D43","#74ADD1")) +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white")) +
  facet_wrap(ordered(Scenario,c("USA","China","India","Brazil"))~.,)


######### 
abatefrac <- get_witch("ABATECOST") %>%
  inner_join(YGROSS %>% rename(ykali=value)) %>%
  mutate(value = value/ykali,source="ab") %>%
  filter(ttoyear(t)==2100) %>%
  select(file,n,value,source,t) 

perc_impact2 <- IMPACT %>% 
  filter(ttoyear(t) %in% c(2090,2095)) %>%
  mutate(value=-value) %>%
  pivot_wider(names_from=d) %>%
  group_by(n,file) %>%
  summarise(temp = sum(temp),
            prec = prec[ttoyear(t)==2095]) %>%
  pivot_longer(c(temp,prec),names_to="source") %>%
  mutate(t=18) %>%
  bind_rows(abatefrac) %>%
  group_by(file,n,t) %>%
  mutate(perc=value/sum(value))

figa <- gdploss %>%  
  inner_join(sanitized_names) %>%
  inner_join(pop %>% rename(pop=value) ) %>%
  filter(ttoyear(t)==2100 & pimp==5) %>% 
  inner_join(countries_map) %>% 
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  filter(!Scenario %in% c("Free-riding","Optimal","Optimal, no SAI") ) %>%
  ggplot() + 
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  stat_smooth(data=gdploss %>%  
                inner_join(sanitized_names) %>%
                inner_join(pop %>% rename(pop=value) ) %>%
                filter(ttoyear(t)==2100 & pimp==5 & nsrm=="Cooperative" & COOP=="coop") %>% 
                inner_join(countries_map),
              aes(x=meanlat,#ykali/pop*1e6,
                  y=value*100,
                  weight=pop),
              se = FALSE,
              linewidth=2,
              color="grey") +
  stat_smooth(data=gdploss %>%  
                inner_join(sanitized_names) %>%
                inner_join(pop %>% rename(pop=value) ) %>%
                filter(ttoyear(t)==2100 & pimp==5 & nsrm=="no SRM" & COOP=="coop") %>% 
                inner_join(countries_map),
              aes(x=meanlat,#ykali/pop*1e6,
                  y=value*100,
                  weight=pop),
              se = FALSE,
              linewidth=2,
              color="grey",
              linetype=2) +
  geom_point(aes(x=meanlat,#ykali/pop*1e6,
                  y=value*100,
                  color=latitude)) +
  stat_smooth(aes(x=meanlat,#ykali/pop*1e6,
                   y=value*100,
                   group=Scenario,
                   weight=pop), 
               se = FALSE,
               linewidth=2,
              color="black") +
  theme_pubr() + 
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("GDP loss [%]") + theme(legend.position = "none",
                               text=element_text(size=7)) +
#  scale_color_manual(values=regpalette_scenarios) +   
#  scale_fill_manual(values=regpalette_scenarios) +
  facet_wrap(ordered(Scenario,c("USA","China","India","Brazil"))~.,nrow=1)

figb <- gdploss %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp==5) %>%
  inner_join(pop %>% rename(pop=value)) %>%
  inner_join(perc_impact2 %>% select(-value)) %>%
  inner_join(countries_map) %>%
  group_by(n,source) %>%
  mutate(valueerel=-(perc*value-perc[nsrm=="Cooperative" & COOP=="coop"]*value[nsrm=="Cooperative" & COOP=="coop"]) /(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop" & pimp=="5"])) %>%
  filter(!nsrm %in% c("no SRM","Cooperative") ) %>%
  ggplot() +
  geom_hline(yintercept=100) +
  geom_bar(data=. %>% group_by(latitude,file,t,source) %>%
             summarise(med=quantile(valueerel*100,0.5) ) %>%
           inner_join(sanitized_names),
           aes(x=interaction(ordered(nsrm,c("USA","China","India","Brazil")), latitude ),
               y=med,
               fill=latitude,
               alpha=source),
           color="black",
           stat="identity",
           position="stack") +
    geom_errorbar(data=.%>% 
                    group_by(latitude,file,t,n) %>%
                    summarise(value=sum(valueerel*100),
                              pop=sum(pop) ) %>%
                    group_by(latitude,file,t) %>%
                    summarise(max=quantile(value,0.75),
                              min=quantile(value,0.25))%>%
                    inner_join(sanitized_names),
                  aes(x=interaction(ordered(nsrm,c("USA","China","India","Brazil")), latitude ), 
                      ymin=min, 
                      ymax=max,
                      group=latitude), 
                  width=0.25) +
  geom_point(data=.%>% 
                  group_by(latitude,file,t,n) %>%
                  summarise(value=sum(valueerel*100),
                            pop=sum(pop) ) %>%
                  group_by(latitude,file,t) %>%
                  summarise(med=quantile(value,0.5)) %>%
                  inner_join(sanitized_names),
                aes(x=interaction(ordered(nsrm,c("USA","China","India","Brazil")), latitude ), 
                    y=med,
                    group=latitude), 
             size=2,color="black") +
  coord_flip()+
  facet_wrap(pimp~.,nrow=1,labeller=as_labeller(c("1"="Low precipitation impacts","5"="High precipitation impacts"))) +
  xlab("") + ylab("%")+
  scale_alpha_manual(labels=c("Mitigation","Precipitations","Temperature"),
                     values=c(0.1,0.5,1),
                     name="Source") +
  guides(fill="none") +
  theme_pubr() +
  theme(text = element_text(size = 7)) +
  scale_x_discrete(guide = ggh4x::guide_axis_nested(delim="."))
ggsave("Fig_noncoop.png",figb,width=8.8,height=5)

ggarrange(figa,figb,heights=c(1,0.8),nrow=2)


map <- countries_map %>%
  #  mutate(latitude=ifelse(n=="row","ROW",latitude)) %>%
  inner_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = latitude),color='black',size=.1) +
  #  scale_fill_manual(values=rev(c("#8B0000","#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#92C5DE", "#4393C3", "#2166AC"))) + 
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white"))
ggsave("map.png",map,width=8.8,heigh=7)

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
