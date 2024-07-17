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

gdploss_barplot <- gdploss %>% 
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
#  facet_wrap(pimp~.,nrow=1,labeller=as_labeller(c("1"="Low precipitation impacts","5"="High precipitation impacts"))) +
  xlab("") + ylab("%")+
  scale_alpha_manual(labels=c("Mitigation","Precipitations","Temperature"),
                     values=c(0.1,0.5,1),
                     name="Source") +
  guides(fill="none") +
  theme_pubr() +
  theme(text = element_text(size = 7)) +
  scale_x_discrete(guide = ggh4x::guide_axis_nested(delim="."))


map <- countries_map %>%
#  mutate(latitude=ifelse(n=="row",NA,latitude)) %>%
  inner_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = latitude),color='black',size=.1) +
  #  scale_fill_manual(values=rev(c("#8B0000","#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#92C5DE", "#4393C3", "#2166AC"))) + 
  scale_fill_viridis_d(na.value = "grey50") +
  theme_void() + theme(legend.position="none")

#cowplot::ggdraw(gdploss_barplot) + cowplot::draw_plot(plot=map,x=.7,y=.6,width=.3,height=.2) 

damages_maps <- gdploss %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp==5) %>%
  inner_join(pop %>% rename(pop=value)) %>%
  inner_join(countries_map) %>%
  group_by(n) %>%
  mutate(valueerel=-100*(value-value[nsrm=="Cooperative" & COOP=="coop"]) /(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop" & pimp=="5"])) %>%
  filter(!nsrm %in% c("no SRM","Cooperative") ) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize(valueerel,method="fixed",
                                 breaks=c(-10000000,0,100,1000000000),
                                 labels=c("Unilateral implementation","Cooperative implementation","Mitigation"))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),color='black',size=.1) +
  scale_fill_manual(values=c("#4575B4","white","#a2231D"),name="Better-off with") +
  #scale_fill_gradient2() +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_wrap(ordered(nsrm,c("Brazil","India","China","USA"))~.,nrow=1)

fig_noncoop <- ggarrange(damages_maps,gdploss_barplot,heights=c(4,5),nrow=2)
ggsave("Fig_noncoop.png",fig_noncoop,width=18,height=14)
