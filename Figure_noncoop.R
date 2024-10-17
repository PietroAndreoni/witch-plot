precipitation_sel <- 0.2
tspread_sel <- 1

abatefrac <- get_witch("ABATECOST") %>%
  inner_join(YGROSS %>% rename(ykali=value)) %>%
  mutate(value = value/ykali,source="ab") %>%
  select(file,n,value,source,t) 

g0 <- get_witch("basegrowthcap")  %>% rename(g0=value)
 
perc_impact <- get_witch("damfrac_type") %>%
  rename(source=d) %>%
  bind_rows(abatefrac) %>%
  filter(ttoyear(t)==2100) %>%
  group_by(file,n,t) %>%
  mutate(perc=value/sum(value)) %>% ungroup() %>% select(-pathdir)

gdploss_barplot <- gdploss %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp==precipitation_sel & spread==tspread_sel) %>%
  inner_join(pop %>% rename(pop=value)) %>%
  inner_join(perc_impact %>% select(-value)) %>%
  inner_join(countries_map) %>%
  group_by(n,source) %>%
  mutate(valueerel=-(perc*value-perc[nsrm=="Cooperative" & COOP=="coop"]*value[nsrm=="Cooperative" & COOP=="coop"]) /(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop"])) %>%
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
  theme(text = element_text(size = 24)) +
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
n_to_name <- c("Brazil"="bra","India"="ind","China"="chn","USA"="usa")
damages_maps <- gdploss %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp==precipitation_sel & spread==tspread_sel) %>%
  inner_join(pop %>% rename(pop=value)) %>%
  inner_join(countries_map) %>%
  group_by(n) %>%
  mutate(valueerel=-100*(value-value[nsrm=="Cooperative" & COOP=="coop"]) /(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop"])) %>%
  filter(!nsrm %in% c("no SRM","Cooperative") ) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize(valueerel,method="fixed",
                                 breaks=c(-10000000,0,100,1000000000),
                                 labels=c("Laissez-faire","Push to cooperation","Push to mitigation"))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc,color=latitude),size=.2) +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = disc),color='red',size=.4) +
  scale_fill_manual(values=c("#4575B4","white","#a2231D"),name="Preferred strategy") +
  scale_color_viridis_d() +
  theme_void()+ 
  guides(color="none") +
  theme(panel.background = element_rect(fill="white",color="white"),
        legend.position = "top") +
  theme(text = element_text(size = 24)) +
  facet_wrap(ordered(nsrm,c("Brazil","India","China","USA"))~.,nrow=1)

percentages <- gdploss %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp==precipitation_sel & spread==tspread_sel) %>%
  inner_join(pop %>% rename(pop=value)) %>%
  inner_join(ykali %>% rename(y0=value)) %>%
  inner_join(countries_map) %>%
  group_by(n) %>%
  mutate(valueerel=-100*(value-value[nsrm=="Cooperative" & COOP=="coop"]) /(value[nsrm=="Cooperative" & COOP=="coop"]-value[nsrm=="no SRM" & COOP=="coop"])) %>%
  filter(!nsrm %in% c("no SRM","Cooperative") ) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize(valueerel,method="fixed",
                                 breaks=c(-10000000,0,100,1000000000),
                                 labels=c("Laissez-faire","Push to cooperation","Push to mitigation")),
         ypc=y0/pop) %>%
  group_by(file,disc) %>%
  summarise(pop=sum(pop),y=sum(y0)) %>%
  group_by(file) %>%
  mutate(pop=pop/sum(pop), y = y/sum(y)) %>%
  pivot_longer(c(pop,y)) %>%
  filter(name=="pop") %>%
  inner_join(sanitized_names)%>%
  ggplot() +
  geom_bar(aes(x=name, y=value, fill=disc), color="black",stat="identity",position="stack") +
  facet_wrap(ordered(nsrm,c("Brazil","India","China","USA"))~.,nrow=1) + 
  coord_flip() +
  scale_fill_manual(values=c("#4575B4","white","#a2231D"),name="Preferred strategy") +
  theme_void() +
  theme(legend.position="none",strip.text=element_blank())

fig_noncoop <- ggarrange(damages_maps,percentages,gdploss_barplot,heights=c(4,0.3,5),nrow=3)
ggsave("Fig_noncoop.png",fig_noncoop,width=18,height=14)



