### global gini
ggplot(shapleyref %>% mutate(factors=case_when(name=="c1"~"Abatement costs",name=="c2"~"DAC profits and tax increase", name=="c3"~"carbon tax net of recycling",name=="err"~"Other factors")) %>% 
         group_by(factors,BD) %>% 
         complete(t=seq(min(t), max(t), 0.2)) %>% mutate(sigma=approxfun(t, sigma)(t))) + 
  geom_area(aes(x=ttoyear(t),y=sigma*100,fill=factors),color="black") +
  geom_line(data=. %>% group_by(t,BD) %>% summarise(value=sum(sigma)) ,
            aes(x=ttoyear(t),y=value*100),color="blue",size=1.5,linetype=2 ) +
#  geom_line(data=giniw %>% filter(ssp==ssp & ttoyear(t)>=2020  & ttoyear(t)<=2100 & ( (O=="yes" & B=="700") )), aes(x=ttoyear(t),y=ginirel*100),color="black" ) +
  xlab('') + ylab('variation in gini index \n 1.5°C vs ref') + theme_pubr() + grids() + theme(legend.title = element_blank()) + guides(fill=guide_legend(nrow=2)) +
  facet_wrap(BD~.) + guides(color="none")

### regional gini 
countries <- c("usa","can","rus","golf57","chn","rsaf","nor")
c2 <- c("USA","Canada","Russia","Gulf states","China","SSA","Norway")
cname <- setNames(c2,countries)
palette_short <- setNames(region_palette_ed57[countries],c2)

ggplot(rbind(ginit,giniw %>% rename(gini=gini_total) %>% select(-ginirel) %>% filter(t>=1)) %>% 
         filter(IMP == impacts) %>%
         group_by_at(c("t","n","ssp") ) %>% 
         mutate(ginirel = (gini - gini[B=="ref"])) %>%
         filter(ssp %in% c(2) & setting==setting_select & B=="700" & O=="yes" & ttoyear(t)>=2020 & ttoyear(t)<=2100) %>%
         ungroup() %>% mutate(ssp=paste0("ssp",ssp),Region=cname[n])) +
  geom_line(data=.%>%
              filter(n %in% countries),aes(x=ttoyear(t),y=ginirel*100,color=Region),size=1.5) +  
  geom_line(data=.%>%
              filter(!n %in% countries) %>% 
              group_by(t,ssp,file,BD) %>% summarise(med=median(ginirel)),aes(x=ttoyear(t),y=med*100),size=1,color="black") + 
  geom_ribbon(data=.%>% filter(!n %in% countries) %>% group_by(t,ssp,file,BD) %>% summarise(min=quantile(ginirel,probs=c(0.33)),max=quantile(ginirel,probs=c(0.66))),
              aes(x=ttoyear(t),ymin=min*100,ymax=max*100),size=0.5,alpha=0.2,fill="black") + 
  scale_fill_manual(values=c(palette_short),guide="none") +
  geom_hline(yintercept=0,color="grey") +
  scale_color_manual(values=c(palette_short,"Others"="black")) +
  theme_pubr()+ grids() + xlab('') + ylab('Delta gini points') + theme(legend.title = element_blank()) + facet_wrap(BD~.,)

#regional gini map
require(arules)
maplot <- rbind(ginit,giniw %>% rename(gini=gini_total) %>% select(-ginirel) %>% filter(t>=1)) %>% 
  filter(IMP == impacts) %>%
  group_by_at(c("t","n","ssp") ) %>% 
  mutate(ginirel = (gini - gini[B=="ref"])) %>% 
  filter(ttoyear(t) %in% c(2075) & B=="700" ) %>% mutate(n=as_factor(n)) %>% ungroup() %>% mutate(value=discretize(round(ginirel*100,2),breaks=5)) %>%
  full_join(reg) %>% filter(!is.na(value))

ginimapplot <- ggplot(maplot, aes(x = long, y = lat) ) +
  geom_polygon(aes(group = group, fill = value ),color='black',size=.1)+
  scale_fill_viridis_d() +
  theme_void()+
  theme(legend.position = "bottom", strip.text.x = element_text(size=12, face="bold"),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) + ggtitle('Gini index variation due to CDR in 2075') + facet_wrap(BD~.,) 

#dac sharing map 
maplot <- dacum %>% 
  filter(B=="700" & n!="World") %>% 
  mutate(n=as_factor(n)) %>% ungroup() %>% 
#  mutate(use=discretize(round(use,2),breaks=10)) %>%
  full_join(reg) %>% filter(!is.na(use))

ggplot(maplot, aes(x = long, y = lat) ) +
  geom_polygon(aes(group = group, fill = use ),color='black',size=.1)+
  scale_fill_viridis_c() +
  theme_void()+
  theme(legend.position = "bottom", strip.text.x = element_text(size=12, face="bold"),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) + ggtitle('Cumulative DAC distribution') + facet_wrap(BD~.,) 
