##### global effects 
names <-  list("Central specification"="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
               "Regressive taxes"="ssp2_B700_DISTgeo_COSTbest_TAXlow_NEGbest",
               "Concentrated capital"="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGhigh",
               "Shared capital"="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGlow",
               "Progressive taxes"="ssp2_B700_DISTgeo_COSTbest_TAXhigh_NEGbest")

a <- shapleyreftheil %>% 
  group_by(t,file,group,dec) %>% summarise(value=sum(value)) %>%
  filter(file %in% files & 
           ttoyear(t) %in% c(2075,2100)) %>% inner_join(scenarios)  %>% mutate(group=as.factor(group),file=as.factor(file))
levels(a$group) <- list("Abatement costs"="1","CDR costs"="2","CDR transfers"="3")
levels(a$file) <- names

ggplot(a) +
  geom_bar(data=. %>% filter(value>0) %>% group_by(t,file,DIST,COST) %>% summarise(value=sum(value)),aes(x=file,y=value*100),fill=NA,stat="identity",color="grey",linetype=2) +
  geom_bar(data=.%>% filter(group %in% c("CDR costs","CDR transfers")),aes(x=file,y=value*100,fill=group,color=group,alpha=dec),stat="identity",color="black") +
  geom_point(data=.%>% filter(group %in% c("CDR costs","CDR transfers")) %>% group_by(t,file,DIST,COST) %>% summarise(value=sum(value)),
             aes(x=file,y=value*100),shape=3,size=2) +
  geom_point(data=. %>% group_by(t,file,DIST,COST) %>% summarise(value=sum(value)),
             aes(x=file,y=value*100),shape=1,size=2) +
  facet_grid(ttoyear(t)~.,) + xlab('') + ylab('') +
  guides(fill=guide_legend(title="Inequality driver"),alpha=guide_legend(title="Inequality contribution")) +
  scale_fill_manual(values=c("#00BA38","#619CFF")) + theme_pubr() + 
  theme(text = element_text(size = 7))
ggsave("SIA_fig1.png",width=18,height=16,dpi=300,units="cm")
