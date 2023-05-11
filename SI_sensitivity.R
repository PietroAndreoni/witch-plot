##### global effects 

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
  scale_fill_manual(values=c("#00BA38","#619CFF")) + theme_pubr()
ggsave("SIA_fig1.png",width=9,height=8,dpi=320)



