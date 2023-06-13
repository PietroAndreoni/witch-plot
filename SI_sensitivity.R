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



plot_gini <- ginit  %>%
  filter(file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                     "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGhigh",
                     "",
                     ""))
  inner_join(share) %>%
  group_by(t,file,cap,breaks) %>%
  summarise(med=median(ginirel0),min=quantile(ginirel0,0.33),max=quantile(ginirel0,0.66)) %>%
  mutate(breaks=as.factor(as.character(breaks)))

breaksnames <- c("[0.147,3.45)"="Low","[3.45,8.12)"="Medium","[8.12,18.1)"="High","[18.1,65.9]"="Very high")

# Update the levels of the 'breaks' column in your data frame
plot_gini$breaks <- factor(plot_gini$breaks, levels = names(breaksnames))

# Assign the new level names
levels(plot_gini$breaks) <- breaksnames
plot_gini$breaks <- ordered(plot_gini$breaks, levels=c("Low","Medium","High","Very high"))

ggplot(plot_gini) +
  geom_point(data=.%>%filter(ttoyear(t)%%10==0 & cap!="neutral"),aes(x=ttoyear(t),y=med,color=breaks,alpha=cap),size=3) +
  geom_line(data=.%>%filter(cap!="neutral"),aes(x=ttoyear(t),y=med,color=breaks,group=cap,alpha=cap),linewidth=2) +
  #  geom_point(data=.%>%filter(ttoyear(t)%%10==0 & cap=="neutral"),aes(x=ttoyear(t),y=med),color="black",size=3) +
  geom_line(data=.%>%filter(cap=="neutral"),aes(x=ttoyear(t),y=med,group=cap),color="black",linewidth=2,linetype=2) +
  facet_wrap(breaks~.,) +
  scale_alpha_manual(values=c(0.5,0.6,0.7,0.8,1,1),breaks = c("0 %","25 %","50 %","75 %","100 %","neutral")) +
  scale_color_manual(values= scales::hue_pal()(4) ) +
  ggpubr::theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("Inequality variation [Gini points]") + xlab("") +
  guides(color = "none",alpha = guide_legend(title=NULL))
