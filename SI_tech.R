cpricesplot <- ggplot() +
  geom_line( data = CPRICE %>% 
              filter(TECH!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>%
              group_by_at(c("t",file_group_columns)) %>%
              summarise(med=median(value),max=quantile(value,0.66),min=quantile(value,0.33)),
            aes(x=ttoyear(t),y=med,color=TECH),linewidth=2) +
  theme_pubr() + xlab('') + ylab('')

cumulative_all_dac <- E_NEG %>%
  filter(ttoyear(t) <= 2100) %>%
  group_by(file,n) %>% 
  complete(t=seq(min(t), max(t), 0.2)) %>% 
  mutate(use=approxfun(t, use)(t)) %>%
  group_by(file,n) %>% 
  mutate(enegcum=cumsum(use)) 

plot_eneg <- cumulative_all_dac %>% 
  filter(ttoyear(t) %in% c(2050,2100) ) %>%
  inner_join(scenarios) %>%
  filter(n=="World" & (TECH!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest")) %>%
  inner_join(COST_CDR %>% 
    filter(TECH!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>%
    inner_join(E_NEG) %>%
    group_by_at(c("t",file_group_columns)) %>%
    summarise(med=sum(cdrcost)/sum(use)*1000))

cumcarbon <- ggplot(plot_eneg) +
  geom_bar(aes(x=TECH,y=enegcum,fill=TECH),stat="identity",position="dodge",color="black") + 
  geom_text(aes(x=TECH,y=enegcum*1.05,color=TECH,label=round(med)),stat="identity",position="dodge") + 
  facet_grid(ttoyear(t)~.,scales="free") +
  theme_pubr() + xlab('') + ylab('') + 
  theme(legend.position="none",
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())
  

ggarrange(cpricesplot + theme(legend.position="none"), cumcarbon, widths = c(0.65,0.35), labels=c("a","b"), legend.grob = get_legend(cpricesplot))
ggsave("SIT_fig1.png",width=18,height=9,dpi=300,units="cm")

gini_onlyref <- YGROSS_DIST %>%
  filter(TECH!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>%
  inner_join(COST_CDR) %>%
  inner_join(ABATECOST) %>%
  inner_join(Y_DIST %>% rename(gdp=value)) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="abatement") %>% rename(ctax=value) %>% select(-ineq_elast) ) %>%
  inner_join(quantiles_ref %>% make_scen() %>% rename(qref=value) ) %>%
  group_by(t,file) %>%
  mutate(ynew = ygrossd - ctax*(abcost+cdrcost) ) %>%
  group_by_at(c("t","n",file_group_columns)) %>% 
  summarise(gini=reldist::gini(ynew/sum(ynew))*100,gini0=reldist::gini(qref/sum(qref))*100) %>% ungroup() %>%
  group_by_at(c("t","n")) %>%
  mutate(ginirel = gini-gini[file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest"] ) %>%
  rowwise() %>% mutate(ginirel0 = gini-gini0 ) %>%  
  inner_join(share %>% filter(file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>% ungroup() %>% select(n,breaksname)) %>%
  group_by_at(c("t",file_group_columns,"breaksname")) %>%
  summarise(med=median(ginirel0),max=quantile(ginirel0,0.66),min=quantile(ginirel0,0.33)) %>%
  ungroup()

gini_techsens <- Y_DIST %>% 
  filter(TECH!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>%
  inner_join(quantiles_ref %>% make_scen() %>% rename(qref=value)) %>%
  group_by_at(c("t","n",file_group_columns)) %>% 
  summarise(gini=reldist::gini(value/sum(value))*100,gini0=reldist::gini(qref/sum(qref))*100) %>% ungroup() %>%
  group_by_at(c("t","n")) %>%
  mutate(ginirel = gini-gini[file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest"] ) %>%
  rowwise() %>% mutate(ginirel0 = gini-gini0 ) %>%
  inner_join(share %>% filter(file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>% ungroup() %>% select(n,breaksname)) %>%
  group_by_at(c("t",file_group_columns,"breaksname")) %>%
  summarise(med=median(ginirel0),max=quantile(ginirel0,0.66),min=quantile(ginirel0,0.33)) %>%
  ungroup()

ggplot(gini_techsens) +
  geom_line(aes(x=ttoyear(t),y=med,color=TECH),linewidth=2) +
  geom_line(data=gini_onlyref,aes(x=ttoyear(t),y=med,color=TECH),linewidth=1,linetype=2) +
  geom_ribbon(aes(x=ttoyear(t),ymin=min,ymax=max,fill=TECH),alpha=0.1) +
  facet_wrap(breaksname~.,) + theme_pubr() + xlab('') + ylab('') +
  guides(fill=guide_legend(nrow=3),color=guide_legend(nrow=3)) 
ggsave("SIT_fig2.png",width=18,height=18,dpi=300,units="cm")


#### figure 3
shapleyreftheil_tech <- ALL_FLOWS %>%
  filter(  (TECH!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") &
             ttoyear(t) %in% c(2075,2100)) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,file) %>%
  do(owen(zid_theilb,list(c("abcost","err"),c("cdrcost"), c("cdrrev","gentax","transfer","ctx")), data=.)) %>%
  mutate(dec="between") 

shapleyreftheil_tech <- ALL_FLOWS %>%
  filter(  (TECH!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") &
             ttoyear(t) %in% c(2075,2100)) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,file) %>%
  do(owen(zid_theilw,list(c("abcost","err"),c("cdrcost"), c("cdrrev","gentax","transfer","ctx")), data=.)) %>%
  mutate(dec="within") %>%
  rbind(shapleyreftheil_tech) 

plot_theil <- shapleyreftheil_tech %>% 
  group_by(t,file,group,dec) %>% summarise(value=sum(value)) %>% 
  filter(ttoyear(t) %in% c(2075,2100)) %>% inner_join(scenarios)  %>% 
  mutate(group=as.factor(group))
levels(plot_theil$group) <- list("Abatement costs"="1","CDR costs"="2","CDR transfers"="3")

ggplot(plot_theil) +
  geom_bar(data=. %>% filter(value>0) %>% group_by(t,file,DIST,COST,TECH) %>% summarise(value=sum(value)),aes(x=TECH,y=value*100),fill=NA,stat="identity",color="grey",linetype=2) +
  geom_bar(data=.%>% filter(group %in% c("CDR costs","CDR transfers")),aes(x=TECH,y=value*100,fill=group,color=group,alpha=dec),stat="identity",color="black") +
  geom_point(data=.%>% filter(group %in% c("CDR costs","CDR transfers")) %>% group_by(t,file,DIST,COST,TECH) %>% summarise(value=sum(value)),
             aes(x=TECH,y=value*100),shape=3,size=2) +
  geom_point(data=. %>% group_by(t,file,DIST,COST,TECH) %>% summarise(value=sum(value)),
             aes(x=TECH,y=value*100),shape=1,size=2) +
  facet_grid(ttoyear(t)~.,) + xlab('') + ylab('') +
  guides(fill=guide_legend(title="Inequality driver"),alpha=guide_legend(title="Inequality contribution")) +
  scale_fill_manual(values=c("#00BA38","#619CFF")) + theme_pubr() + theme(text = element_text(size = 7))
ggsave("SIT_fig3.png",width=18,height=16,dpi=300,units="cm")

