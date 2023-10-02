cprice_brackets <- ykali  %>%
  filter(ttoyear(t)==2020) %>%
  make_scen() %>% 
  filter(CP=="differentiated") %>% 
  mutate(gdp=value) %>%
  inner_join(pop) %>%
  mutate(bracket = case_when(gdp/pop*1e6*113.647/104.691 < 4125 ~ "Low",
                            gdp/pop*1e6*113.647/104.691 > 13205 ~ "High",
                            .default = "Middle") ) %>%
  select_at(c("n","bracket"))

plot_prices <- CPRICE %>% 
  filter(!CP %in% c("Central","differentiated") | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>%
  group_by_at(c("t",file_group_columns)) %>%
  summarise(med=median(value),max=quantile(value,0.66),min=quantile(value,0.33)) %>%
  mutate(bracket="Uniform") %>%
  bind_rows(CPRICE %>% 
              filter(CP == "differentiated") %>%
              inner_join(cprice_brackets) %>%
              group_by_at(c("t",file_group_columns,"bracket")) %>%
              summarise(med=median(value),max=quantile(value,0.66),min=quantile(value,0.33))   )

plot_prices$bracket <- ordered(plot_prices$bracket, levels=c("Uniform","Low","Middle","High"))
cprice_brackets$bracket <- ordered(cprice_brackets$bracket, levels=c("Low","Middle","High"))

insert <- ggplot(inner_join(reg %>% filter(iso3!="ATA"),cprice_brackets,relationship = "many-to-many"),aes(x=long,y=lat)) +
  geom_polygon(aes(group = group, alpha = bracket),color='black',fill="#7CAE00",linewidth=.1) +
  theme_void()+
  scale_alpha_manual(values=c(0.1,0.5,1)) +
  theme(legend.position="none",strip.text.x = element_text(size=12, face="bold"),
        plot.title = element_text(hjust = 0.5),
        text = element_text(size = 7)) +
  labs(fill="") 

main <- ggplot(plot_prices) +
  geom_line(aes(x=ttoyear(t),y=med,color=CP,alpha=bracket),linewidth=2) +
  scale_alpha_manual(values=c(1,0.4,0.7,1)) +
  theme_pubr() + xlab('') + ylab('Carbon price [US$/tonCO2]') + theme(text = element_text(size = 7))
leg <- get_legend(main)

require(cowplot)
cpricesplot <- ggdraw() +
  draw_plot(main  + theme(legend.position="none") ) +
  draw_plot(insert, x = 0.15, y = .6, width = .3, height = .3)

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
  filter(n=="World" & (CP!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest")) %>%
  inner_join(COST_CDR %>% 
               filter(CP!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>%
               inner_join(E_NEG) %>%
               group_by_at(c("t",file_group_columns)) %>%
               summarise(med=sum(cdrcost)/sum(use)*1000))

cumcarbon <- ggplot(plot_eneg) +
  geom_bar(aes(x=CP,y=enegcum,fill=CP),stat="identity",position="dodge",color="black") + 
  geom_text(aes(x=CP,y=enegcum*1.15,color=CP,label=round(med) ),stat="identity",position="dodge") + 
  facet_grid(ttoyear(t)~.,scales="free") +
  theme_pubr() + xlab('') + ylab('Cumulative carbon removed [GtCO2]') + 
  theme(legend.position="none",text = element_text(size = 7),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())


ggarrange(cpricesplot , cumcarbon, widths = c(0.65,0.35), labels=c("a","b"), legend.grob = leg)
ggsave("SICP_fig1.png",width=18,height=9,dpi=300,units="cm")

gini_onlyref <- YGROSS_DIST %>%
  filter(CP!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>%
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
  filter(CP!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>%
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
  geom_line(aes(x=ttoyear(t),y=med,color=CP),linewidth=2) +
  geom_line(data=gini_onlyref,aes(x=ttoyear(t),y=med,color=CP),linewidth=1,linetype=2) +
  geom_ribbon(aes(x=ttoyear(t),ymin=min,ymax=max,fill=CP),alpha=0.1) +
  facet_wrap(breaksname~.,) + theme_pubr() + xlab('') + ylab('') + theme(text = element_text(size = 7))
ggsave("SICP_fig2.png",width=18,height=16,dpi=300,units="cm")

#### figure 3
shapleyreftheil_cp <- ALL_FLOWS %>%
  filter(  (CP!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") &
           ttoyear(t) %in% c(2075,2100)) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,file) %>%
  do(owen(zid_theilb,list(c("abcost","err"),c("cdrcost"), c("cdrrev","gentax","transfer","ctx")), data=.)) %>%
  mutate(dec="between") 

shapleyreftheil_cp <- ALL_FLOWS %>%
  filter(  (CP!="Central" | file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") &
              ttoyear(t) %in% c(2075,2100)) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,file) %>%
  do(owen(zid_theilw,list(c("abcost","err"),c("cdrcost"), c("cdrrev","gentax","transfer","ctx")), data=.)) %>%
  mutate(dec="within") %>%
  rbind(shapleyreftheil_cp) 

plot_theil <- shapleyreftheil_cp %>% 
  group_by(t,file,group,dec) %>% summarise(value=sum(value)) %>% 
  filter(ttoyear(t) %in% c(2075,2100)) %>% inner_join(scenarios)  %>% 
  mutate(group=as.factor(group))
levels(plot_theil$group) <- list("Abatement costs"="1","CDR costs"="2","CDR transfers"="3")

ggplot(plot_theil) +
  geom_bar(data=. %>% filter(value>0) %>% group_by(t,file,DIST,COST,CP) %>% summarise(value=sum(value)),aes(x=CP,y=value*100),fill=NA,stat="identity",color="grey",linetype=2) +
  geom_bar(data=.%>% filter(group %in% c("CDR costs","CDR transfers")),aes(x=CP,y=value*100,fill=group,color=group,alpha=dec),stat="identity",color="black") +
  geom_point(data=.%>% filter(group %in% c("CDR costs","CDR transfers")) %>% group_by(t,file,DIST,COST,CP) %>% summarise(value=sum(value)),
             aes(x=CP,y=value*100),shape=3,size=2) +
  geom_point(data=. %>% group_by(t,file,DIST,COST,CP) %>% summarise(value=sum(value)),
             aes(x=CP,y=value*100),shape=1,size=2) +
  facet_grid(ttoyear(t)~.,) + xlab('') + ylab('') +
  guides(fill=guide_legend(title="Inequality driver"),alpha=guide_legend(title="Inequality contribution")) +
  scale_fill_manual(values=c("#00BA38","#619CFF")) + theme_pubr() + theme(text = element_text(size = 7))
ggsave("SICP_fig3.png",width=18,height=16,dpi=300,units="cm")
