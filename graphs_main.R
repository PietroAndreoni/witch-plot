### figure 1: 
###1a: global flows
fig1a <- FLOWS %>% select(-trgdprel,-txgdprel) %>% mutate(net = transfer + gentax + cdrcost + ctx + abcost + cdrrev) %>% 
  pivot_longer(c(transfer,gentax,cdrcost,ctx,abcost,cdrrev),names_to="Flow") %>% mutate(valuerel=value/gdp,netrel=net/gdp) %>% 
  filter(n=="World" & ttoyear(t) >= 2020 & ttoyear(t) <= 2100 & O=="yes" & B=="650" & ssp==2) %>%
  mutate(Source=case_when( Flow=="abcost" ~ "Abatement costs",
                           Flow=="cdrcost" ~ "Cost of NETs",
                           Flow=="ctx" ~ "Carbon tax",
                           Flow=="gentax" ~ "Other taxes",
                           Flow=="transfer" ~ "Recycling",
                           Flow=="cdrrev" ~ "NET revenues" )) %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),y=valuerel,fill=Source),color="black") + 
  geom_line(aes(x=ttoyear(t),y=netrel),color="blue",size=1.2,linetype="dotted") +
  theme_pubr() +
  scale_y_continuous(labels=scales::percent) +
  ylab('Costs and gains, \n global aggregate [% GDP]') + xlab('') + theme(legend.position = "bottom")

###1b: elasticities characterization
fig1b <- ggplot(ineq_weights %>% filter(ttoyear(t) %in% c(2075) & ssp==2 & O=="yes" & B=="650" & !ineq_elast %in% c("damages","redist") ) %>%
         mutate(ineq_elast=case_when(ineq_elast=="carbon_rev" ~ "NET revenus and costs",
                                     ineq_elast=="abatement" ~ "Carbon tax, abatement costs and recycling",
                                     ineq_elast=="tax" ~ "Other taxes" )) %>% 
         mutate(dist=as.factor(dist)) %>% mutate(dist=fct_relevel(dist,paste0("D",seq(1,10)))) ) +
  #  geom_line(aes(x=as.numeric(str_remove(dist,"D"))/10,y=value,group=interaction(n,ineq_elast),color=ineq_elast),alpha=0.3) +
  geom_line(data=quantiles_ref %>% mutate(dist=as.factor(dist)) %>% mutate(dist=fct_relevel(dist,paste0("D",seq(1,10)))) %>% filter(ttoyear(t) %in% c(2075) & ssp==2 & O=="yes" & B=="650") %>% group_by(t,ssp,dist) %>% summarise(med=median(value)),aes(x=dist,y=med,group=t),size=1,linetype=2) +
  geom_line(data=. %>% group_by(ineq_elast,t,ssp,dist) %>% summarise(med=median(value)),aes(x=dist,y=med,color=ineq_elast,group=interaction(t,ineq_elast)),size=2) +
  geom_point(data=. %>%group_by(ineq_elast,t,ssp,dist) %>% summarise(med=median(value)),
             aes(x=dist,y=med,color=ineq_elast),size=2,shape=21,fill="white") + theme_pubr() + xlab('Income distribution') + ylab('% of flow falling on decile d, \n country median') + theme(legend.title = element_blank()) +
  scale_y_continuous(labels=scales::percent) +
  scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) + theme(legend.position = "bottom")+ guides(color=guide_legend(nrow=2))

fig1 <- ggarrange(fig1a,fig1b,labels="AUTO")
ggsave("fig1.png",width=12,height=5,path=str_c(main_directory,"/graphs"),dpi=400)

### figure 2: 
emiplot <- ggplot(EMITOT %>% filter(n %in% c("World") & ttoyear(t) <= 2100 & B=="650" & ssp==2) %>% 
                  mutate(Source=case_when(Source=="eind"~"Industrial emissions",Source=="eland"~ "Land use change",Source=="use"~ "Carbon removal"  ), 
                  O=case_when(O=="yes"~"With overshoot",
                              O=="no"~"Without overshoot") )  %>% mutate(O=as.factor(O)) %>% 
                    mutate(O=fct_relevel(O,c("With overshoot","Without overshoot"))) )  +
  geom_area(aes(x=ttoyear(t), y=value,fill=Source)) +
  geom_line(data=E %>% filter(n %in% c("World") & ttoyear(t) >= 2015 & ttoyear(t) <= 2100 & B=="650" & ssp==2)%>% mutate(O=case_when(O=="yes"~"With overshoot", O=="no"~"Without overshoot"))  %>% mutate(O=as.factor(O)) %>% mutate(O=fct_relevel(O,c("With overshoot","Without overshoot"))),aes(x=ttoyear(t), y=e*44/12), size=1.2) +
  geom_line(data=CPRICE %>% filter(n %in% c("World") & ttoyear(t) <= 2100 & B=="650" & ssp==2) %>% mutate(O=case_when(O=="yes"~"With overshoot", O=="no"~"Without overshoot")) %>% mutate(O=as.factor(O)) %>% mutate(O=fct_relevel(O,c("With overshoot","Without overshoot"))), aes(x=ttoyear(t),y=2*cprice/100),color="red",size=1.2,linetype="dotted" ) +
  geom_text(data=CPRICE %>% mutate(O=case_when(O=="yes"~"With overshoot", O=="no"~"Without overshoot"))  %>% mutate(O=as.factor(O)) %>% mutate(O=fct_relevel(O,c("With overshoot","Without overshoot"))) %>% filter(n=="World" & B=="650"  & ssp==2) %>% group_by(B,O) %>% filter(cprice==max(cprice) | ttoyear(t)==2030),
            aes(x=ttoyear(t),y=2*cprice/100+3,label=paste(as.character(round(cprice)),"$/tCO2")),color="red" ) +
  facet_wrap(O~.,) +
  theme_pubr() +
  scale_fill_manual(values=c("#66FFFF","#FFCC99","#474826")) +
  ylab('Emissions and removal [GtCO2/yr]') + xlab('') + theme(legend.position = "bottom", legend.title = element_blank())

tatm <- ggplot(TATM %>% make_scen() %>% filter(ssp=="2" & (B=="650" | B=="ref") & ttoyear(t)<=2100) %>%
                 mutate(Scenario=case_when(Scenario=="1.5C full"~"1.5C w/o overshoot",Scenario=="1.5C peak"~"1.5C w/ overshoot",Scenario=="NDCs"~"NDCs"))) +
  geom_line(aes(x=ttoyear(t),y=value,color=Scenario),size=1) +
  theme_pubr() + xlab('') + ylab('Temperature increase') +
  scale_color_manual(values=c("red","green","blue"))

costs <- ggplot(COSTS %>% filter(ssp=="2" & B=="650" & ttoyear(t)<=2100 & n=="World") %>%
                  mutate(Scenario=case_when(Scenario=="1.5C full"~"1.5C w/o overshoot",Scenario=="1.5C peak"~"1.5C w overshoot"))) +
  geom_line(aes(x=ttoyear(t),y=costrel*100,color=Scenario),size=1) +
  theme_pubr() + xlab('') + ylab('GDP loss, REF relative [%]') +
  scale_color_manual(values=c("red","green"))

require(arules)
dacreg <- ggplot(dacum %>% filter( B=="650" & ssp==2) %>% mutate(ssp=paste0("ssp",ssp)) %>%
       mutate(Scenario=case_when(Scenario=="1.5C full"~"1.5C w/o overshoot",Scenario=="1.5C peak"~"1.5C w overshoot")) %>% full_join(reg) %>% filter(iso3!='ATA' & !is.na(iso3)) %>% ungroup() %>% mutate(useper=discretize(round(useper*100),breaks=4)), aes(x = long, y = lat) ) +
  geom_polygon(aes(group = group, fill = useper),color='black',size=.1)+
  geom_text(data=dacum %>% filter(B=="650" & ssp==2 & n=="World") %>% mutate(ssp=paste0("ssp",ssp)) %>%
              mutate(Scenario=case_when(Scenario=="1.5C full"~"1.5C w/o overshoot",Scenario=="1.5C peak"~"1.5C w overshoot")),aes(label=paste0("Global carbon removed\n",round(use)," GtCO2")),x=0,y=-50,fontface='bold') +
  scale_fill_viridis_d() +
  theme_void()+
  facet_wrap(Scenario~.,) + 
  theme(legend.position = "bottom", strip.text.x = element_text(size=12, face="bold"),plot.title = element_text(hjust = 0.5)) +
  labs(fill="% of global carbon removed")

fig2 <- ggarrange(emiplot,dacreg,ggarrange(tatm,costs,common.legend=TRUE,legend="bottom"),nrow=3,heights=c(0.35,0.35,0.3),labels="AUTO")
ggsave("fig2.png",width=12,height = 15,path=str_c(main_directory,"/graphs"),dpi=400)

### figure 3: 1.5 vs ref
countries <- c("usa","can","rus","golf57","egy","nor","swe","row")
c2 <- c("USA","Canada","Russia","Gulf states","Egypt","Norway","Sweden","Rest of the World")
cname <- setNames(c2,countries)
palette_short <- setNames(region_palette_ed57[setdiff(countries,"row")],setdiff(c2,"Rest of the World"))

fig3a <- full_join(inner_join(emabcum %>% filter(B=="650" & O=="yes" & n!="World"),dacum) %>% mutate(ssp=paste0("ssp",ssp)),shapleyrefall%>% group_by(t,n,ssp) %>% summarise(dacineq=(sigma[name=="c2"]+sigma[name=="c3"]),tot=sum(sigma)) %>% ungroup() %>% mutate(abineq=tot-dacineq) ) %>% inner_join(pop %>% rename(pop=value) %>% mutate(ssp=paste0("ssp",ssp))) %>%
  mutate(Region=cname[n]) %>% filter(ttoyear(t) %in% c(2075) & ssp=="ssp2" & use/abate>0.01) %>% ggplot() +
  geom_point(aes(x=use/(use+abate)*100,y=tot*100,fill=Region,size=use),shape=21,color="black") +
  #  geom_text_repel(aes(x=use/(use+abate)*100,y=tot*100,color=Region,label=n)) +
  scale_fill_manual(values=palette_short,na.value = "grey") +
  scale_color_manual(values=palette_short,na.value = "grey") +
  scale_size_continuous(range=c(3,12),breaks =c(10,40,80)) +
  labs(size="Carbon sequestred [GtCO2]") +
  guides(color="none",fill="none") +
  #  scale_x_log10() + #scale_y_log10() +
  xlab('Cumulative carbon removed \n[% abated + removed]') + ylab('Change in gini index, net-zero year \n [points]') +
  theme_pubr()

fig3b <- ggplot(rbind(ginit,giniw %>% rename(gini=gini_total) %>% select(-ginirel) %>% filter(t>=1)) %>% 
         group_by_at(c("t","n","ssp") ) %>% 
         mutate(ginirel = (gini - gini[B=="ref"])) %>%
         filter(ssp %in% c(2) & B=="650" & O=="yes" & ttoyear(t)>=2020 & ttoyear(t)<=2100) %>%
        ungroup() %>% mutate(ssp=paste0("ssp",ssp),Region=cname[n])) +
  geom_line(data=.%>%
              filter(n %in% countries),aes(x=ttoyear(t),y=ginirel*100,color=Region),size=1.5) +  
  geom_line(data=.%>%
              filter(!n %in% countries) %>% 
              group_by(t,ssp,file) %>% summarise(med=median(ginirel)),aes(x=ttoyear(t),y=med*100),size=1,color="black") + 
  geom_ribbon(data=.%>% filter(!n %in% countries) %>% group_by(t,ssp,file) %>% summarise(min=quantile(ginirel,probs=c(0.25)),max=quantile(ginirel,probs=c(0.75))),
              aes(x=ttoyear(t),ymin=min*100,ymax=max*100),size=0.5,alpha=0.2,fill="black") + 
  scale_fill_manual(values=c(palette_short),guide="none") +
  ylim(c(0,2.2)) +
  geom_hline(yintercept=0,color="grey") +
  scale_color_manual(values=c(palette_short,"Rest of the World"="black")) +
  theme_pubr()+ xlab('') + ylab('Change in gini index \n [points]') + theme(legend.title = element_blank())

fig3c <- ggplot() +
  geom_bar(data=shapleyrefall %>% filter(ssp %in% c("ssp2") & ttoyear(t) %in% c(2075,2100) ) %>% 
             mutate(n=ifelse(n %in% countries,n,"row")) %>% group_by(t,name,ssp,n) %>% summarise(sigma=median(sigma)) %>% 
             mutate(factors=case_when(name=="c1"~"Abatement costs",name=="c2"~"DAC revenues net of tax increase",name=="c3"~"carbon tax net of recycling",name=="err"~"Other factors"),Region=cname[n]) %>% group_by(t,n,ssp) %>% 
             mutate(value=sigma/sum(sigma)) %>% ungroup() %>% mutate(Region=as_factor(Region)) %>%
             mutate(Region=fct_relevel(Region,c2)),
           aes(x=Region,y=value*100,fill=factors,group=n),stat="identity",position="stack",color="black") +
  facet_wrap(ttoyear(t)~.,nrow=2) + 
  theme_pubr()+ xlab('') + ylab('Shapley decomposition \nof total inequality variation [%]') + theme(legend.position="bottom",axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),legend.title = element_blank()) + guides(fill=guide_legend(nrow=4,byrow=TRUE))
                         
fig3 <- ggarrange(ggarrange(fig3a,fig3b,nrow=2,labels="AUTO"),fig3c,ncol=2,widths=c(1.4,1),labels=c(NA,"C") )
ggsave("fig3.png",width=12,height=10,path=str_c(main_directory,"/graphs"),dpi=400)


### figure 4:overshoot vs no overshoot
fig4a <- ggplot(rbind(ginit,giniw %>% rename(gini=gini_total) %>% select(-ginirel) %>% filter(t>=1)) %>% 
         filter(IMP == impacts & B!="ref") %>%
         group_by_at(c("t","n","ssp","B") ) %>% 
         mutate(ginirel = (gini - gini[O=="yes"])) %>%
         filter(ssp %in% c(2) & setting==setting_select & B=="650" & O=="no" & ttoyear(t)>=2020 & ttoyear(t)<=2100) %>%
         mutate(Region=cname[n]) ) +
  geom_line(data=.%>%
              filter(n %in% countries),aes(x=ttoyear(t),y=ginirel*100,color=Region),size=1.5) + 
  geom_line(data=.%>%
              filter(!n %in% countries) %>% 
              group_by(t,ssp,file) %>% summarise(med=median(ginirel)),aes(x=ttoyear(t),y=med*100),size=1,color="black") + 
  geom_ribbon(data=.%>% filter(!n %in% countries) %>% group_by(t,ssp,file) %>% summarise(min=quantile(ginirel,probs=c(0.25)),max=quantile(ginirel,probs=c(0.75))),aes(x=ttoyear(t),ymin=min*100,ymax=max*100),size=0.5,alpha=0.2,fill="black",color="black") + 
  geom_hline(yintercept=0,color="grey") +
  scale_color_manual(values=c(palette_short,"Rest of the World"="black")) +
  theme_pubr() + xlab('') + ylab('Change in gini index \n [points]') + theme(legend.title=element_blank())

fig4b <- shapley0all %>% filter(ttoyear(t) %in% c(2050,2075,2100) & ssp=="ssp2" ) %>% 
  mutate(factors=case_when(name=="c1"~"Abatement costs",name=="c2"~"DAC revenues net of tax increase",name=="c3"~"carbon tax net of recycling",name=="err"~"Other factors"),Region=cname[n]) %>% 
  mutate(n=ifelse(n %in% setdiff(countries,"row"),"Selected","Rest of the World")) %>% 
  group_by(t,n,name,factors,ssp) %>% summarise(sigma=median(sigma)) %>% 
  #  group_by(t,ssp) %>% mutate(sum=med/sum(med)) %>% 
  ggplot() +
  geom_bar(aes(x=n,y=-sigma*100,fill=factors),position="stack",stat="identity",color="black") +
  facet_grid(.~as.factor(ttoyear(t)),) +
  xlab('') + ylab('Shapley decomposition of \n total change in gini index  [points]') +
  theme_pubr()+ theme(legend.title=element_blank(),axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + guides(fill=guide_legend(nrow=2))  

fig4 <- ggarrange(fig4a,fig4b,labels="AUTO")
ggsave("fig4.png",width=12,height=6,path=str_c(main_directory,"/graphs"),dpi=400)

## figure 5: global gini
fig5 <- ggplot(shapleyref %>% mutate(factors=case_when(name=="c1"~"Abatement costs",name=="c2"~"DAC profits and tax increase", name=="c3"~"carbon tax net of recycling",name=="err"~"Other factors")) %>% 
         group_by(factors,ssp) %>% 
         complete(t=seq(min(t), max(t), 0.2)) %>% mutate(sigma=approxfun(t, sigma)(t))) + 
  geom_area(aes(x=ttoyear(t),y=sigma*100,fill=factors),color="black") +
  geom_line(data=. %>% group_by(t,ssp) %>% summarise(value=sum(sigma)) ,
            aes(x=ttoyear(t),y=value*100),color="blue",size=1.5,linetype=2 ) +
  geom_line(data=giniw %>% filter(ssp==2 & Scenario!="Baseline" & O=="no" & ttoyear(t)<=2100) ,
            aes(x=ttoyear(t),y=ginirel*100)) +
  xlab('') + ylab('variation in gini index \n 1.5°C vs ref') + theme_pubr() + theme(legend.title = element_blank()) + guides(fill=guide_legend(nrow=2)) + guides(color="none")
ggsave("fig5.png",width=8.3,height = 6.5,path=str_c(main_directory,"/graphs"),dpi=400)

ggplot(gini_dec %>% filter(B=="650" & O=="no" & ssp==2 & ttoyear(t)<=2100)) +
  geom_area(data=.%>%pivot_longer(c(d0,d1,d2,d3)),aes(x=ttoyear(t),y=value,fill=name),position="stack") +
  geom_line(aes(x=ttoyear(t),y=d0+d1+d2+d3),linetype=2,color="blue") +
  geom_line(aes(x=ttoyear(t),y=tot)) 

ggplot(ginit %>% filter(B=="650" & O=="no" & ssp==2 & ttoyear(t)<=2100)) +
  geom_line(aes(x=ttoyear(t),y=ginirel*100)) +
  facet_wrap(n~.,)

## METHODS 
figm1a <- ggplot(full_join(gini,giniwealth) %>% filter(year>=1950) ) +
  geom_point(aes(x=gini,y=giniw,color=alpha2)) + 
  geom_smooth(data=. %>% filter(gini<0.6),aes(x=gini,y=giniw),method="lm",se=FALSE) + 
  geom_smooth(data=. %>% filter(gini>=0.6),aes(x=gini,y=giniw),method="lm",se=FALSE) + 
  xlab('Gini on income') + ylab('Gini on wealth') + theme_pubr() +   theme(legend.position="none")

figm1b <- ggplot(full_join(gini,eta) %>% full_join(etatax)) +
  geom_point(aes(x=gini,y=eta),color="blue",alpha=0.5) + 
  geom_point(aes(x=gini,y=etatax),color="red",shape=2,alpha=0.5) + 
  geom_smooth(aes(x=gini,y=eta),method="lm",formula=y ~ poly(x, 2),color="grey") + 
  geom_smooth(aes(x=gini,y=etatax),method="lm",linetype=2,color="grey") + 
  xlab('Gini index') + ylab('Estimated elasticity') + theme_pubr()+   theme(legend.position="none")

figm1 <- ggarrange(figm1a,figm1b,labels="AUTO")
ggsave("figm1.png",width = 12,height=6,path=str_c(main_directory,"/graphs"),dpi=400)

get_witch_simple("el_coeff")
el_coeff <- el_coeff %>% make_scen()
figm2 <- ggplot(el_coeff %>% filter(ttoyear(t)>=2020 & ttoyear(t)<=2100 & O=="yes" & B=="650" & !ineq_elast %in% c("damages","redist") ) %>%
         mutate(ineq_elast=case_when(ineq_elast=="carbon_rev" ~ "NET revenus and costs",
                                   ineq_elast=="abatement" ~ "Carbon tax, abatement costs and recycling",
                                   ineq_elast=="tax" ~ "Other taxes" ))  ) +
  geom_line(aes(x=ttoyear(t),y=value,group=interaction(n,ineq_elast),color=ineq_elast),alpha=0.3) +
  geom_line(data=.%>%group_by(ineq_elast,t,ssp) %>% summarise(med=median(value)),aes(x=ttoyear(t),y=med,color=ineq_elast),size=2) +
  geom_point(data=. %>%group_by(ineq_elast,t,ssp) %>% summarise(med=median(value)) %>% filter(ttoyear(t)%%5==0),
             aes(x=ttoyear(t),y=med,color=ineq_elast),size=2,shape=21,fill="white") +
  facet_wrap(ssp~.) + theme_pubr() + xlab('') + ylab('Elasticity') + theme(legend.title = element_blank()) 
ggsave("figm2.png",width=12,height=10,path=str_c(main_directory,"/graphs"),dpi=400)


### SSPs 
emissp <- ggplot(E %>% filter(n=="World" & ttoyear(t)>=2000 & ttoyear(t)<=2100)) +
  geom_line(aes(x=ttoyear(t),y=e*44/12,color=ssp,linetype=Scenario),size=1) +
  theme_pubr() + 
  ylab('CO2 emissions [GtCO2/yr]') + xlab('')

dacssp <- ggplot(E_NEG %>% filter(n=="World" & ttoyear(t)>=2000 & ttoyear(t)<=2100 & B!="ref")) +
  geom_line(aes(x=ttoyear(t),y=use,color=ssp,linetype=Scenario),size=1) +
  theme_pubr() + 
  ylab('Carbon captured [GtCO2/yr]') + xlab('')

cpssp <- ggplot(CPRICE %>% filter(n=="World" & ttoyear(t)>=2000 & ttoyear(t)<=2100) ) +
  geom_line(aes(x=ttoyear(t),y=cprice,color=ssp,linetype=Scenario),size=1) +
  theme_pubr() + 
  ylab('Carbon price [2010US$/tonCO2]') + xlab('')

plot <- rbind(ginit,giniw %>% mutate(n="World") %>% rename(gini=gini_total) %>% select(-ginirel) %>% filter(t>=1)) %>% 
  filter(ttoyear(t) >=2015 & ttoyear(t) <=2100) %>%
  group_by(t,n,ssp) %>% 
  mutate(ginirel = (gini - gini[B=="ref"]))

ginissp <- ggplot() +
  geom_line(data=plot %>% filter(n=="World"),
            aes(x=ttoyear(t),y=ginirel*100,color=ssp,linetype=Scenario),size=1) +
  theme_pubr() +
  ylab('Global gini index') + xlab('')

ggarrange(emissp,dacssp,cpssp,ginissp,common.legend=TRUE,legend="bottom")
ggsave("figm3.png",width=8.3,height = 6.5,path=str_c(main_directory,"/graphs"),dpi=400)

## informations in section on global inquality
gdpc <- Y %>% filter(B==700 & O=="yes" & ttoyear(t)>=2050 & ttoyear(t) <= 2100) %>% 
  inner_join(pop %>% make_scen()) %>% inner_join(K_DAC) %>% inner_join(PROF_CDR) %>%
  group_by_at(c("n", "pathdir", file_group_columns)) %>% 
  complete(t=seq(min(t), max(t), 0.2)) %>% mutate(gdp=approxfun(t, gdp)(t),value=approxfun(t, value)(t),use=approxfun(t, use)(t),cdrcost=approxfun(t, cdrcost)(t)) %>%
  group_by_at(c("n", "pathdir", file_group_columns)) %>% 
  summarise(gdp=sum(gdp/((1+0.015)^(ttoyear(t)-2020))),pop=sum(value),use=sum(use),cdrcost=sum(cdrcost/((1+0.015)^(ttoyear(t)-2020))),) %>% 
  ungroup() %>% inner_join(cleanreg) %>% 
  group_by(file,ssp) %>% 
  mutate(rich=ifelse(gdp/pop>quantile(gdp/pop,0.5),"yes","no")) %>%
  group_by(North,ssp) %>% summarise(use=sum(use),pop=sum(pop),cdrcost=sum(cdrcost),cdrcost_mean=weighted.mean(cdrcost/pop,weights=pop)) %>% 
  group_by(ssp) %>% mutate(ratio=cdrcost_mean/cdrcost_mean[North=="no"])

### profits of CDR



### GDPc vs baseline Gini
regmap <- ggplot(full_join(PROF_CDR %>% 
                             filter(ttoyear(t)==2100 & Scenario=="1.5C peak" & n!="World" & ssp==2) %>% 
                             inner_join(YGROSS) %>% 
                             inner_join(l %>% rename(pop=value)) %>% 
                                        mutate(profd=discretize(prof*1000,method="fixed",breaks=c(0,10,100,500,1000,3000)),
                                               profdpc=discretize(prof/pop*1e6,breaks=5),
                                               profrel=discretize(prof/value,method="fixed",breaks=c(0,0.01,0.02,0.05,0.10,0.4))),
                           reg %>% filter(ISO!='ATA' & !is.na(ISO))), aes(x = long, y = lat) ) +
  geom_polygon(aes(group = group, fill = profdpc),color='black',size=.1)+
  theme_void()+
  scale_fill_viridis_d() +
  theme(strip.text.x = element_text(size=12, face="bold"),plot.title = element_text(hjust = 0.5))

PROF_CDR %>% 
            filter(ttoyear(t)==2100 & Scenario=="1.5C peak" & n=="World") %>% 
            inner_join(YGROSS) %>%                            
            inner_join(l %>% make_scen() %>% make_global_sum()%>% rename(pop=value)) %>%
  mutate(profrel=prof/ygross,profc=prof/pop*1e6)
          