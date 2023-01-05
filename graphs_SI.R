### fig 3 per ssp:
full_join(inner_join(emabcum %>% filter(B=="700" & O=="yes" & n!="World"),dacum) %>% mutate(ssp=paste0("ssp",ssp)),shapleyrefall%>% group_by(t,n,ssp) %>% summarise(dacineq=(sigma[name=="c2"]+sigma[name=="c3"]),tot=sum(sigma)) %>% ungroup() %>% mutate(abineq=tot-dacineq) ) %>% inner_join(pop %>% rename(pop=value) %>% mutate(ssp=paste0("ssp",ssp))) %>%
  mutate(Region=cname[n]) %>% filter(ttoyear(t) %in% c(2050,2075,2100) & use/abate>0.01) %>% ggplot() +
  geom_point(aes(x=use/(use+abate)*100,y=tot*100,fill=Region,size=use),shape=21,color="black") +
  #  geom_text_repel(data=.%>%filter(!is.na(Region)),aes(x=use/abate*100,y=dacineq*100,color=Region,label=Region)) +
  #  geom_smooth(aes(x=use/abate*100,y=dacineq*100),method="lm",color="red") +
  geom_smooth(aes(x=use/(use+abate)*100,y=tot*100),method="lm") +
  stat_cor(aes(x=use/(use+abate)*100,y=tot*100)) +
  scale_fill_manual(values=palette_short,na.value = "grey") +
  scale_color_manual(values=palette_short,na.value = "grey") +
  scale_size_continuous(range=c(3,12),breaks =c(10,40,80)) +
  labs(size="Carbon sequestred") +
  facet_grid(ssp~ttoyear(t),scales ="free_y") +
  guides(color="none") +
  #  scale_x_log10() + #scale_y_log10() +
  xlab('Cumulative carbon removed \n[% abated + removed]') + ylab('Inequality increase [Delta gini points]') +
  theme_pubr()

ggsave("SI_fig1.png",width=10,height=13,path=str_c(main_directory,"/graphs"),dpi=400)

# figure 3c summary all SSPs
shapleyrefall %>% filter(ttoyear(t) %in% c(2050,2075,2100) ) %>% 
  mutate(factors=case_when(name=="c1"~"Abatement costs",name=="c2"~"DAC revenues net of tax increase",name=="c3"~"carbon tax net of recycling",name=="err"~"Other factors"),Region=cname[n]) %>% 
  mutate(n=ifelse(n %in% countries,"Selected","ROW")) %>% group_by(t,n,name,factors,ssp) %>% summarise(sigma=median(sigma)) %>% 
  #  group_by(t,ssp) %>% mutate(sum=med/sum(med)) %>% 
  ggplot() +
  geom_bar(aes(x=n,y=sigma*100,fill=factors),position="stack",stat="identity",color="black") +
  facet_grid(ssp~as.factor(ttoyear(t)),) +
  xlab('') + ylab('Inequality increase \n [Delta gini points]') +
  theme_pubr()
 ggsave("SI_fig2.png",width=10,height=13,path=str_c(main_directory,"/graphs"),dpi=400)

## fig 4b: all SSPS
shapley0all %>% filter(ttoyear(t) %in% c(2050,2075,2100) ) %>% 
  mutate(factors=case_when(name=="c1"~"Abatement costs",name=="c2"~"DAC revenues net of tax increase",name=="c3"~"carbon tax net of recycling",name=="err"~"Other factors"),Region=cname[n]) %>% 
  mutate(n=ifelse(n %in% countries,"Selected","ROW")) %>% group_by(t,n,name,factors,ssp) %>% summarise(sigma=median(sigma)) %>% 
  #  group_by(t,ssp) %>% mutate(sum=med/sum(med)) %>% 
  ggplot() +
  geom_bar(aes(x=as.factor(ttoyear(t)),y=-sigma*100,fill=factors),position="stack",stat="identity",color="black") +
  facet_grid(ssp~n,) +
  xlab('') + ylab('Inequality increase \n no overshoot vs overshoot [Delta gini points]') +
  theme_pubr()
ggsave("SI_fig3.png",width=10,height=13,path=str_c(main_directory,"/graphs"),dpi=400)

require(arules)
ggplot(dacum %>% filter(O=="yes" & B=="700"  & n!="World") %>% mutate(ssp=paste0("ssp",ssp)) %>% full_join(reg) %>% filter(ISO!='ATA' & !is.na(ISO)) %>% ungroup() %>% mutate(useper=discretize(round(useper*100),breaks=4)), aes(x = long, y = lat) ) +
  geom_polygon(aes(group = group, fill = useper),color='black',size=.1)+
  #  geom_point(aes(x=long,y=lat)) +
  scale_fill_viridis_d() +
  theme_void()+
  facet_wrap(ssp~.,) + ggtitle('regional cumulative negative emissions [% of global]') +
  theme(legend.position = "bottom", strip.text.x = element_text(size=12, face="bold"),legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
ggsave("SI_fig4.png",width=10,height=8,path=str_c(main_directory,"/graphs"),dpi=400)

##### figure 8: correlations
scatp <- inner_join(COST_CDR %>% filter(ttoyear(t) <= 2100 & B=="650" & O=="yes" & n!="World"),E_NEG) %>% 
                     inner_join(ABATECOST %>% select_at(c("t","n",file_group_columns,"abcost")))  %>% 
                     inner_join(YGROSS) %>% 
                     inner_join(dacum %>% rename(usecum=use)) %>% 
                     inner_join(emabcum) %>%
                     inner_join(pop) %>% 
  inner_join(ginit) %>% 
  inner_join(left_join(el_coeff %>% filter(ineq_elast %in% c("tax","abatement","carbon_rev") ) %>% rename(ela=value),EIND) %>% 
                left_join(E_NEG) %>% pivot_wider(names_from=ineq_elast,values_from=ela) %>% 
                mutate(weight= if_else(eind>use,1,eind/use)) %>%
                mutate(avg = carbon_rev - abatement*weight- tax*(1-weight) ) %>% 
                select(-use) )

fit_netela <- scatp %>% select(t,n,ssp,weight) %>% 
  right_join(ineq_weights %>% filter(ttoyear(t) <= 2100 & B=="650" & O=="yes" & n!="World")) %>% 
               pivot_wider(names_from="ineq_elast",values_from="value") %>% 
  inner_join(quantiles_ref %>% rename(q=value)) %>% 
  mutate(y=carbon_rev - weight*abatement - (1-weight)*tax ) %>% 
  group_by(ssp,t,n) %>%
  summarise(eta=coef(nls( y ~I( q^power/sum(q^power) ),start=list(power=1.5)))[[1]] )

ggplot(scatp %>% inner_join(fit_netela) %>% filter(ttoyear(t) %in% c(2050,2075,2100))) +
  geom_point(aes(x=eta,y=avg,color=ssp)) +
  facet_wrap(ssp~.,) +
  xlab('estimated elasticity of net removal') + ylab('Simplified calculated elasticity of net removal') + theme_pubr() 

ggplot(scatp %>% inner_join(fit_netela) %>% filter(ttoyear(t) %in% c(2050,2075,2100))) +
  geom_point(aes(x=ygross/pop*1000,y=eta,color=ssp,size=gini)) +
  facet_grid(.~ttoyear(t),) +
  xlab('GDP per capita') + ylab('Regressivity of 1$ of removal') + theme_pubr() 
ggsave("SI_fig6.png",width=12,height=15,path=str_c(main_directory,"/graphs"),dpi=400)

ggplot(scatp %>% filter(use>0.00001 & ttoyear(t)>=2050) %>% inner_join(shapleyrefall %>% filter(factor %in% c("cdrrev","cdrcost","ctx","transfer","gentax")) %>%
                              group_by(t,n,ssp) %>% 
                              summarise(ginicdr=sum(value)) %>% 
                              mutate(ssp=as.character(ssp))) %>% 
         inner_join(fit_netela) %>%
         inner_join(CPRICE) %>% inner_join(MRC %>% mutate(mrc=value)) ) +
  geom_point(aes(x=eta*(cprice-mrc)/cprice-carbon_rev*(1-(cprice-mrc)/cprice),y=ginicdr/use)) +
#  geom_smooth(data=.%>%filter(ginicdr/use<0.2),aes(x=eta,y=ginicdr/use),method="lm") +
#  geom_text_repel(aes(x=eta,y=ginicdr/use,label=n)) + #(,
  scale_color_viridis_c() +
  xlab('') + ylab('Delta gini') + theme_pubr()  

data <- scatp %>% filter(use>0.00001) %>% inner_join(shapleyrefall %>% filter(factor %in% c("cdrrev","cdrcost","ctx","transfer","gentax")) %>%
                                                                  group_by(t,n,ssp) %>% 
                                                                  summarise(ginicdr=sum(value)) %>% 
                                                                  mutate(ssp=as.character(ssp))) %>% 
  inner_join(fit_netela) %>%
  inner_join(CPRICE) %>% inner_join(MRC %>% mutate(mrc=value)) %>%
  mutate(x1=eta,x2=carbon_rev,x3=(cprice-mrc)/cprice,y=ginicdr/use)
require(plm)
summary(plm(y ~ x1 + x2 + x3,data=data, model="within",effects="twoways",index=c("n","t")))
summary(plm(y ~ abatement + carbon_rev + x3 + tax + use,data=data, model="within",effects="twoways",index=c("n","t")))
cor(data %>% select(x1,x2,x3) )

ggplot(scatp %>% inner_join(fit_netela)  %>% filter(ttoyear(t) %in% c(2050,2075,2100))) +
  geom_point(aes(x=gini,y=eta,color=ssp,size=ygross/pop)) +
  geom_hline(yintercept=0,color="grey") +
  geom_hline(data=.%>%group_by(t,ssp)%>%summarise(eta=median(eta)),aes(yintercept=eta,color=ssp),linetype="dotted") +
#  geom_violin(aes(x=0.8+as.numeric(ssp)/10,y=eta,color=ssp),draw_quantiles = c(0.25, 0.5, 0.75),fill=NA) +
  facet_grid(.~ttoyear(t),) +
  xlab('Exogenous gini') + ylab('Regressivity of 1$ of removal') + theme_pubr() 
ggsave("SI_fig7.png",width=12,height=15,path=str_c(main_directory,"/graphs"),dpi=400)

## regressivity of financing NETs, per region and SSP
require(arules)
ggplot(fit_netela %>% filter(n!="World" & ttoyear(t) %in% c(2050,2075,2100)) %>% full_join(reg) %>% filter(ISO!='ATA' & !is.na(ISO)) %>% ungroup() %>% mutate(eta=discretize(round(eta*100),breaks=5)), aes(x = long, y = lat) ) +
  geom_polygon(aes(group = group, fill = eta),color='black',size=.1)+
  #  geom_point(aes(x=long,y=lat)) +
  scale_fill_viridis_d() +
  theme_void()+
  facet_grid(ssp~ttoyear(t),) +
  theme(legend.position = "bottom", strip.text.x = element_text(size=12, face="bold"),legend.title = element_blank(),plot.title = element_text(hjust = 0.5))
ggsave("SI_fig8.png",width=12,height=15,path=str_c(main_directory,"/graphs"),dpi=400)

## normalized differences in elasticities
ggplot(ineq_weights %>% filter(ttoyear(t) %in% c(2075) & ssp==2 & O=="yes" & B=="700" & !ineq_elast %in% c("damages") ) %>% left_join(quantiles_ref %>% rename(q=value)) %>% mutate(value=value/q) %>% 
         pivot_wider(names_from="ineq_elast") %>% mutate(`Carbon tax recycling`=1+abatement-redist,`Abatement costs`=abatement, `NET profits by other taxes` = 1-carbon_rev+tax, `NET profits by carbon tax` = -carbon_rev+abatement+1 ) %>% select(-abatement,-redist,-carbon_rev,-tax) %>% pivot_longer(c(`Carbon tax recycling`, `Abatement costs`, `NET profits by other taxes`, `NET profits by carbon tax`),names_to="ineq_elast") %>% 
         mutate(dist=as.factor(dist)) %>% mutate(dist=fct_relevel(dist,paste0("D",seq(1,10)))) %>% group_by(ineq_elast,ssp,dist) %>% summarise(med=median(value),min=quantile(value,0.1),max=quantile(value,0.9)) ) +
  geom_hline(yintercept=1,color="black",linetype=3,size=1.2) +
  geom_line(aes(x=dist,y=med,color=ineq_elast,group=ineq_elast),size=2) +
  geom_ribbon(aes(x=dist,ymin=min,ymax=max,fill=ineq_elast,group=ineq_elast),alpha=0.2) +
  geom_point(aes(x=dist,y=med,color=ineq_elast),size=2,shape=21,fill="white") + theme_pubr() + xlab('Income distribution') + ylab('share of costs accruing to each decile per net flow, \n normalized by decile share of GDP') + theme(legend.title = element_blank()) +
  scale_fill_manual(values=c("#F8766D","#F564E3","#B79F00","#619CFF")) +
  scale_color_manual(values=c("#F8766D","#F564E3","#B79F00","#619CFF")) + theme(legend.position = "bottom")+ guides(color=guide_legend(nrow=2))
ggsave("SI_fig9.png",width=9,height=7.5,path=str_c(main_directory,"/graphs"),dpi=400)

##### carbon prices
cpricesreg <- ggplot(CPRICE %>% filter(O=="yes" & B=="700" & n!="World" & ssp==2 & ttoyear(t)<=2100) %>% inner_join(cleanreg)) +
  geom_line(aes(x=ttoyear(t),y=cprice,color=n),size=1.2) +
  geom_text_repel(data=.%>%filter(ttoyear(t)==2100),aes(x=ttoyear(t),y=cprice,color=n,label=region)) +
  scale_color_manual(values=region_palette_ed57) +
  facet_wrap(Continent_Name~.,) +
  xlab('') + ylab('Carbon price [US$/tonCO2]') + theme_pubr() + theme(legend.position="none")

ggarrange(cpricesreg,regmap,nrow=2,heights = c(0.6,0.4))
ggsave("SI_fig8.png",width=12,height=15,path=str_c(main_directory,"/graphs"),dpi=400)

### GDPc vs baseline Gini
regmap <- ggplot(reg %>% filter(ISO!='ATA' & !is.na(ISO)), aes(x = long, y = lat) ) +
  geom_polygon(aes(group = group, fill = n),color='black',size=.1)+
  scale_fill_manual(values=region_palette_ed57) +
  theme_void()+
  theme(legend.position = "none", strip.text.x = element_text(size=12, face="bold"),plot.title = element_text(hjust = 0.5))

giniedg <- ggplot(ginit %>% filter(O=="yes" & B=="700" & n!="World" & ttoyear(t) %in% c(2025,2050,2075,2100) & ssp==2) %>% inner_join(Y) %>% inner_join(pop %>% make_scen()) %>% inner_join(cleanreg)) +
  geom_point(aes(x=gdp/value*1000,y=gini,color=n)) +
  geom_path(aes(x=gdp/value*1000,y=gini,color=n,group=n),size=1.2,
            arrow = arrow(type = "open", angle = 30, length = unit(0.1, "inches")) ) +
  geom_text_repel(data=.%>%filter(ttoyear(t)==2100),aes(x=gdp/value*1000,y=gini,color=n,label=region)) +
  geom_text_repel(data=.%>%filter(region == "South Africa"),aes(x=gdp/value*1000,y=gini,label=ttoyear(t))) +
  scale_color_manual(values=region_palette_ed57) +
  facet_wrap(Continent_Name~.,) +
  xlab('GDP per capita [US$/yr]') + ylab('Gini index') + theme_pubr() + theme(legend.position="none")

ggarrange(giniedg,regmap,nrow=2,heights = c(0.6,0.4))
ggsave("SI_fig5.png",width=12,height=15,path=str_c(main_directory,"/graphs"),dpi=400)

