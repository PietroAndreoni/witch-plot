# NB require make_df to run

##### external ownership, scenario
get_witch_simple("el_coeff")
el_coeff <- el_coeff %>% make_scen()

##### profit cap imposed to CDR companies
new_y <- tibble()
prof <- tibble()
for (profit_cap in c(0,0.25,0.5,0.75,1)) {
new_y <- Y_DIST %>% rename(gdp=value) %>%
  filter(file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest")) %>%
  inner_join(REV_CDR)  %>%
  inner_join(COST_CDR) %>%
  inner_join(EIND) %>%
  inner_join(E_NEG) %>%
  inner_join(GENTAX)  %>%
  group_by(file,t,n) %>%
  mutate(prof=profit_cap * sum(max(cdrrev-cdrcost,0)) ) %>%
  mutate(taxfrac = ifelse(sum(gentax)>prof,1,sum(gentax)/prof ) ) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% rename(crev=value) %>% select(-ineq_elast) ) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="tax") %>% rename(tax=value) %>% select(-ineq_elast) ) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="abatement") %>% rename(ctax=value) %>% select(-ineq_elast) ) %>%
  mutate(ynew = gdp + prof*(tax*taxfrac + ctax*(1-taxfrac) - crev ) ) %>% 
  mutate(cap=paste0(as.character((1-profit_cap)*100)," %")) %>%
  select(t,n,file,dist,cap,ynew,gdp,taxfrac) %>%
  rbind(new_y) 

prof <- REV_CDR  %>%
  filter(file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") & ttoyear(t)>=2020) %>%
  inner_join(COST_CDR) %>%
  rowwise() %>% mutate(prof=ifelse((cdrrev-cdrcost)>=0,(1-profit_cap)*(cdrrev-cdrcost),cdrrev-cdrcost)) %>%
  group_by(file,n,O) %>% 
  complete(t=seq(min(t), max(t), 0.2)) %>% 
  mutate(prof=approxfun(t, prof)(t),cdrcost=approxfun(t, cdrcost)(t),) %>%
  group_by(file,n) %>% 
  mutate(profit_margin=cumsum(prof / (1 + 0.06)^(ttoyear(t)-2020) ) / cumsum(cdrcost / (1 + 0.06)^(ttoyear(t)-2020) ) ) %>%  
  mutate(cap=paste0(as.character((1-profit_cap)*100)," %")) %>%
  rbind(prof)  }

new_y <- new_y %>% bind_rows(YGROSS_DIST %>%
  filter(file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest")) %>%
  inner_join(COST_CDR) %>%
  inner_join(ABATECOST) %>%
  inner_join(Y_DIST %>% rename(gdp=value)) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="abatement") %>% rename(ctax=value) %>% select(-ineq_elast) ) %>%
  inner_join(quantiles_ref %>% rename(qref=value) ) %>%
  group_by(t,file) %>%
  mutate(ynew = ygrossd - ctax*(abcost+cdrcost) ) %>% 
  mutate(cap="neutral") %>% 
  select(t,n,file,dist,cap,ynew,gdp) )

  
check <- new_y %>% 
  group_by(t,file,cap) %>%
  summarise(check=sum(ynew)/sum(gdp),check2=sum(ynew-gdp))

gini_new <- new_y %>% 
  inner_join(quantiles_ref %>% rename(qref=value)) %>%
  group_by(t,n,file,cap) %>% 
  summarise(gini=reldist::gini(ynew/sum(ynew))*100, gini0=reldist::gini(gdp/sum(gdp))*100,gini00=reldist::gini(qref)*100) %>% 
  ungroup() %>%
  mutate(ginirel= gini-gini0, ginirel0 = gini-gini00 )  %>% inner_join(scenarios) 
gini_new$DIST = factor(gini_new$DIST)

plot_gini <- gini_new  %>% 
  inner_join(share) %>%
  group_by(t,file,cap,breaks) %>%
  summarise(med=median(ginirel0),min=quantile(ginirel0,0.33),max=quantile(ginirel0,0.66)) %>%
  mutate(breaks=as.factor(as.character(breaks)))

plot_frac <- new_y  %>% 
  inner_join(share) %>%
  inner_join(el_coeff %>% filter(ineq_elast=="abatement")) %>%
  mutate(ela_eq= taxfrac*value + (1-taxfrac)*1.4) %>%
  group_by(t,file,cap,breaks) %>%
  summarise(med=median(ela_eq))
            
breaksnames <- c("[0.147,3.45)"="Low","[3.45,8.12)"="Medium","[8.12,18.1)"="High","[18.1,65.9]"="Very high")

# Update the levels of the 'breaks' column in your data frame
plot_gini$breaks <- factor(plot_gini$breaks, levels = names(breaksnames))

# Assign the new level names
levels(plot_gini$breaks) <- breaksnames
plot_gini$breaks <- ordered(plot_gini$breaks, levels=c("Low","Medium","High","Very high"))

ggplot(plot_gini) +
  geom_point(data=.%>%filter(ttoyear(t)%%10==0 & cap!="neutral"),aes(x=ttoyear(t),y=med,color=breaks,alpha=cap),size=3) +
  geom_line(data=.%>%filter(cap!="neutral"),aes(x=ttoyear(t),y=med,color=breaks,group=cap,alpha=cap),linewidth=2) +
  geom_point(data=.%>%filter(ttoyear(t)%%10==0 & cap=="neutral"),aes(x=ttoyear(t),y=med),color="black",size=3) +
  geom_line(data=.%>%filter(cap=="neutral"),aes(x=ttoyear(t),y=med,group=cap),color="black",linewidth=2) +
  facet_wrap(breaks~.,) +
  scale_alpha_manual(values=c(0.5,0.6,0.7,0.8,1,1),breaks = c("0 %","25 %","50 %","75 %","100 %","neutral")) +
  scale_color_manual(values= scales::hue_pal()(4) ) +
  ggpubr::theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("Inequality variation [Gini points]") + xlab("") +
  guides(color = "none",alpha = guide_legend(title=NULL))
ggsave("SIPM_fig1.png",width=11.7,height=10,dpi=320)

plot_prof <- prof %>% 
  inner_join(share) %>% mutate(cap = factor(cap,levels=c("0 %","25 %","50 %","75 %","100 %","neutral"),ordered=TRUE))
# Update the levels of the 'breaks' column in your data frame

levels(plot_prof$breaks) <- breaksnames
plot_prof$breaks <- factor(plot_prof$breaks, levels = names(breaksnames))

# Assign the new level names
levels(plot_prof$breaks) <- breaksnames
plot_prof$breaks <- ordered(plot_prof$breaks, levels=c("Low","Medium","High","Very high"))

ggplot(plot_prof ) +
  geom_boxplot(data=.%>%filter(ttoyear(t)==2100),
               aes(x=cap,y=profit_margin*100,fill=breaks)) +
  geom_text(data=.%>%filter(profit_margin>0 & min(profit_margin)) %>% 
              group_by(file,cap,O) %>% 
              summarise(med=median(ttoyear(t)),mmin=quantile(ttoyear(t),0.33),mmax=quantile(ttoyear(t),0.66)),
            aes(x=cap,y=-10,label=paste0(med,"[",mmin,",",mmax,"]"))) +
  geom_hline(yintercept=0,color="grey") +
  theme_pubr() + ylab("Discounted profit margin 2020-2100 [%]") + xlab("") +
  scale_alpha_manual(values=c(0.5,0.6,0.7,0.8,1,1),breaks = c("0 %","25 %","50 %","75 %","100 %","neutral")) +
  scale_fill_manual(values= scales::hue_pal()(4) ) +
  guides(fill = guide_legend(title=NULL))
ggsave("SIPM_fig2.png",width=11.7,height=10,dpi=320)
