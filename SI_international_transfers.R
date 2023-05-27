# NB require make_df to run

##### partial 
new_y <- tibble()
for (redist_share in c(0.05,0.1,0.15,0.2)) {
new_y <- Y_DIST %>% rename(gdp=value) %>%
  filter(file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                     "ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest") ) %>%
  inner_join(REV_CDR)  %>%
  inner_join(TRANSFER)  %>%
  inner_join(GENTAX)  %>%
  inner_join(CTX) %>%
  inner_join(E_NEG %>% group_by(t,file) %>% mutate(eff=use/sum(use))) %>% 
#  inner_join(EIND %>% group_by(t,file) %>% mutate(resp=eind/sum(eind))) %>% 
#  inner_join(EIND %>% filter(ttoyear(t) == 2020) %>% group_by(file) %>% mutate(resp=eind/sum(eind))) %>% 
  inner_join(YGROSS %>% group_by(t,file) %>% mutate(resp=ygross/sum(ygross))) %>% 
  group_by(file,t) %>%
  mutate(pool=redist_share*max(sum(cdrrev),0)) %>%
  mutate(efffrac = ifelse(sum(gentax)>(pool*resp),1,sum(gentax)/(pool*eff) ) ) %>%
  mutate(respfrac = ifelse(sum(transfer)>(pool*resp),1,sum(transfer)/(pool*resp) ) ) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% rename(crev=value) %>% select(-ineq_elast) ) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="tax") %>% rename(tax=value) %>% select(-ineq_elast) ) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="redist") %>% rename(redist=value) %>% select(-ineq_elast) ) %>%
  mutate(ynew = gdp + pool * eff * (tax*efffrac + redist*(1-efffrac)) - pool * resp * (redist*respfrac + tax*(1-respfrac)) )  %>% 
  mutate(share=redist_share) %>%
  rbind(new_y) %>% ungroup() }

check <- new_y %>% 
  group_by(t,file,share) %>%
  summarise(check=sum(ynew)/sum(gdp),check2=sum(ynew-gdp))

gini_new <- new_y %>% 
  inner_join(quantiles_ref %>% rename(qref=value)) %>%
  group_by(t,n,file,share,eff,resp) %>% 
  summarise(gini=reldist::gini(ynew/sum(ynew))*100, gini0=reldist::gini(gdp/sum(gdp))*100,gini00=reldist::gini(qref)*100) %>% 
  ungroup() %>%
  mutate(ginirel= gini-gini0, ginirel0 = gini-gini00 )  %>% inner_join(scenarios) 
gini_new$DIST = factor(gini_new$DIST)
levels(gini_new$DIST) <- list("Global north"="geo","Global south"="epc")

# within country gini index
plot_gini <- gini_new %>% filter(ttoyear(t) %in% c(2050,2075,2100)) %>% 
  mutate(breaks=arules::discretize((eff-resp)*100,breaks=7,method="frequency")) %>%
  group_by(breaks,t,file,share,DIST) %>%
  summarise(med=median(ginirel),min=quantile(ginirel,0.33),max=quantile(ginirel,0.66)) 

fig2a <- ggplot(plot_gini) +
  geom_point(aes(x=breaks,y=med,color=as.factor(ttoyear(t))),size=3) +
  geom_path(aes(x=breaks,y=med,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_ribbon(aes(x=breaks,ymin=min,ymax=max,fill=as.factor(ttoyear(t)),group=as.factor(ttoyear(t))),alpha=0.2) +
  geom_path(aes(x=breaks,y=med,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_path(aes(x=breaks,y=med,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  facet_grid(DIST~share,scales="free_x") + theme_pubr() + geom_hline(yintercept=0,color="grey") + 
  ylab("Inequality variation [Gini points]") + xlab("") +
  guides(color = guide_legend(title=NULL),fill = guide_legend(title=NULL))

#within country gdp loss
plot_y <- new_y %>% ungroup() %>% filter(ttoyear(t) %in% c(2050,2075,2100)) %>%
  mutate(breaks=arules::discretize((eff-resp)*100,breaks=5,method="frequency")) %>%
  group_by(breaks,t,file,share,DIST) %>%
  summarise(med=median( (ynew-gdp)/gdp ),min=quantile((ynew-gdp)/gdp,0.33),max=quantile((ynew-gdp)/gdp,0.66)) %>%
  mutate(DIST=as.factor(DIST))
levels(plot_y$DIST) <- list("Global north"="geo","Global south"="epc")

fig2b <- ggplot(plot_y) +
  geom_point(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t))),size=3) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_ribbon(aes(x=breaks,ymin=min*100,ymax=max*100,fill=as.factor(ttoyear(t)),group=as.factor(ttoyear(t))),alpha=0.2) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  facet_grid(DIST~share,scales="free_x") + theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("GDP variation [% points]") + xlab("") +
  guides(color = guide_legend(title=NULL),fill = guide_legend(title=NULL))

fig2 <- ggarrange(fig2a,fig2b,nrow=2,common.legend=TRUE,labels=c("a","b"))
ggsave("SIC_fig1.png",width=11.7,height=10,dpi=320)

plot_y2 <- new_y %>% filter(ttoyear(t) %in% c(2075) & share==0.2) %>%
  rowwise() %>% mutate(loss=ynew-gdp) %>% group_by(t,n,file,share,DIST) %>% summarise(loss=sum(loss)) %>%
  ungroup() %>% inner_join(pop) %>% mutate(loss=loss/pop*1e6) %>% 
  group_by(t,share) %>%
  mutate(breaks_loss=arules::discretize(loss,breaks=8,method="frequency")) 

require(broman)
ggplot(inner_join(reg %>% filter(iso3!="ATA"),plot_y2,relationship = "many-to-many"),aes(x=long,y=lat)) +
  geom_polygon(aes(group = group, fill = breaks_loss),color='black',size=.1) +
  theme_void()+
  scale_fill_manual(values=RColorBrewer::brewer.pal(n = 8, name = "RdBu")) +
  theme(legend.position="top",strip.text.x = element_text(size=12, face="bold"),plot.title = element_text(hjust = 0.5)) +
  facet_grid(DIST~.) +
  labs(fill="Gain and losses [US$/(pc*yr)]") 
ggsave("SIC_fig2.png",width=10,height=12,dpi=320)

#global variation in the theil index
theil <- new_y %>% inner_join(pop) %>%
  group_by(t,file,share) %>%
  summarise(Within=dineq::mld_decomp(ynew*1e6/pop, n, pop)$mld_decomp$mld_within-dineq::mld_decomp(gdp*1e6/pop, n, pop)$mld_decomp$mld_within, Between=dineq::mld_decomp(ynew*1e6/pop, n, pop)$mld_decomp$mld_between-dineq::mld_decomp(gdp*1e6/pop, n, pop)$mld_decomp$mld_between)  %>% 
  inner_join(scenarios) %>% mutate(DIST=factor(DIST))
levels(theil$DIST) <- list("Global north"="geo","Global south"="epc")

ggplot(theil %>% pivot_longer(c(Within,Between),names_to="Inequality contribution")) +
  geom_area(aes(x=ttoyear(t),y=value*100,fill=`Inequality contribution`),color="black") +
  geom_line(data=.%>%group_by(t,DIST,share) %>% summarise(value=sum(value)),aes(x=ttoyear(t),y=value*100),color="blue",linewidth=1.2,linetype="dotted") +
  facet_grid(DIST~share) + theme_pubr()  + ylab("Inequality variation [MLD index points]") + xlab("") +
  geom_hline(yintercept=0) +
  guides(color = guide_legend(title=NULL))
ggsave("SIC_fig3.png",width=10,height=8,dpi=320)

library(countrycode)