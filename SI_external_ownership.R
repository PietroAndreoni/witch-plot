# NB require make_df to run

##### external ownership, scenario
get_witch_simple("share_ext_fin_pos")
share_ext_fin_pos <- share_ext_fin_pos %>% make_scen()

##### simple model of external ownership
# percentage of the economy held abroad
new_y <- tibble()
for (frac_abroad in c(0.05,0.1,0.15,0.2)) {
new_y <- Y_DIST %>% rename(gdp=value) %>%
  filter(file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                     "ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest")) %>%
  inner_join(REV_CDR %>% full_join(ineq_weights %>% filter(ineq_elast=="carbon_rev")) %>% mutate(revd=cdrrev*value) %>% select(-value,-ineq_elast,-cdrrev) ) %>%
  inner_join(COST_CDR %>% full_join(ineq_weights %>% filter(ineq_elast=="carbon_rev")) %>% mutate(costd=cdrcost*value) %>% select(-value,-ineq_elast,-cdrcost) ) %>%
  inner_join(share_ext_fin_pos %>% filter(pos=="assets") %>% rename(asset_share=value)  %>% select(t,n,asset_share)) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% rename(crev=value)) %>%
  group_by(t,file) %>%
  mutate(ynew = gdp - (revd-costd)*frac_abroad + sum((revd-costd)*frac_abroad) * crev * asset_share ) %>% select(-asset_share,-ela) %>%
  mutate(frac=frac_abroad) %>%
  rbind(new_y) }

check <- new_y %>% 
  group_by(t,file,frac) %>%
  summarise(check=sum(ynew)/sum(gdp),check2=sum(ynew-gdp))

gini_new <- new_y %>% 
  group_by(t,n,file,frac) %>% 
  summarise(gini=reldist::gini(ynew/sum(ynew)), gini0=reldist::gini(gdp/sum(gdp))) %>% 
  ungroup() %>%
  mutate(ginirel=gini-gini0) %>% inner_join(share_ext_fin_pos %>% filter(pos=="assets") %>% rename(asset_share=value) %>% select(t,n,asset_share)) %>% inner_join(scenarios) 
gini_new$n = factor(gini_new$n, levels = unique(gini_new[order(-gini_new$asset_share),]$n)) 
gini_new$DIST = factor(gini_new$DIST)
levels(gini_new$DIST) <- list("Global north"="geo","Global south"="epc")

# within country gini index
plot_gini <- gini_new %>% 
  group_by(t,file,frac,DIST) %>%
  mutate(breaks=arules::discretize(asset_share*100,breaks=4,method="frequency")) %>%
  group_by(breaks,t,file,frac,DIST) %>%
  summarise(med=median(ginirel),min=quantile(ginirel,0.33),max=quantile(ginirel,0.66)) 
levels(plot_gini$breaks) <- list("low"="[0.0235,0.155)","medium"="[0.155,0.421)","high"="[0.421,1.78)","very high"="[1.78,15.8]")

fig2a <- ggplot(plot_gini %>% filter(ttoyear(t) %in% c(2050,2075,2100))) +
  geom_point(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t))),size=3) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_ribbon(aes(x=breaks,ymin=min*100,ymax=max*100,fill=as.factor(ttoyear(t)),group=as.factor(ttoyear(t))),alpha=0.2) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  facet_grid(DIST~frac,scales="free_x") + theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("Inequality variation [Gini points]") + xlab("") +
  guides(color = guide_legend(title=NULL),fill = guide_legend(title=NULL))

ggplot(plot_gini %>% filter(DIST=="Global north")) +
  geom_point(data=.%>%filter(ttoyear(t)%%10==0),aes(x=ttoyear(t),y=med*100,color=breaks,alpha=as.character(frac) ),size=3) +
  geom_line(aes(x=ttoyear(t),y=med*100,color=breaks,group=frac,alpha=as.character(frac) ),linewidth=2) +
  scale_alpha_manual(values=c(0.3,0.5,0.7,1),labels=c("5 %","10 %","15 %","20 %")) +
  facet_wrap(breaks~.,) +
  ggpubr::theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("Inequality variation [Gini points]") + xlab("") +
  guides(color = "none",alpha = guide_legend(title=NULL))
ggsave("SIH_fig1.png",width=11.7,height=10,dpi=320)
#within country gdp loss
plot_y <- new_y %>% filter(ttoyear(t) %in% c(2050,2075,2100)) %>% 
  inner_join(share_ext_fin_pos %>% filter(pos=="assets") %>% rename(asset_share=value) %>% select(t,n,asset_share)) %>% inner_join(scenarios) %>%
  group_by(t,file,frac,DIST) %>%
  mutate(breaks=discretize(asset_share*100,breaks=4,method="frequency")) %>%
  group_by(breaks,t,file,frac,DIST) %>%
  summarise(med=median( (ynew-gdp)/gdp ),min=quantile((ynew-gdp)/gdp,0.33),max=quantile((ynew-gdp)/gdp,0.66)) %>%
  mutate(DIST=as.factor(DIST))
levels(plot_y$DIST) <- list("Global north"="geo","Global south"="epc")
levels(plot_y$breaks) <- list("low"="[0.0235,0.155)","medium"="[0.155,0.421)","high"="[0.421,1.78)","very high"="[1.78,15.8]")

fig2b <- ggplot(plot_y) +
  geom_point(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t))),size=3) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_ribbon(aes(x=breaks,ymin=min*100,ymax=max*100,fill=as.factor(ttoyear(t)),group=as.factor(ttoyear(t))),alpha=0.2) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  facet_grid(DIST~frac,scales="free_x") + theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("GDP variation [% points]") + xlab("% of global foreign assets held by country") +
  guides(color = guide_legend(title=NULL),fill = guide_legend(title=NULL))

fig2 <- ggarrange(fig2a,fig2b,nrow=2,common.legend=TRUE,labels=c("a","b"))
ggsave("SIC_fig1.png",width=11.7,height=10,dpi=320)


plot_y2 <- new_y %>% filter(ttoyear(t) %in% c(2075) & frac==0.1) %>% 
  inner_join(share_ext_fin_pos %>% filter(pos=="assets") %>% rename(asset_share=value) %>% select(t,n,asset_share)) %>% inner_join(scenarios) %>%
  group_by(t,file,frac,DIST) %>%
  mutate(breaks=discretize(asset_share*100,breaks=4,method="frequency")) %>%
  mutate(loss=ynew-gdp) %>% group_by(t,n,file,frac,DIST,breaks) %>% summarise(loss=sum(loss)) %>%
  ungroup() %>% inner_join(pop) %>% mutate(loss=loss/pop*1e6) %>% 
  group_by(t,frac) %>%
  mutate(breaks_loss=discretize(loss,breaks=8,method="frequency")) %>%
  mutate(DIST=as.factor(DIST)) %>% inner_join(E_NEG) %>% 
  group_by(t,frac) %>%
  mutate(breaks_neg=discretize(use,breaks=4,method="frequency"))

require(broman,ggh4x)

ggplot(inner_join(reg %>% filter(iso3!="ATA"),plot_y2),aes(x=long,y=lat)) +
  geom_polygon(aes(group = group, fill = breaks_loss),color='black',size=.1) +
#  stat_midpoint(data=.%>%filter(is.na(subregion)),aes(shape=breaks,color=breaks_neg),geom = "point",size=4) +
  theme_void()+
  scale_fill_manual(values=brewer.pal(n = 8, name = "RdBu")) +
  theme(legend.position="top",strip.text.x = element_text(size=12, face="bold"),plot.title = element_text(hjust = 0.5)) +
  facet_grid(DIST~.) +
  labs(fill="Gain and losses [US$/(pc*yr)]") 
ggsave("SIC_fig2.png",width=10,height=12,dpi=320)

#global variation in the theil index
theil <- new_y %>% inner_join(pop) %>%
  group_by(t,file,frac) %>%
  summarise(Within=dineq::mld_decomp(ynew*1e6/pop, n, pop)$mld_decomp$mld_within-dineq::mld_decomp(gdp*1e6/pop, n, pop)$mld_decomp$mld_within, Between=dineq::mld_decomp(ynew*1e6/pop, n, pop)$mld_decomp$mld_between-dineq::mld_decomp(gdp*1e6/pop, n, pop)$mld_decomp$mld_between)  %>% 
  inner_join(scenarios) %>% mutate(DIST=factor(DIST))
levels(theil$DIST) <- list("Global north"="geo","Global south"="epc")

ggplot(theil %>% pivot_longer(c(Within,Between),names_to="Inequality contribution")) +
  geom_area(aes(x=ttoyear(t),y=value*100,fill=`Inequality contribution`),color="black") +
  geom_line(data=.%>%group_by(t,DIST,frac) %>% summarise(value=sum(value)),aes(x=ttoyear(t),y=value*100),color="blue",linewidth=1.2,linetype="dotted") +
  facet_grid(DIST~frac,scales="free") + theme_pubr()  + ylab("Inequality variation [MLD index points]") + xlab("") +
  geom_hline(yintercept=0) +
  guides(color = guide_legend(title=NULL))
ggsave("SIC_fig3.png",width=10,height=8,dpi=320)

library(countrycode)
