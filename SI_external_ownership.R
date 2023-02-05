# NB require make_df to run

##### external ownership, scenario
get_witch_simple("share_ext_fin_pos")
share_ext_fin_pos <- share_ext_fin_pos %>% make_scen()

a <- ginit %>% filter(file %in% c("ssp2_B700e_DISTgeo_COSTbest_TAXbest_NEGbest",
                                  "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest")) %>%
  full_join(share_ext_fin_pos %>% filter(pos=="assets")) %>%
  group_by(t,n) %>%
  mutate(gini0 = gini - gini[file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest"]) %>%
  filter(file=="ssp2_B700e_DISTgeo_COSTbest_TAXbest_NEGbest" & ttoyear(t) %in% c(2075,2100) ) 
a$n = factor(a$n, levels = unique(a[order(-a$value),]$n))

ggplot(a) +
  geom_point(aes(x=n,y=gini0*100,color=as.factor(ttoyear(t)))) + coord_flip()

a <- Y %>% filter(file %in% c("ssp2_B700e_DISTgeo_COSTbest_TAXbest_NEGbest",
                              "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") & n!="World") %>%
  full_join(share_ext_fin_pos %>% filter(pos=="assets")) %>%
  group_by(t,n) %>%
  mutate(gdp0 = (gdp - gdp[file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest"])/gdp[file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest"] ) %>%
  filter(file=="ssp2_B700e_DISTgeo_COSTbest_TAXbest_NEGbest" & ttoyear(t) %in% c(2075,2100) ) 

a$n = factor(a$n, levels = unique(a[order(-a$value),]$n))
ggplot(a) +
  geom_point(aes(x=n,y=gdp0*100,color=as.factor(ttoyear(t)))) + coord_flip()

get_witch_simple("POOL_FIN")
POOL_FIN <- POOL_FIN %>% make_scen()
check <- COST_CDR %>% 
  filter(file %in% c("ssp2_B700e_DISTgeo_COSTbest_TAXbest_NEGbest") & n!="World")  %>%
  inner_join(REV_CDR) %>% 
  group_by(file,t) %>%
  summarise(pool_in=sum((cdrrev-cdrcost)*0.05)) %>% inner_join(POOL_FIN) %>%
  mutate(check=value/pool_in)

check2 <- Y_DIST %>% 
  filter(file %in% c("ssp2_B700e_DISTgeo_COSTbest_TAXbest_NEGbest")) %>%
  group_by(file,t,n) %>%
  summarise(value=sum(value)) %>% 
  inner_join(Y) %>%
  mutate(check2=gdp/value)

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
  inner_join(ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% rename(ela=value)) %>%
  group_by(t,file) %>%
  mutate(ynew = gdp - (revd-costd)*frac_abroad + sum((revd-costd)*frac_abroad) * ela * asset_share ) %>% select(-asset_share,-ela) %>%
  mutate(frac=frac_abroad) %>%
  rbind(new_y) }

check <- new_y %>% 
  group_by(t,file,frac) %>%
  summarise(check=sum(ynew)/sum(gdp),check2=sum(ynew-gdp))

gini_new <- new_y %>% 
  group_by(t,n,file,frac) %>% 
  summarise(gini=reldist::gini(ynew/sum(ynew)), gini0=reldist::gini(gdp/sum(gdp))) %>% 
  ungroup() %>%
  mutate(ginirel=gini-gini0) %>% inner_join(share_ext_fin_pos %>% filter(pos=="assets") %>% rename(asset_share=value) %>% select(t,n,asset_share)) %>% inner_join(scenarios) %>%
gini_new$n = factor(gini_new$n, levels = unique(gini_new[order(-gini_new$asset_share),]$n)) 
gini_new$DIST = factor(gini_new$DIST)
levels(gini_new$DIST) <- list("Global north"="geo","Global south"="epc")

# within country gini index
plot_gini <- gini_new %>% filter(ttoyear(t) %in% c(2050,2075,2100)) %>% 
  group_by(t,file,frac,DIST) %>%
  mutate(breaks=discretize(asset_share*100,breaks=4,method="frequency")) %>%
  group_by(breaks,t,file,frac,DIST) %>%
  summarise(med=median(ginirel),min=quantile(ginirel,0.33),max=quantile(ginirel,0.66)) 

ggplot(plot_gini) +
  geom_point(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t))),size=3) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_ribbon(aes(x=breaks,ymin=min*100,ymax=max*100,fill=as.factor(ttoyear(t)),group=as.factor(ttoyear(t))),alpha=0.2) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  facet_grid(DIST~frac,scales="free_x") + theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("Inequality variation [Gini points]") + xlab("% of global foreign assets held by country n") +
  guides(color = guide_legend(title=NULL),fill = guide_legend(title=NULL))


#within country gdp loss
plot_y <- new_y %>% filter(ttoyear(t) %in% c(2050,2075,2100)) %>% 
  inner_join(share_ext_fin_pos %>% filter(pos=="assets") %>% rename(asset_share=value) %>% select(t,n,asset_share)) %>% inner_join(scenarios) %>%
  group_by(t,file,frac,DIST) %>%
  mutate(breaks=discretize(asset_share*100,breaks=4,method="frequency")) %>%
  group_by(breaks,t,file,frac,DIST) %>%
  summarise(med=median( (ynew-gdp)/gdp ),min=quantile((ynew-gdp)/gdp,0.33),max=quantile((ynew-gdp)/gdp,0.66)) %>%
  mutate(DIST=as.factor(DIST))
levels(plot_y$DIST) <- list("Global north"="geo","Global south"="epc")

ggplot(plot_y) +
  geom_point(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t))),size=3) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_ribbon(aes(x=breaks,ymin=min*100,ymax=max*100,fill=as.factor(ttoyear(t)),group=as.factor(ttoyear(t))),alpha=0.2) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  facet_grid(DIST~frac,scales="free_x") + theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("GDP variation [% of national ownership scenario]") + xlab("% of global foreign assets held by country n") +
  guides(color = guide_legend(title=NULL),fill = guide_legend(title=NULL))

plot_y2 <- new_y %>% filter(ttoyear(t) %in% c(2075) & frac==0.1) %>% 
  inner_join(share_ext_fin_pos %>% filter(pos=="assets") %>% rename(asset_share=value) %>% select(t,n,asset_share)) %>% inner_join(scenarios) %>%
  mutate(loss=ynew-gdp) %>% group_by(t,n,file,frac,DIST) %>% summarise(loss=sum(loss)) %>%
  ungroup() %>% inner_join(pop) %>% mutate(loss=loss/pop*1e6) %>% 
  group_by(t,frac) %>%
  mutate(breaks=discretize(loss,breaks=8,method="frequency")) %>%
  mutate(DIST=as.factor(DIST))
levels(plot_y2$DIST) <- list("Global north"="geo","Global south"="epc")

ggplot(inner_join(reg %>% filter(iso3!="ATA"),plot_y2),aes(x=long,y=lat)) +
  geom_polygon(aes(group = group, fill = breaks),color='black',size=.1) +
  theme_void()+
  scale_fill_manual(values=brewer.pal(n = 8, name = "RdBu")) +
  theme(legend.position="top",strip.text.x = element_text(size=12, face="bold"),plot.title = element_text(hjust = 0.5)) +
  facet_grid(DIST~.) +
  labs(fill="Gain and losses [US$/(pc*yr)]") 
ggsave("SIC_fig1.png",width=10,height=12,dpi=320)

#global variation in the theil index
theil <- new_y %>% inner_join(pop) %>%
  group_by(t,file,frac) %>%
  summarise(Within=dineq::mld_decomp(ynew*1e6/pop, n, pop)$mld_decomp$mld_within-dineq::mld_decomp(gdp*1e6/pop, n, pop)$mld_decomp$mld_within, Between=dineq::mld_decomp(ynew*1e6/pop, n, pop)$mld_decomp$mld_between-dineq::mld_decomp(gdp*1e6/pop, n, pop)$mld_decomp$mld_between)  %>% 
  inner_join(scenarios) %>% mutate(DIST=factor(DIST))
levels(theil$DIST) <- list("Global north"="geo","Global south"="epc")

ggplot(theil %>% pivot_longer(c(Within,Between),names_to="Inequality contribution")) +
  geom_area(aes(x=ttoyear(t),y=value*100,fill=`Inequality contribution`),color="black") +
  geom_line(data=.%>%group_by(t,DIST,frac) %>% summarise(value=sum(value)),aes(x=ttoyear(t),y=value*100),color="blue",linewidth=1.2,linetype="dotted") +
  facet_grid(DIST~frac,scales="free") + theme_pubr()  + ylab("Inequality variation [Theil index points]") + xlab("") +
  geom_hline(yintercept=0) +
  guides(color = guide_legend(title=NULL))
ggsave("SIC_fig3.png",width=10,height=12,dpi=320)

