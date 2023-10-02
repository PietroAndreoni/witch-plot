# NB require make_df to run

##### external ownership, scenario
share_ext_fin_pos <- tibble(gdxtools::batch_extract( "share_ext_fin_pos",files=paste0(main_directory,"/ext_fin_pos.gdx") )[[1]])
share_ext_fin_pos <- share_ext_fin_pos %>% select(-gdx) 
share_ext_fin_pos$t <- as.numeric(share_ext_fin_pos$t)

##### simple model of external ownership
# percentage of the economy held abroad
new_y_extown <- tibble()
for (frac_abroad in c(0.05,0.1,0.15,0.2)) {
new_y_extown <- Y_DIST %>% rename(gdp=value) %>%
  filter(file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                     "ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest")) %>%
  mutate(frac=frac_abroad) %>%
  inner_join(REV_CDR %>% full_join(ineq_weights %>% filter(ineq_elast=="carbon_rev")) %>% mutate(revd=cdrrev*value) %>% select(-value,-ineq_elast,-cdrrev) ) %>%
  inner_join(COST_CDR %>% full_join(ineq_weights %>% filter(ineq_elast=="carbon_rev")) %>% mutate(costd=cdrcost*value) %>% select(-value,-ineq_elast,-cdrcost) ) %>%
  inner_join(share_ext_fin_pos %>% filter(pos=="assets") %>% rename(asset_share=value)  %>% select(t,n,asset_share)) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% rename(crev=value)) %>%
  group_by(t,file,frac) %>%
  mutate(ynew = gdp - (revd-costd)*frac_abroad + sum((revd-costd)*frac_abroad) * crev * asset_share ) %>% select(-asset_share) %>%
  rbind(new_y_extown) }

check <- new_y_extown %>% 
  group_by(t,file,frac) %>%
  summarise(check=sum(ynew)/sum(gdp),check2=sum(ynew-gdp))

gini_new <- new_y_extown %>% 
  group_by(t,n,file,frac) %>% 
  summarise(gini=reldist::gini(ynew/sum(ynew)), gini0=reldist::gini(gdp/sum(gdp))) %>% 
  ungroup() %>%
  mutate(ginirel=gini-gini0) %>% inner_join(share_ext_fin_pos %>% filter(pos=="assets") %>% rename(asset_share=value) %>% select(t,n,asset_share)) %>% inner_join(scenarios) 
gini_new$n = factor(gini_new$n, levels = unique(gini_new[order(-gini_new$asset_share),]$n)) 
gini_new$DIST = factor(gini_new$DIST)
levels(gini_new$DIST) <- list("Global north"="geo","Global south"="epc")

# within country gini index
maps_extown <- share_ext_fin_pos %>% filter(pos=="assets") %>% rename(asset_share=value)  %>% select(t,n,asset_share) %>% 
  inner_join(pop %>% filter(ttoyear(t)==2020)) %>%
  mutate(breaks=arules::discretize(asset_share/pop,breaks=4,method="frequency",labels=c("low","medium","high","very high"))) %>% 
  select(n,breaks) %>% unique()

plot_gini <- gini_new %>% inner_join(maps_extown) %>%
  group_by(breaks,t,file,frac,DIST) %>%
  summarise(med=median(ginirel),min=quantile(ginirel,0.33),max=quantile(ginirel,0.66)) 

insert <- ggplot(inner_join(reg %>% filter(iso3!="ATA"),maps_extown),aes(x=long,y=lat),relationship = "many-to-many") +
  geom_polygon(aes(group = group, fill = breaks),color='black',size=.1) +
  theme_void()+
  theme(legend.position="top",strip.text.x = element_text(size=7),plot.title = element_text(hjust = 0.5)) +
  labs(fill="International assets per capita")  +
  theme(text = element_text(size = 7))

main <- ggplot(plot_gini) +
  geom_point(data=.%>%filter(ttoyear(t)%%10==0),aes(x=ttoyear(t),y=med*100,color=breaks,alpha=as.character(frac) ),size=3) +
  geom_line(aes(x=ttoyear(t),y=med*100,color=breaks,group=frac,alpha=as.character(frac) ),linewidth=2) +
  scale_alpha_manual(values=c(0.3,0.5,0.7,1),labels=c("5 %","10 %","15 %","20 %")) +
  facet_grid(DIST~breaks,) +
  ggpubr::theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("Inequality variation [Gini points]") + xlab("") +
  guides(color = "none",alpha = guide_legend(title=NULL)) +
  theme(text = element_text(size = 7))

fig2a <- cowplot::ggdraw() +
  cowplot::draw_plot(main) +
  cowplot::draw_plot(insert, x = 0.35, y = .25, width = .3, height = .25)

#within country gdp loss
gdp_loss <- new_y_extown %>% 
  group_by(t,n,file,frac) %>%
  summarise(gdploss=(sum(ynew)-sum(gdp))/sum(gdp)) %>%
  inner_join(scenarios)

plot_y <- gdp_loss %>% filter(ttoyear(t) %in% c(2050,2075,2100)) %>% 
  inner_join(maps_extown) %>%
  group_by(breaks,t,file,frac,DIST) %>%
  summarise(med=median(gdploss),min=quantile(gdploss,0.33),max=quantile(gdploss,0.66)) %>%
  mutate(DIST=as.factor(DIST))
levels(plot_y$DIST) <- list("Global north"="geo","Global south"="epc")

fig2b <- ggplot(plot_y%>% mutate(frac=ordered(paste0(as.character(frac*100)," %"),levels=c("5 %","10 %","15 %","20 %")))) +
  geom_point(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t))),size=3) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_ribbon(aes(x=breaks,ymin=min*100,ymax=max*100,fill=as.factor(ttoyear(t)),group=as.factor(ttoyear(t))),alpha=0.2) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  scale_color_manual(values=c("#2ECBE9","#1E80C1","blue")) +
  scale_fill_manual(values=c("#2ECBE9","#1E80C1","blue")) +
  facet_grid(DIST~frac,scales="free_x") + theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("GDP variation [% points]") + xlab("% of global foreign assets held by country") +
  guides(color = guide_legend(title=NULL),fill = guide_legend(title=NULL))  +
  theme(text = element_text(size = 7))

fig2 <- ggarrange(fig2a,fig2b,nrow=2,labels=c("a","b"))
ggsave("SIEO_fig1.png",width=18,height=18,dpi=300,units="cm")


plot_y2 <- new_y_extown %>% filter(ttoyear(t) %in% c(2075) & frac==0.1) %>% 
  inner_join(share_ext_fin_pos %>% filter(pos=="assets") %>% rename(asset_share=value) %>% select(t,n,asset_share)) %>% inner_join(scenarios) %>%
  mutate(loss=ynew-gdp) %>% group_by(t,n,file,frac,DIST) %>% summarise(loss=sum(loss)) %>%
  ungroup() %>% inner_join(pop) %>% mutate(loss=loss/pop*1e6) %>% 
  group_by(t,frac) %>%
  mutate(breaks_loss=arules::discretize(loss,breaks=8,method="frequency")) %>%
  mutate(DIST=as.factor(DIST))
plot_y2$DIST <- factor(plot_y2$DIST)
levels(plot_y2$DIST) <- list("Global north"="geo","Global south"="epc")

require(broman,ggh4x)

ggplot(inner_join(reg %>% filter(iso3!="ATA"),plot_y2),aes(x=long,y=lat),relationship = "many-to-many") +
  geom_polygon(aes(group = group, fill = breaks_loss),color='black',size=.1) +
#  stat_midpoint(data=.%>%filter(is.na(subregion)),aes(shape=breaks,color=breaks_neg),geom = "point",size=4) +
  theme_void()+
  scale_fill_manual(values=brewer.pal(n = 8, name = "RdBu")) +
  theme(legend.position="top",strip.text.x = element_text(size=12, face="bold"),plot.title = element_text(hjust = 0.5)) +
  facet_grid(DIST~.) +
  labs(fill="Gain and losses [US$/(pc*yr)]") 
ggsave("SIEO_fig2.png",width=18,height=20,dpi=300,units="cm")

#global variation in the theil index
theil <- new_y_extown %>% inner_join(pop) %>%
  group_by(t,file,frac) %>%
  summarise(Within=dineq::mld_decomp(ynew*1e6/pop, n, pop)$mld_decomp$mld_within-dineq::mld_decomp(gdp*1e6/pop, n, pop)$mld_decomp$mld_within, Between=dineq::mld_decomp(ynew*1e6/pop, n, pop)$mld_decomp$mld_between-dineq::mld_decomp(gdp*1e6/pop, n, pop)$mld_decomp$mld_between)  %>% 
  inner_join(scenarios) %>% mutate(DIST=factor(DIST))
levels(theil$DIST) <- list("Global north"="geo","Global south"="epc")

ggplot(theil %>% pivot_longer(c(Within,Between),names_to="Inequality contribution")) +
  geom_area(aes(x=ttoyear(t),y=value*100,fill=`Inequality contribution`),color="black") +
  geom_line(data=.%>%group_by(t,DIST,frac) %>% summarise(value=sum(value)),aes(x=ttoyear(t),y=value*100),color="blue",linewidth=1.2,linetype="dotted") +
  facet_grid(DIST~frac) + theme_pubr()  + ylab("Inequality variation [MLD index points]") + xlab("") +
  geom_hline(yintercept=0) +
  guides(color = guide_legend(title=NULL)) +
  theme(text = element_text(size = 7))
ggsave("SIEO_fig3.png",width=18,height=14,dpi=300,units="cm")
