# NB require make_df to run
oecd <- c("usa","gbr","aus","can","jpn","kor","rus","chn",witchtools::eu27_regions(region_mapping=witchtools::region_mappings$enerdata56))

##### partial 
new_y_inttr <- tibble()
for (redist_share in c(0.05,0.1,0.15,0.2)) {
new_y_inttr <- Y_DIST %>% rename(gdp=value) %>%
  filter(file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                     "ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest") ) %>%
  inner_join(REV_CDR)  %>%
  inner_join(TRANSFER)  %>%
  inner_join(GENTAX)  %>%
  inner_join(CTX) %>%
  inner_join(E_NEG %>% filter(n!="World") %>% group_by(t,file) %>% mutate(eff=use/sum(use))) %>% 
  full_join(YGROSS %>%
              filter(n %in% oecd & file %in%
                       c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                         "ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest")) %>%
              group_by(t,file) %>%
              mutate(resp=ygross/sum(ygross))) %>%
  rowwise() %>% mutate(resp=ifelse(is.na(resp),0,resp)) %>%
  mutate(share=redist_share) %>%
  group_by(file,t,share,dist) %>%
  mutate(pool=redist_share*max(sum(cdrrev),0)) %>%
  rowwise() %>% mutate(poolpos= pool * max((eff - resp),0),  poolneg = pool *  max((resp - eff),0) ) %>%
  group_by(file,t,n,share) %>%
  mutate(efffrac = ifelse(sum(gentax)>(poolpos),1,sum(gentax)/(poolpos) ), respfrac = ifelse(sum(transfer)>(poolneg),1,sum(transfer)/(poolneg) ) ) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% rename(crev=value) %>% select(-ineq_elast) ) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="tax") %>% rename(tax=value) %>% select(-ineq_elast) ) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="redist") %>% rename(redist=value) %>% select(-ineq_elast) ) %>%
  rowwise() %>% mutate(ynew = gdp + poolpos * (tax*efffrac + redist*(1-efffrac)) - poolneg * (redist*respfrac + tax*(1-respfrac))  )  %>% 
  rbind(new_y_inttr) %>% ungroup() }

check <- new_y_inttr %>% 
  group_by(t,file,share) %>%
  summarise(check=sum(ynew)/sum(gdp),check2=sum(ynew-gdp))

check2 <- new_y_inttr %>% 
  group_by(t,file,n,share) %>%
  summarise(check=sum(tax*efffrac + redist*(1-efffrac)),check2=sum(redist*respfrac + tax*(1-respfrac)))

gini_new <- new_y_inttr %>% 
  inner_join(quantiles_ref %>% rename(qref=value)) %>%
  group_by(t,n,file,share,eff,resp) %>% 
  summarise(gini=reldist::gini(ynew/sum(ynew))*100, gini0=reldist::gini(gdp/sum(gdp))*100,gini00=reldist::gini(qref)*100) %>% 
  ungroup() %>%
  mutate(ginirel= gini-gini0, ginirel0 = gini-gini00 )  %>% inner_join(scenarios) 
gini_new$DIST = factor(gini_new$DIST)
levels(gini_new$DIST) <- list("Global north"="geo","Global south"="epc")

# within country gini index

maps_intr <- gini_new %>% 
  group_by(file,DIST,share) %>%
  mutate(breaks=arules::discretize((eff-resp)*100,breaks=3,method="frequency",labels=c("donor","neutral","receiver"))) %>%
  ungroup() %>% select(t,n,file,DIST,breaks) %>% unique()
  
plot_gini <- gini_new %>% 
  filter(ttoyear(t) >= 2025) %>%
  inner_join(maps_intr) %>%
  group_by(breaks,t,file,share,DIST) %>%
  summarise(med=median(ginirel),min=quantile(ginirel,0.33),max=quantile(ginirel,0.66)) 
  
insert <- ggplot(inner_join(reg %>% filter(iso3!="ATA"),maps_intr %>% filter(ttoyear(t)==2075)),aes(x=long,y=lat),relationship = "many-to-many") +
  geom_polygon(aes(group = group, fill = breaks),color='black',size=.1) +
  theme_void()+
  facet_grid(.~DIST,) +
  theme(legend.position="none",strip.text.x = element_text(size=12, face="bold"),plot.title = element_text(hjust = 0.5)) +
  labs(fill="") 

main <- ggplot(plot_gini) +
  geom_point(data=.%>%filter(ttoyear(t)%%10==0),aes(x=ttoyear(t),y=med,color=breaks,alpha=as.character(share) ),size=3) +
  geom_line(aes(x=ttoyear(t),y=med,color=breaks,group=share,alpha=as.character(share) ),linewidth=2) +
  scale_alpha_manual(values=c(0.3,0.5,0.7,1),labels=c("5 %","10 %","15 %","20 %")) +
  facet_grid(DIST~breaks,) +
  ggpubr::theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("Inequality variation [Gini points]") + xlab("") +
  guides(color = "none") + labs(alpha=NULL)

fig2a <- cowplot::ggdraw() +
  cowplot::draw_plot(main ) +
  cowplot::draw_plot(insert, x = 0.35, y = .35, width = .3, height = .2)

#within country gdp loss
gdp_loss <- new_y_inttr %>% 
  group_by(t,n,file,share,eff,resp) %>%
  summarise(gdploss=(sum(ynew)-sum(gdp))/sum(gdp)) %>%
  inner_join(scenarios)

plot_y <- gdp_loss %>% ungroup() %>% filter(ttoyear(t) %in% c(2050,2075,2100)) %>%
  group_by(file,DIST,share) %>%
  mutate(breaks=arules::discretize((eff-resp)*100,breaks=3,method="frequency",labels=c("donor","neutral","receiver"))) %>%
  group_by(breaks,t,file,share,DIST) %>%
  summarise(med=median( gdploss ),min=quantile(gdploss,0.33),max=quantile(gdploss,0.66)) %>%
  mutate(DIST=as.factor(DIST))
levels(plot_y$DIST) <- list("Global north"="geo","Global south"="epc")

fig2b <- ggplot(plot_y %>% mutate(share=ordered(paste0(as.character(share*100)," %"),levels=c("5 %","10 %","15 %","20 %")))) +
  geom_point(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t))),size=3) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_ribbon(aes(x=breaks,ymin=min*100,ymax=max*100,fill=as.factor(ttoyear(t)),group=as.factor(ttoyear(t))),alpha=0.2) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  geom_path(aes(x=breaks,y=med*100,color=as.factor(ttoyear(t)),group=as.factor(ttoyear(t)))) +
  scale_color_manual(values=c("#2ECBE9","#1E80C1","blue")) +
  scale_fill_manual(values=c("#2ECBE9","#1E80C1","blue")) +
  facet_grid(DIST~share ,scales="free_x") + theme_pubr() + geom_hline(yintercept=0,color="grey") + ylab("GDP variation [% points]") + xlab("") +
  guides(color = guide_legend(title=NULL),fill = guide_legend(title=NULL))

fig2 <- ggarrange(fig2a,fig2b,nrow=2,labels=c("a","b"))
ggsave("SIIT_fig1.png",width=11.7,height=10,dpi=320)

plot_y2 <- new_y_inttr %>% filter(ttoyear(t) %in% c(2075) & share==0.1) %>%
  rowwise() %>% mutate(loss=ynew-gdp) %>% group_by(t,n,file,share,DIST) %>% summarise(loss=sum(loss)) %>%
  ungroup() %>% inner_join(pop) %>% mutate(loss=loss/pop*1e6) %>% 
  group_by(t,share) %>%
  mutate(breaks_loss=arules::discretize(loss,breaks=8,method="frequency")) 
plot_y2$DIST <- factor(plot_y2$DIST)
levels(plot_y2$DIST) <- list("Global north"="geo","Global south"="epc")

require(broman)
ggplot(inner_join(reg %>% filter(iso3!="ATA"),plot_y2,relationship = "many-to-many"),aes(x=long,y=lat)) +
  geom_polygon(aes(group = group, fill = breaks_loss),color='black',size=.1) +
  theme_void()+
  scale_fill_manual(values=RColorBrewer::brewer.pal(n = 8, name = "RdBu")) +
  theme(legend.position="top",strip.text.x = element_text(size=12, face="bold"),plot.title = element_text(hjust = 0.5)) +
  facet_grid(DIST~.) +
  labs(fill="Gain and losses [US$/(pc*yr)]") 
ggsave("SIIT_fig2.png",width=10,height=12,dpi=320)

#global variation in the theil index
theil <- new_y_inttr %>% inner_join(pop) %>%
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
ggsave("SIIT_fig3.png",width=10,height=8,dpi=320)
