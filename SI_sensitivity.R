# sd_dist -> standard deviation to generate an income distribution (lognormal)
# etacdr -> how skewed is ownership of CDR relative to the income distribution (1->distribution neutral,>1 more concentrated to the top)
# etatax -> how regressive or progressive are income taxes used to pay for CDR revenues
# prof_mar -> profit margin of CDR, i.e. cprice/avg_cost 
# rev_gdp -> revenues of CDR over GDP
red_model <- function(dist0=rep(0.1,10), etacdr=2, etatax=1, prof_mar=2, rev_gdp= 0.05){
  out0 <- reldist::gini(dist0)
  dist <- dist0 + rev_gdp * ((1-1/prof_mar)*dist0^etacdr/sum(dist0^etacdr) - dist0^etatax/sum(dist0^etatax)) 
  dist <- dist/sum(dist)
  out <-  reldist::gini(dist) 
  return( out-out0 )
}

files <- c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
           "ssp2_B700_DISTgeo_COSTbest_TAXlow_NEGbest",
           "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGhigh",
           "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGlow",
           "ssp2_B700_DISTgeo_COSTbest_TAXhigh_NEGbest")

names <-  list("Central specification"="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
            "Regressive taxes"="ssp2_B700_DISTgeo_COSTbest_TAXlow_NEGbest",
            "Concentrated capital"="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGhigh",
            "Shared capital"="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGlow",
            "Progressive taxes"="ssp2_B700_DISTgeo_COSTbest_TAXhigh_NEGbest")


gini_cdr <- ALL_FLOWS %>% mutate(value=ygrossd-gentax-cdrcost-ctx+cdrrev+transfer) %>%
  group_by_at(c("t","n",file_group_columns)) %>%
  summarise(ginirel=reldist::gini(value,weights=rep(0.1,10))-reldist::gini(ygrossd,weights=rep(0.1,10))) 

get_witch_simple("el_coeff")
validate_model <- COST_CDR %>% 
  filter(file %in% files) %>%
  inner_join(REV_CDR) %>% 
  inner_join(YGROSS) %>%
  inner_join(quantiles_ref %>% group_by(t,n,file) %>% summarise(q=list(value))) %>%
  mutate(prof=cdrrev/cdrcost,revgdp=cdrrev/ygross) %>%
  inner_join(el_coeff %>% filter(ineq_elast=="tax") %>% rename(e_tax=value) %>% select(-ineq_elast)) %>%
  inner_join(el_coeff %>% filter(ineq_elast=="carbon_rev") %>% rename(e_cdr=value) %>% select(-ineq_elast)) %>%
  inner_join(el_coeff %>% filter(ineq_elast=="abatement") %>% rename(e_ctax=value) %>% select(-ineq_elast)) %>%
  inner_join(EIND) %>%
  inner_join(E_NEG) %>%
  inner_join(gini_cdr) %>%
  mutate(weight= if_else(eind>use,1,eind/use)) %>%
  mutate(e_eff = e_tax * (1 - weight) + e_ctax * weight ) %>%
  filter(file %in% files) %>%
  rowwise() %>%   
  mutate(pred=red_model(dist0=q, etacdr=e_cdr, etatax=e_eff, prof_mar=prof, rev_gdp=revgdp)) %>%
  mutate(file=factor(file))

levels(validate_model$file) <- names

main <- ggplot(validate_model %>% filter(ginirel*100>=-0.1 & ginirel*100 <=1)) +
  geom_point(aes(x=ginirel*100,y=pred*100,color=file)) +
  geom_smooth(aes(x=ginirel*100,y=pred*100),method="lm") +
  geom_abline(slope=1,intercept=0) +
  theme_pubr() + xlab("Scenario results") + ylab("Simplified model results") +
  guides(color=guide_legend(nrow=2,title = NULL))

small <- ggplot(validate_model) +
  geom_point(aes(x=ginirel*100,y=pred*100,color=file)) +
  geom_smooth(aes(x=ginirel*100,y=pred*100),method="lm") +
  geom_abline(slope=1,intercept=0) + 
  theme_pubr() + xlab("") + ylab("") + theme(legend.position = "none")

SI_fig1 <- ggdraw() +
  draw_plot(main) +
  draw_plot(small, x = 0.15, y = .5, width = .3, height = .3)
ggsave("SIC_fig1.png",width=7,height=5.5,dpi=320)

##### global effects 

a <- shapleyreftheil %>% 
  group_by(t,file,group,dec) %>% summarise(value=sum(value)) %>%
  filter(file %in% files & 
           ttoyear(t) %in% c(2075,2100)) %>% inner_join(scenarios)  %>% mutate(group=as.factor(group),file=as.factor(file))
levels(a$group) <- list("Abatement costs"="1","CDR costs"="2","CDR transfers"="3")
levels(a$file) <- names

ggplot(a) +
  geom_bar(data=. %>% filter(value>0) %>% group_by(t,file,DIST,COST) %>% summarise(value=sum(value)),aes(x=file,y=value*100),fill=NA,stat="identity",color="grey",linetype=2) +
  geom_bar(data=.%>% filter(group %in% c("CDR costs","CDR transfers")),aes(x=file,y=value*100,fill=group,color=group,alpha=dec),stat="identity",color="black") +
  geom_point(data=.%>% filter(group %in% c("CDR costs","CDR transfers")) %>% group_by(t,file,DIST,COST) %>% summarise(value=sum(value)),
             aes(x=file,y=value*100),shape=3,size=2) +
  geom_point(data=. %>% group_by(t,file,DIST,COST) %>% summarise(value=sum(value)),
             aes(x=file,y=value*100),shape=1,size=2) +
  facet_grid(ttoyear(t)~.,) + xlab('') + ylab('') +
  guides(fill=guide_legend(title="Inequality driver"),alpha=guide_legend(title="Inequality contribution")) +
  scale_fill_manual(values=c("#00BA38","#619CFF")) + theme_pubr()
ggsave("SIA_fig1.png",width=9,height=8,dpi=320)



