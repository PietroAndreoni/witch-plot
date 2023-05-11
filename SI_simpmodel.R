
## METHODS FIGURE 1: 

# sd_dist -> standard deviation to generate an income distribution (lognormal)
# etacdr -> how skewed is ownership of CDR relative to the income distribution (1->distribution neutral,>1 more concentrated to the top)
# etatax -> how regressive or progressive are income taxes used to pay for CDR revenues
# prof_mar -> profit margin of CDR, i.e. cprice/avg_cost 
# rev_gdp -> revenues of CDR over GDP
red_model <- function(sd_dist=1, etacdr=2, etatax=1, prof_mar=2, rev_gdp= 0.05){
  dist0 <- qlnorm(p=seq(0.05,0.95,by=0.1),mean=1,sdlog= sd_dist)
  dist0 <- dist0/sum(dist0)
  out0 <- reldist::gini(dist0)
  dist <- dist0 + rev_gdp * ((1-1/prof_mar)*dist0^etacdr/sum(dist0^etacdr) - dist0^etatax/sum(dist0^etatax)) 
  dist <- dist/sum(dist)
  out <-  reldist::gini(dist) 
  return(c("out0"=out0*100,"out"=out*100))
}

gini0 <- tibble(sd_dist=c(),gini0=c())
for (sd in seq(0.5,1.5,by=0.5)) {
  gini0 <- rbind(gini0,
                 tibble(sd_dist=sd,gini0=red_model(sd_dist=sd)["out0"]))
}
map_toy <- tibble(expand.grid(sd_dist=seq(0.5,1.5,by=0.5),
                              deta=seq(-1,2,by=0.1),
                              etatax=seq(0.6,1.8,by=0.4),
                              prof_mar=seq(1,20,by=0.5),
                              rev_gdp=seq(0.05,0.2,by=0.05))) %>%
  rowwise() %>%
  mutate(gini=red_model(sd_dist=sd_dist, etacdr=etatax+deta, etatax=etatax, prof_mar=prof_mar, rev_gdp=rev_gdp)["out"]) %>% 
  inner_join(gini0) %>% mutate(dgini=gini-gini0)

require(ggrepel)

ggplot(map_toy %>% ungroup() %>% filter(sd_dist==1 & rev_gdp==0.05) %>%
         mutate(Taxation=as.factor(case_when(round(etatax,1)==0.6 ~ "Regressive tax",
                                             round(etatax,1)==1 ~ "Neutral tax",
                                             round(etatax,1)==1.4 ~ "Best fit",
                                             round(etatax,1)==1.8 ~ "Progressive tax")))) +
  geom_contour_filled(aes(x=deta,y=1-1/prof_mar,z=dgini)) +
  geom_contour(aes(x=deta,y=1-1/prof_mar,z=dgini),color="red",breaks=c(-1000,0,+1000),size=1.3) +
  geom_vline(data=.%>%
               mutate(etacdr=etatax+deta) %>%
               filter(round(etacdr,1) %in% c(1,1.4,1.8,2.2)) %>%
               mutate(`Equity concentration`=case_when(round(etacdr,1)==1 ~ "Neutral",
                                                       round(etacdr,1)==1.4 ~ "Low",
                                                       round(etacdr,1)==1.8 ~ "Best fit",
                                                       round(etacdr,1)==2.2 ~ "High") ),
             aes(xintercept=deta,linetype=paste0(`Equity concentration`,": ",etacdr)),size=1.3,color="blue") +
  facet_wrap(.~paste0(Taxation,": ",etatax)) + theme_pubr() + scale_y_continuous(labels = scales::percent) +
  ylab('Profit margin for CDR') +
  xlab(paste0('Intrinstic regressivity of financing CDR [etacdr-etatax]')) +
  labs(fill = 'Gini - gini0',linetype="Equity concentration (relative to income distribution)") + theme(legend.box="vertical")
ggsave("SIM_fig1.png",width=11.7,height=8,dpi=320)


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
ggsave("SIM_fig2.png",width=7,height=5.5,dpi=320)
