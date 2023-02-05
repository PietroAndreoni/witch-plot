
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
                              etatax=seq(0.6,1.8,by=0.1),
                              prof_mar=seq(1,10,by=0.5),
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
  geom_contour_filled(aes(x=deta,y=prof_mar,z=dgini)) +
  geom_contour(aes(x=deta,y=prof_mar,z=dgini),color="red",breaks=c(-1000,0,+1000),size=1.3) +
  geom_vline(data=.%>%
               mutate(etacdr=etatax+deta) %>%
               filter(round(etacdr,1) %in% c(1,1.4,1.8,2.2)) %>%
               mutate(`Equity concentration`=case_when(round(etacdr,1)==1 ~ "Neutral",
                                                       round(etacdr,1)==1.4 ~ "Low",
                                                       round(etacdr,1)==1.8 ~ "Best fit",
                                                       round(etacdr,1)==2.2 ~ "High") ),
             aes(xintercept=deta,linetype=paste0(`Equity concentration`,": ",etacdr)),size=1.3,color="blue") +
  facet_wrap(.~paste0(Taxation,": ",etatax)) + theme_pubr() +
  ylab('Profit margin for CDR [cprice/avg cost]') +
  xlab(paste0('Intrinstic regressivity of financing CDR [etacdr-etatax]')) +
  labs(fill = 'Gini - gini0',linetype="Equity concentration (relative to income distribution)")
ggsave("mfig1.png",width=15,height=10,dpi=320)

rand_sd <- runif(20, min=0, max=2)
gini0 <- tibble(sd_dist=c(),gini0=c())
for (sd in rand_sd) {
  gini0 <- rbind(gini0,
                 tibble(sd_dist=sd,gini0=red_model(sd_dist=sd)["out0"]))
}
sens_toy <- tibble(expand.grid(sd_dist=rand_sd,
                               deta=runif(20, min=-1.5, max=1.5),
                               etatax=runif(20, min=0, max=2),
                               prof_mar=runif(20, min=0, max=10),
                               rev_gdp=runif(20, min=0, max=0.2))) %>%
  rowwise() %>%
  mutate(gini=red_model(sd_dist=sd_dist, etacdr=etatax+deta, etatax=etatax, prof_mar=prof_mar, rev_gdp=rev_gdp)["out"]) %>% 
  inner_join(gini0) %>% mutate(dgini=gini-gini0)

ggplot(sens_toy %>%
         mutate(etacdr=deta+etatax) %>% 
         pivot_longer(c(sd_dist,etatax,etacdr,deta,prof_mar,rev_gdp)) %>% 
         filter(name=="prof_mar")) +
  geom_point(aes(x=value,y=dgini)) +
  #  geom_line(data=. %>% group_by(name,value) %>% summarise(med=median(value)),aes(x=value,y=med),color="red") +
  facet_wrap(name~.,scales="free")
ggsave("global_sens.png")  
