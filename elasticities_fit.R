###############################################################################
require(data.table)
require(tidyverse)
require(plm)
require(nls2)
require(mlr3measures)
require(wid)

##### FIRST: compute elasticities from decile based data. Using year>2000 data  
inequality_s_wid <- fread('data/inequality-wid-wid_data.csv')
inequality_s_wid <- inequality_s_wid[quantile %in% c(paste0("D",seq(1,10)) ),]


taxsh <- inequality_s_wid %>%
  filter(variable %in% c("pretaxinc","dispinc")) %>%
  pivot_wider(names_from="variable",values_from="value") %>%
  drop_na() %>%
  group_by(iso3,year) %>%
  mutate(tax_sh=(pretaxinc-dispinc)/sum(pretaxinc-dispinc) ) %>%
  select(year,iso3,quantile,tax_sh) 

etatax <- inner_join(inequality_s_wid %>% filter(variable=="pretaxinc_sh") %>% rename(pretaxinc_sh=value),taxsh) %>%
  drop_na() %>%
  filter(year>=2000 & tax_sh < 0.8) %>%
  group_by(iso3,year) %>%
  mutate(n=n()) %>%
  filter(n==10) %>% #only complete data
  mutate(x=pretaxinc_sh,y=tax_sh) %>%
  group_by(iso3,year) %>%
  summarise(etatax=coef(nls(y~I(x^power/sum(x^power)),start=list(power=1.05)))[[1]] )
summary(etatax)

ggplot(etatax) +
  geom_line(aes(x=year,y=etatax)) +
  facet_wrap(iso3~.,scales="free")

eta <- inequality_s_wid %>%
  filter(year>=2000 & variable %in% c("wealth_sh","pretaxinc_sh")) %>%
  pivot_wider(names_from="variable",values_from="value") %>%
  drop_na() %>%
  group_by(iso3,year) %>%
  mutate(n=n()) %>%
  filter(n==10) %>%
  ungroup() %>% 
  #  mutate(x=ifelse(pretaxinc_sh <= 1e-4,1e-4,pretaxinc_sh),y=ifelse(wealth_sh <=1e-4,1e-4,wealth_sh)) %>%
  mutate(x=pretaxinc_sh,y=wealth_sh) %>%
  group_by(iso3,year) %>%
  summarise(eta=coef(nls(y ~ I(x^power/sum(x^power)),start=list(power=1)))[[1]] )
summary(eta)

ggplot() +
  geom_violin(data=full_join(eta,etatax),aes(x=year,y=eta,group=year),color="blue",fill=NA,width=0.8) +
  geom_boxplot(data=inner_join(eta,etatax),aes(x=year,y=eta,group=year),color="red",fill=NA,width=0.5) +
  geom_boxplot(data=anti_join(eta,etatax),aes(x=year,y=eta,group=year),color="green",fill=NA,width=0.3)

gini <- inequality_s_wid %>%
  filter(variable %in% c("pretaxinc_sh")) %>%
  drop_na() %>%
  group_by(iso3,year) %>%
  mutate(n=n()) %>%
  filter(n==10) %>%
  group_by(iso3,year) %>% 
  summarise(gini=reldist::gini(value/sum(value)))

mfig2 <- ggplot(inner_join(eta,etatax) %>% filter(etatax>0)) +
  geom_boxplot(aes(x=year,y=eta,group=year),color="blue") +
  geom_boxplot(aes(x=year,y=etatax,group=year),color="red",fill=NA) +
  geom_text(data=.%>%group_by(year) %>% summarise(med=median(eta),n=n()),aes(x=year,y=med*1.02,label=round(med,2)),color="blue") +
  geom_text(data=.%>%group_by(year) %>% summarise(med=median(etatax),n=n()),aes(x=year,y=med*1.02,label=round(med,2)),color="red") +
  geom_text(data=.%>%group_by(year) %>% summarise(med=median(eta-etatax),n=n()),aes(x=year,y=0.5,label=paste0("N=",n))) +
  xlab('') + ylab('Elasticity') + theme_pubr()
ggsave("figm2.png",height=6,width=9,dpi=320)

estimates <- inner_join(eta,
                        inequality_s_wid %>% 
                          filter(variable=="pretaxinc_sh") %>% 
                          drop_na() %>%
                          group_by(iso3,year) %>%
                          mutate(n=n()) %>%
                          filter(n==10) %>%
                          ungroup() ) %>%
  group_by(iso3,year) %>%
  mutate(valuest=value^eta/sum(value^eta)) %>%
  select(iso3,year,quantile,valuest) %>%
  inner_join(inequality_s_wid %>% 
               filter(variable=="wealth_sh"))

ggplot(estimates %>% filter(year==2010 & iso3=="ITA")) +
  geom_point(aes(x=as.numeric(str_remove(quantile,"D")),y=value)) +
  geom_point(aes(x=as.numeric(str_remove(quantile,"D")),y=valuest),color="red") +
  facet_wrap(iso3~.,)

errors <- estimates %>%
  group_by(iso3,year) %>%
  summarise(mse=medse(value,valuest),mae=maxae(value,valuest))

estimates_tax <- inner_join(etatax,
                            inequality_s_wid %>% 
                              filter(variable=="pretaxinc_sh") %>% 
                              drop_na() %>%
                              group_by(iso3,year) %>%
                              mutate(n=n()) %>%
                              filter(n==10) %>%
                              ungroup() ) %>%
  group_by(iso3,year) %>%
  mutate(valuest=value^etatax/sum(value^etatax)) %>%
  select(iso3,year,quantile,valuest) %>%
  inner_join(taxsh)

ggplot(estimates_tax %>% filter(year==2010 & iso3=="ALB")) +
  geom_point(aes(x=as.numeric(str_remove(quantile,"D")),y=tax_sh)) +
  geom_point(aes(x=as.numeric(str_remove(quantile,"D")),y=valuest),color="red") +
  facet_wrap(iso3~.,)

errors_tax <- estimates_tax %>%
  group_by(iso3,year) %>%
  summarise(mse=medse(value,valuest),mae=maxae(value,valuest))

#### regressions on exogenous gini index
ols2 <- lm(eta~I(gini)+I(gini^2),data=inner_join(gini,eta))
fixed2 <- plm(eta~I(gini)+I(gini^2),data=inner_join(gini,eta) %>% drop_na() %>% ungroup(),index=c("iso3","year"),model="within",effects="twoways")
pFtest(fixed2, ols2) 

summary(ols2)

ggplot(inner_join(gini,eta),aes(x=gini,y=eta)) +
  geom_point(aes(color=iso3,alpha=year)) + 
  geom_smooth(method="lm",formula=y~x+I(x^2) ) +
  theme(legend.position="none") 

olstax <-lm(eta-etatax~gini,data=inner_join(gini,etatax) %>% inner_join(eta))
fixedtax <- plm(eta-etatax~eta,data=inner_join(gini,etatax) %>% inner_join(eta),index=c("iso3","year"),model="within",effect = "twoways")
summary(olstax)

#fixed effects on etatax
olstax <-lm(etatax~gini,data=full_join(gini,etatax) %>% drop_na() %>% ungroup())
fixedtax <- plm(etatax~gini,data=full_join(gini,etatax) %>% drop_na() %>% ungroup(),index=c("iso3","year"),model="within",effect = "twoways")
pFtest(fixedtax, olstax) 
summary(olstax)

ggplot(inner_join(gini,etatax),aes(x=gini,y=etatax)) +
  geom_point(aes(color=iso3,alpha=year)) + 
  geom_smooth(method="lm") + theme(legend.position="none")

##### carbon elasticities 

#carbon inequality variables
carbon_footprint <- download_wid(
  indicators = c("lpfghg"),
  areas = "all", 
  years = "all",
  perc = perc<-c("p0p10","p10p20","p20p30","p30p40","p40p50","p50p60","p60p70","p70p80","p80p90","p90p100"),
  ages = 999,
  pop = "i",
  include_extrapolations = FALSE )

# rename the quantiles
carbon_footprint$percentile <-revalue(carbon_footprint$percentile, c("p0p10"="D1", "p10p20"="D2", "p20p30"="D3", "p30p40"="D4", "p40p50"="D5",
                                        "p50p60"="D6", "p60p70"="D7", "p70p80"="D8", "p80p90"="D9", "p90p100"="D10"))

carbon_footprint <- carbon_footprint %>% 
  rename(quantile=percentile,cf=value,iso3=country) %>% 
  select(-variable) %>%
  group_by(iso3,year) %>%
  mutate(cf_sh=cf/sum(cf))
  
carbon_footprint$iso3<-countrycode(carbon_footprint$iso3, origin ="iso2c", destination="iso3c")

etac <- carbon_footprint %>% 
  inner_join(inequality_s_wid %>%
               filter(year>=2000 & variable %in% c("pretaxinc_sh")) %>% select(-variable) %>% rename(pretaxinc_sh=value)) %>%
  drop_na() %>%
  group_by(iso3,year) %>%
  mutate(n=n()) %>%
  filter(n==10) %>%
  ungroup() %>% 
  mutate(x=pretaxinc_sh,y=cf_sh) %>%
  group_by(iso3,year) %>%
  summarise(eta=coef(nls(y ~ I(x^power/sum(x^power)),start=list(power=1)))[[1]] )
summary(etac)

olstax <-lm(eta~value,data=inner_join(etac,inequality_s_wid %>%
                                          filter(variable %in% c("pretaxinc")) %>% group_by(iso3,year) %>% summarise(value=sum(value))) %>% 
              drop_na() %>% ungroup())

ggplot(inner_join(etac,inequality_s_wid %>%
                    filter(variable %in% c("pretaxinc")) %>% group_by(iso3,year) %>% summarise(value=sum(value))) %>% 
         drop_na() %>% ungroup() %>% filter(value<5 & eta < 1.1)) +
  geom_point(aes(x=value,y=eta))
