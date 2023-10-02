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
ggsave("M_fig1.pdf",width=18,height=12,dpi=300,units="cm")
