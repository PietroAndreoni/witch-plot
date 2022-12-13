shapleyref <- data.frame(t=numeric(),sigma=numeric(),name=character(),ssp=character())
shapleyrefall <- data.frame(t=numeric(),n=character(),sigma=numeric(),name=character(),ssp=character())
shapleyreftheil <- data.frame(t=numeric(),n=character(),name=character(),ssp=character())

### extra dataframes
for (i in c(2)) {

print(paste0("Global calc ssp",as.character(i))) 

  shapleyref <- ALL_FLOWS %>% select(-yab,-ynet) %>%
    filter(ssp==i & ttoyear(t)>=2020  & ttoyear(t)<=2100 & ( (O=="yes" & B=="650") | (O=="no" & B=="ref") )  ) %>%
    inner_join(ykali_dist) %>% 
    mutate(c1=-abcost,c2=cdrrev-cdrcost,c3=transfer-ctx-gentax,err=ygrossd-ykali) %>%
    select_at((c("t","n","dist","B","c1","c2","c3","err","ykali"))) %>%
    pivot_longer(c(c1,c2,c3,err)) %>%
    mutate(state=ifelse(B=="650","s1","s2")) %>% select(-B) %>%
    pivot_wider(names_from="state",values_from="value") %>%
    group_by(t) %>%
    do(make_shapley(.,global=TRUE)) %>%
    mutate(ssp=paste0("ssp",as.character(i))) %>%
    rbind(shapleyref)

print(paste0("Done with ssp",as.character(i))) 
} 

zid <- function(factors, data) {
  get_witch_simple("pop")
  cntf <- data[["ykali"]]  # baseline for counterfactual income
  for (f in factors)
    cntf <- cntf + data[[f]]
  weights <- data[["pop"]]
  reldist::gini(cntf*1e6/weights,weights)
}

require(shapley)
shapleytry <- ALL_FLOWS %>% select(-yab,-ynet) %>%
  filter(ssp==2 & ttoyear(t)==2050 & O=="yes" & B=="650" ) %>%
  inner_join(ykali_dist) %>% 
  inner_join(pop %>% select(t,n,ssp,value) %>% unique() %>% rename(pop=value) %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t) %>%
#  do(owen(zid,list(c("abcost","err"),c("cdrrev","cdrcost"), c("transfer","ctx","gentax")), data=. )) 
  do(shapley(zid,c("abcost","err","cdrrev","cdrcost","transfer","ctx","gentax"), data=. )) 

#very computationally expensive, only for ssp2 
shapleyreftheil <- ALL_FLOWS %>% select(-yab,-ynet) %>%
  filter(ssp==2 & ttoyear(t)>=2020  & ttoyear(t)<=2100 & ( (O=="yes" & B=="650") | (O=="no" & B=="ref") )  ) %>%
  inner_join(ykali_dist) %>% inner_join(Y_DIST) %>%
  mutate(c1=-abcost,c2=cdrrev-gentax-cdrcost,c3=transfer-ctx,err=ygross-ykali+value-y) %>%
  select_at((c("t","n","dist","B","c1","c2","c3","err","ykali"))) %>%
  pivot_longer(c(c1,c2,c3,err)) %>%
  rename(state=B) %>% mutate(state=ifelse(state=="650","s1","s2")) %>%
  pivot_wider(names_from="state",values_from="value") %>%
  group_by(t) %>%
  do(make_shapley_theil(.,global=TRUE)) %>%
  mutate(ssp=paste0("ssp",as.character(i))) %>%
  rbind(shapleyreftheil)  
 
shapleyrefall <- ALL_FLOWS %>% select(-yab,-ynet) %>%
  filter(ssp==2 & ttoyear(t) %in% c(2050,2075,2100) & ( (O=="yes" & B=="650") | (O=="no" & B=="ref")  ) ) %>%
  inner_join(ykali_dist) %>% inner_join(Y_DIST) %>%
  mutate(c1=-abcost,c2=cdrrev-gentax-cdrcost,c3=transfer-ctx,err=ygross-ykali+value-y) %>%
  select_at((c("t","n","dist","B","c1","c2","c3","err","ykali"))) %>%
  pivot_longer(c(c1,c2,c3,err)) %>%
  rename(state=B) %>% mutate(state=ifelse(state=="650","s1","s2")) %>%
  pivot_wider(names_from="state",values_from="value") %>%
  group_by(t,n) %>%
  do(make_shapley(.,global=FALSE)) %>%
  mutate(ssp=paste0("ssp",as.character(i))) %>% 
  rbind(shapleyrefall)
