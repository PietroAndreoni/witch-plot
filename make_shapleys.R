shapleyref <- data.frame()
shapleyrefall <- data.frame()
shapleyreftheil <- data.frame()

zid_gini <- function(factors, data, scen="1.5C peak",ref="Baseline") {
  s1 <- data[data$Scenario==scen,][["ykali"]]  # baseline for counterfactual income
  s0 <- data[data$Scenario==ref,][["ykali"]]  # baseline for counterfactual income
  weights <- data[data$Scenario==ref,][["pop"]]
  for (f in factors) {
    s1 <- s1 + data[data$Scenario==scen,][[f]]
    s0 <- s0 + data[data$Scenario==ref,][[f]] }
  reldist::gini(s1*1e6/weights,weights) -  reldist::gini(s0*1e6/weights,weights)
}

zid_theilb <- function(factors, data, scen="1.5C peak",ref="Baseline") {
  s1 <- data[data$Scenario==scen,][["ykali"]]  # baseline for counterfactual income
  s0 <- data[data$Scenario==ref,][["ykali"]]  # baseline for counterfactual income
  weights <- data[data$Scenario==ref,][["pop"]]
  n <- as.factor(data[data$Scenario==ref,][["n"]])
  for (f in factors) {
    s1 <- s1 + data[data$Scenario==scen,][[f]]
    s0 <- s0 + data[data$Scenario==ref,][[f]] }
  dineq::mld_decomp(s1*1e6/weights, n, weights)$mld_decomp$mld_between - dineq::mld_decomp(s0*1e6/weights, n, weights)$mld_decomp$mld_between 
}

zid_theilw <-function(factors, data, scen="1.5C peak",ref="Baseline") {
  s1 <- data[data$Scenario==scen,][["ykali"]]  # baseline for counterfactual income
  s0 <- data[data$Scenario==ref,][["ykali"]]  # baseline for counterfactual income
  weights <- data[data$Scenario==ref,][["pop"]]
  n <- as.factor(data[data$Scenario==ref,][["n"]])
  for (f in factors) {
    s1 <- s1 + data[data$Scenario==scen,][[f]]
    s0 <- s0 + data[data$Scenario==ref,][[f]] }
  dineq::mld_decomp(s1*1e6/weights, n, weights)$mld_decomp$mld_within - dineq::mld_decomp(s0*1e6/weights, n, weights)$mld_decomp$mld_within 
}

require(shapley)
require(reldist)
require(dineq)
get_witch_simple("pop")
pop <- pop %>% make_scen() %>% rename(pop=value) 
scen <- "1.5C full"
ref <- "Baseline"
sel_scens <- c(scen,ref)
### extra dataframes
for (i in c(1,2,3,4,5)) {

print(paste0("Global calc ssp",as.character(i))) 

shapleyref <- ALL_FLOWS %>% select(-yab,-ynet) %>%
  filter(ssp==i & ttoyear(t)<=2100 & ttoyear(t)>=2020 & Scenario %in% sel_scens ) %>%
  inner_join(ykali_dist) %>% 
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t) %>%
  do(owen(zid_gini,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.,scen=scen,ref=ref)) %>%
  mutate(ssp=i) %>%
  rbind(shapleyref) 

print(paste0("Done with ssp",as.character(i))) 

} 

print("Regional ssp 2") 

shapleyrefall <- ALL_FLOWS %>% select(-yab,-ynet) %>%
  filter(ssp==2 & ttoyear(t)<=2100 & ttoyear(t)>=2020 & Scenario %in% sel_scens ) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,n) %>%
  do(owen(zid_gini,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.,scen=scen,ref=ref)) %>%
  mutate(ssp=2) %>%
  rbind(shapleyrefall) 

print("Theil between, ssp") 

shapleyreftheil <- ALL_FLOWS %>%
  filter(ssp==2 & ttoyear(t)<=2100 & ttoyear(t)>=2020  & Scenario %in% sel_scens ) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t) %>%
  do(owen(zid_theilb,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.,scen=scen,ref=ref )) %>%
  mutate(ssp=2,dec="between") %>%
  rbind(shapleyreftheil) 

print("Theil within, ssp") 

shapleyreftheil <- ALL_FLOWS %>%
  filter(ssp==2 & ttoyear(t)<=2100 & ttoyear(t)>=2020 & Scenario %in% sel_scens ) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t) %>%
  do(owen(zid_theilw,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.,scen=scen,ref=ref )) %>%
  mutate(ssp=2,dec="within") %>%
  rbind(shapleyreftheil) 
