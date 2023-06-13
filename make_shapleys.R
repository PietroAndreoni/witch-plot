require(shapley)
require(reldist)
require(dineq)
require(tidyverse)

### useful functions 
zid_gini <- function(factors, data, scen1="",scen2="baseline") {
  data <- data.table::as.data.table(data)
  all_scens <- as.character(unique(data[["file"]]))
  if(length(all_scens)==1) scen1 <- all_scens[[1]]
  if(length(all_scens)>2)  abort("no more than 2 scenarios")
  if(scen1!="" & !scen1 %in% all_scens) abort("please choose a valid name for scenario 1")
  if(scen2!="baseline" & !scen2 %in% all_scens) abort("please choose a valid name for scenario 2")
  
  if (scen2=="baseline") {
    s1 <- data[file==scen1][["ykali"]] 
    weights <- data[file==scen1][["pop"]]
    for (f in factors) s1 <- s1 + data[file==scen1][[f]] 
    reldist::gini(s1*1e6/weights,weights)  } 
  else {
    s1 <- data[file==scen1][["ykali"]] 
    s2 <- data[file==scen2][["ykali"]]  # baseline for counterfactual income
    weights <- data[file==scen1][["pop"]]
    for (f in factors) { 
      s1 <- s1 + data[file==scen1][[f]] 
      s2 <- s2 + data[file==scen2][[f]] }
    reldist::gini(s1*1e6/weights,weights) - reldist::gini(s2*1e6/weights,weights) }
}

zid_theilb <- function(factors, data, scen1="",scen2="baseline") {
  data <- data.table::as.data.table(data)
  all_scens <- as.character(unique(data[["file"]]))
  if(length(all_scens)==1) scen1 <- all_scens[[1]]
  if(length(all_scens)>2)  abort("no more than 2 scenarios")
  if(scen1!="" & !scen1 %in% all_scens) abort("please choose a valid name for scenario 1")
  if(scen2!="baseline" & !scen2 %in% all_scens) abort("please choose a valid name for scenario 2")
  weights <- data[file==scen1][["pop"]]
  n <- data[file==scen1][["n"]]
  
  if (scen2=="baseline") {
    s1 <- data[file==scen1][["ykali"]] 
    for (f in factors) s1  <- s1 + data[file==scen1][[f]] 
    dineq::mld_decomp(s1*1e6/weights, n, weights)$mld_decomp$mld_between  } 
  else {
    s1 <- data[file==scen1][["ykali"]] 
    s2 <- data[file==scen2][["ykali"]]  # baseline for counterfactual income
    weights2 <- data[file==scen2][["pop"]]
    for (f in factors) { 
      s1 <- s1 + data[file==scen1][[f]] 
      s2 <- s2 + data[file==scen2][[f]] }
    dineq::mld_decomp(s1*1e6/weights, n, weights)$mld_decomp$mld_between - dineq::mld_decomp(s2*1e6/weights2, n, weights2)$mld_decomp$mld_between }
}

zid_theilw <- function(factors, data, scen1="",scen2="baseline") {
  data <- data.table::as.data.table(data)
  all_scens <- as.character(unique(data[["file"]]))
  if(length(all_scens)==1) scen1 <- all_scens[[1]]
  if(length(all_scens)>2)  abort("no more than 2 scenarios")
  if(scen1!="" & !scen1 %in% all_scens) abort("please choose a valid name for scenario 1")
  if(scen2!="baseline" & !scen2 %in% all_scens) abort("please choose a valid name for scenario 2")
  weights <- data[file==scen1][["pop"]]
  n <- data[file==scen1][["n"]]
  
  if (scen2=="baseline") {
    s1 <- data[file==scen1][["ykali"]] 
    for (f in factors) s1  <- s1 + data[file==scen1][[f]] 
    dineq::mld_decomp(s1*1e6/weights, n, weights)$mld_decomp$mld_within  } 
  else {
    s1 <- data[file==scen1][["ykali"]] 
    s2 <- data[file==scen2][["ykali"]]  # baseline for counterfactual income
    weights2 <- data[file==scen2][["pop"]]
    for (f in factors) { 
      s1 <- s1 + data[file==scen1][[f]] 
      s2 <- s2 + data[file==scen2][[f]] }
    dineq::mld_decomp(s1*1e6/weights, n, weights)$mld_decomp$mld_within - dineq::mld_decomp(s2*1e6/weights2, n, weights2)$mld_decomp$mld_within }
}

shapleyref <- ALL_FLOWS %>% select(-yab,-ynet) %>%
  filter(ttoyear(t)<=2100 & ttoyear(t)>=2015 & file %in% c("ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest")) %>%
  inner_join(ykali_dist) %>% 
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,file) %>%
  do(shapley::owen(zid_gini,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.)) 

shapleyrefall <- ALL_FLOWS %>% select(-yab,-ynet) %>%
  filter(ttoyear(t)<=2100 & ttoyear(t)>=2020 & file %in% c("ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest")) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>%
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,n,file) %>%
  do(shapley::owen(zid_gini,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.))

shapleyOall <- ALL_FLOWS %>% select(-yab,-ynet) %>%
  filter(ttoyear(t)<=2100 & ttoyear(t)>=2020 & file %in% c("ssp2_B700p_DISTgeo_COSTbest_TAXbest_NEGbest",
                                                           "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") ) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>%
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,n) %>%
  do(shapley::owen(zid_gini,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=., scen1= "ssp2_B700p_DISTgeo_COSTbest_TAXbest_NEGbest", scen2= "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest"))

print("Theil between") 

files <- c("ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest",
  "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
  "ssp2_B700_DISTepc_COSThigh_TAXbest_NEGbest",
  "ssp2_B700_DISTgeo_COSThigh_TAXbest_NEGbest")

shapleyreftheil <- ALL_FLOWS %>%
  filter(file %in% files & 
           ttoyear(t) %in% c(2050,2075,2100)) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,file) %>%
  do(shapley::owen(zid_theilb,list(c("abcost","err"),c("cdrcost"), c("cdrrev","gentax","transfer","ctx")), data=.)) %>%
  mutate(dec="between") 

print("Theil within") 

shapleyreftheil <- ALL_FLOWS %>%
  filter(file %in% files & 
           ttoyear(t) %in% c(2050,2075,2100)) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,file) %>%
  do(shapley::owen(zid_theilw,list(c("abcost","err"),c("cdrcost"), c("cdrrev","gentax","transfer","ctx")), data=.)) %>%
  mutate(dec="within") %>%
  rbind(shapleyreftheil) 

###### SI 
files <- c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
           "ssp2_B700_DISTgeo_COSTbest_TAXlow_NEGbest",
           "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGhigh",
           "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGlow",
           "ssp2_B700_DISTgeo_COSTbest_TAXhigh_NEGbest")

print("Theil between, SI") 

shapleyreftheil <- ALL_FLOWS %>%
  filter(file %in% files & 
           ttoyear(t) %in% c(2075,2100)) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,file) %>%
  do(shapley::owen(zid_theilb,list(c("abcost","err"),c("cdrcost"), c("cdrrev","gentax","transfer","ctx")), data=.)) %>%
  mutate(dec="between") 

print("Theil within, SI") 

shapleyreftheil <- ALL_FLOWS %>%
  filter(file %in% files & 
           ttoyear(t) %in% c(2075,2100)) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop %>% mutate(pop=pop/10)) %>% 
  mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
  group_by(t,file) %>%
  do(shapley::owen(zid_theilw,list(c("abcost","err"),c("cdrcost"), c("cdrrev","gentax","transfer","ctx")), data=.)) %>%
  mutate(dec="within") %>%
  rbind(shapleyreftheil) 

