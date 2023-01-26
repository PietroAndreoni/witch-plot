rm(list = ls())
witch_folder = "../RICE50x" #Where you're RICE/DICE/RICE50x code is located
#main directory of your results files
main_directory <- witch_folder # by default, the witch source folder
subdir = c("") #can be multiple directories

region_id = "ed57" #for historical data folder
year0 = 2015
tstep = 5

restrict_files = c("results_oldrun")
exclude_files = c("")
removepattern = c("")

yearmin = 1980
yearmax = 2300

#Initialize default options, load all witch and other functionsget
source('R/witch_functions.R')

#mapping of variables to historical and validation statistics and unit conversion to WITCH units
map_var_hist <- fread("varname_model, set_model, element_model, var_witch, set_witch, element_witch, conv
Y, , , SOCECON, *, gdp-ppp, 1
E, , , Q_EMI, e, co2, 0.2727273
pop, , , l, , , 1e-3
K, , , K, g, fg, 1
I, , , I, g, fg, 1
")

#gdxcompaR(Standard gdxcompaR based on typical variables, otherwise edit in gdxcompaR/server.R)
#runApp(appDir = "gdxcompaR/rice")

get_witch_simple("ABATECOST")
get_witch_simple("COST_DAC")
get_witch_simple("DACREV")
get_witch_simple("CTX")
get_witch_simple("TRANSFER")
get_witch_simple("GENTAX")
get_witch_simple("CPRICE")
get_witch_simple("CPRICE_NEG")
get_witch_simple("ineq_weights")
get_witch_simple("YGROSS")

YGROSS <- YGROSS %>% rename(ygross=value)

ABATECOST <- ABATECOST %>% 
  rename(abcost=value) %>%
  full_join(YGROSS %>% filter(t %in% unique(ABATECOST$t)) ) %>%
  mutate(abcostgdprel=abcost/ygross) %>% select(-ygross) 

COST_DAC <- COST_DAC %>% 
  rename(cdrcost=value) %>%
  full_join(YGROSS %>% filter(t %in% unique(COST_DAC$t)) ) %>%
  mutate(cdrcostgdprel=cdrcost/ygross) %>% select(-ygross) 

CTX <- CTX %>% 
  rename(ctx=value) %>%
  full_join(YGROSS %>% filter(t %in% unique(CTX$t)) ) %>%
  mutate(ctxgdprel=ctx/ygross) %>% select(-ygross)

DACREV <- DACREV %>% 
  rename(cdrrev=value) %>%
  full_join(YGROSS %>% filter(t %in% unique(DACREV$t)) ) %>%
  mutate(cdrrevgdprel=cdrrev/ygross) %>% select(-ygross)

TRANSFER <- TRANSFER %>% rename(transfer=value)

GENTAX <- GENTAX %>% rename(gentax=value)

CPRICE <- CPRICE
CPRICE_NEG <- CPRICE_NEG

PROF_CDR <- full_join(DACREV,COST_DAC) %>%
  mutate(prof=cdrrev-cdrcost)

get_witch_simple("ykali")
get_witch_simple("quantiles_ref")
get_witch_simple("YGROSS_DIST")

YGROSS_DIST <- YGROSS_DIST %>% rename(ygrossd=value)

ykali_dist <- right_join(ykali %>% rename(ykali=value),quantiles_ref) %>% mutate(ykali=ykali*value) %>% select(-value)

ALL_FLOWS <- right_join(ABATECOST,ineq_weights %>% filter(ineq_elast=="abatement") %>% select(-ineq_elast) ) %>%
  mutate(abcost=abcost*value) %>% select(-value,-abcostgdprel) %>%
  full_join( right_join(COST_DAC,ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% select(-ineq_elast) ) %>%
               mutate(cdrcost=cdrcost*value) %>% select(-value,-cdrcostgdprel)) %>%
  full_join( right_join(CTX,ineq_weights %>% filter(ineq_elast=="abatement") %>% select(-ineq_elast) ) %>%
               mutate(ctx=ctx*value) %>% select(-value,-ctxgdprel)) %>%
  full_join( right_join(DACREV,ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% select(-ineq_elast) ) %>%
               mutate(cdrrev=cdrrev*value) %>% select(-value,-cdrrevgdprel)) %>%
  full_join(TRANSFER) %>%
  full_join(GENTAX) %>%
  full_join(YGROSS_DIST) %>%
  mutate(yab=ygrossd-abcost,ynet=yab+cdrrev-cdrcost,y=ynet+transfer-ctx-gentax) 

FLOWS <- ALL_FLOWS %>% 
  group_by_at(c("t","pathdir","n",file_group_columns)) %>%
  summarise(transfer=sum(transfer),gentax=sum(-gentax),cdrcost=sum(-cdrcost),ctx=sum(-ctx),abcost=sum(-abcost),cdrrev=sum(cdrrev))

FLOWS <-  rbind(FLOWS, FLOWS %>%
                  group_by_at(c("t","pathdir",file_group_columns)) %>%
                  summarise(transfer=sum(transfer),gentax=sum(gentax),cdrcost=sum(cdrcost),ctx=sum(ctx),abcost=sum(abcost),cdrrev=sum(cdrrev)) %>%
                  mutate(n="World") ) %>%
  full_join(YGROSS %>% filter(t %in% unique(FLOWS$t))) 


zid_gini <- function(factors, data) {
  s1 <- data[["ykali"]]  # baseline for counterfactual income
  weights <- data[["pop"]]
  for (f in factors) s1 <- s1 + data[[f]]
  reldist::gini(s1*1e6/weights,weights) 
}

zid_theilb <- function(factors, data) {
  s1 <- data[["ykali"]]  # baseline for counterfactual income
  weights <- data[["pop"]]
  n <- as.factor(data[["n"]])
  for (f in factors) s1 <- s1 + data[[f]]
  dineq::mld_decomp(s1*1e6/weights, n, weights)$mld_decomp$mld_between 
}

zid_theilw<- function(factors, data) {
  s1 <- data[["ykali"]]  # baseline for counterfactual income
  weights <- data[["pop"]]
  n <- as.factor(data[["n"]])
  for (f in factors) s1 <- s1 + data[[f]]
  dineq::mld_decomp(s1*1e6/weights, n, weights)$mld_decomp$mld_within
}


require(shapley)
require(reldist)
require(dineq)
get_witch_simple("pop")
pop <- pop %>% rename(pop=value) 
### extra dataframes

shapleyref <- ALL_FLOWS %>% select(-yab,-ynet) %>%
    filter(ttoyear(t)<=2100 & ttoyear(t)>=2020 ) %>%
    inner_join(ykali_dist) %>% 
    inner_join(pop %>% mutate(pop=pop/10)) %>% 
    mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
    group_by(t) %>%
    do(owen(zid_gini,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.)) 

print("Theil between") 

  shapleyreftheil <- ALL_FLOWS %>%
    filter(ttoyear(t)<=2100 & ttoyear(t)>=2020) %>%
    inner_join(ykali_dist) %>%
    inner_join(pop %>% mutate(pop=pop/10)) %>% 
    mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
    group_by(t) %>%
    do(owen(zid_theilb,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.)) %>%
    mutate(dec="between") 
  
print("Theil within") 
  
  shapleyreftheil <- ALL_FLOWS %>%
    filter(ttoyear(t)<=2100 & ttoyear(t)>=2020) %>%
    inner_join(ykali_dist) %>%
    inner_join(pop %>% mutate(pop=pop/10)) %>% 
    mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
    group_by(t) %>%
    do(owen(zid_theilw,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.)) %>%
    mutate(dec="within") %>%
    rbind(shapleyreftheil) 

# shapleyrefall <- ALL_FLOWS %>% select(-yab,-ynet) %>%
#   filter(ttoyear(t)<=2100 & ttoyear(t)>=2020) %>%
#   inner_join(ykali_dist) %>%
#   inner_join(pop %>% mutate(pop=pop/10)) %>% 
#   mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
#   group_by(t,n) %>%
#   do(owen(zid_gini,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.))


ggplot(shapleyref %>% group_by(t,group) %>% summarise(value=sum(value))) +
  geom_area(aes(x=ttoyear(t),y=value*100,fill=as.factor(group)))  +
  geom_line(data=.%>% group_by(t) %>% summarise(value=sum(value)),aes(x=ttoyear(t),y=value*100))

ggplot(shapleyreftheil %>% group_by(t,group,dec) %>% summarise(value=sum(value))) +
  geom_area(aes(x=ttoyear(t),y=value*100,fill=as.factor(group),alpha=dec)) +
  geom_line(data=.%>% group_by(t) %>% summarise(value=sum(value)),aes(x=ttoyear(t),y=value*100))

# ggplot(shapleyrefall) +
#   geom_area(aes(x=ttoyear(t),y=value*100,fill=factor)) +
#   facet_wrap(n~.,scales="free")
