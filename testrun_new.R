rm(list = ls())
witch_folder = "../RICE50x" #Where you're RICE/DICE/RICE50x code is located
#main directory of your results files
main_directory <- witch_folder # by default, the witch source folder
subdir = c("") #can be multiple directories

region_id = "ed57" #for historical data folder
year0 = 2015
tstep = 5

restrict_files = c("results_ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest")
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
get_witch_simple("COST_CDR")
get_witch_simple("REV_CDR")
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

COST_CDR <- COST_CDR %>% 
  rename(cdrcost=value) %>%
  full_join(YGROSS %>% filter(t %in% unique(COST_CDR$t)) ) %>%
  mutate(cdrcostgdprel=cdrcost/ygross) %>% select(-ygross) 

CTX <- CTX %>% 
  rename(ctx=value) %>%
  full_join(YGROSS %>% filter(t %in% unique(CTX$t)) ) %>%
  mutate(ctxgdprel=ctx/ygross) %>% select(-ygross)

REV_CDR <- REV_CDR %>% 
  rename(cdrrev=value) %>%
  full_join(YGROSS %>% filter(t %in% unique(REV_CDR$t)) ) %>%
  mutate(cdrrevgdprel=cdrrev/ygross) %>% select(-ygross)

TRANSFER <- TRANSFER %>% rename(transfer=value)

GENTAX <- GENTAX %>% rename(gentax=value)

CPRICE <- CPRICE
CPRICE_NEG <- CPRICE_NEG

PROF_CDR <- full_join(REV_CDR,COST_CDR) %>%
  mutate(prof=cdrrev-cdrcost)

get_witch_simple("ykali")
get_witch_simple("quantiles_ref")
get_witch_simple("YGROSS_DIST")

YGROSS_DIST <- YGROSS_DIST %>% rename(ygrossd=value)

ykali_dist <- right_join(ykali %>% rename(ykali=value),quantiles_ref) %>% mutate(ykali=ykali*value) %>% select(-value)

ALL_FLOWS <- right_join(ABATECOST,ineq_weights %>% filter(ineq_elast=="abatement") %>% select(-ineq_elast) ) %>%
  mutate(abcost=abcost*value) %>% select(-value,-abcostgdprel) %>%
  full_join( right_join(COST_CDR,ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% select(-ineq_elast) ) %>%
               mutate(cdrcost=cdrcost*value) %>% select(-value,-cdrcostgdprel)) %>%
  full_join( right_join(CTX,ineq_weights %>% filter(ineq_elast=="abatement") %>% select(-ineq_elast) ) %>%
               mutate(ctx=ctx*value) %>% select(-value,-ctxgdprel)) %>%
  full_join( right_join(REV_CDR,ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% select(-ineq_elast) ) %>%
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
    filter(ttoyear(t)<=2100 & ttoyear(t)>=2015 ) %>%
    inner_join(ykali_dist) %>% 
    inner_join(pop %>% mutate(pop=pop/10)) %>% 
    mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
    group_by(t) %>%
    do(owen(zid_gini,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.)) 
  
  print("Theil between") 
  
  shapleyreftheil <- ALL_FLOWS %>%
    filter(ttoyear(t)<=2100 & ttoyear(t)>=2015) %>%
    inner_join(ykali_dist) %>%
    inner_join(pop %>% mutate(pop=pop/10)) %>% 
    mutate(abcost=-abcost,cdrcost=-cdrcost,ctx=-ctx,gentax=-gentax,err=ygrossd-ykali) %>%
    group_by(t) %>%
    do(owen(zid_theilb,list(c("abcost","err"),c("cdrrev","cdrcost","gentax"), c("transfer","ctx")), data=.)) %>%
    mutate(dec="between") 
  
  print("Theil within") 
  
  shapleyreftheil <- ALL_FLOWS %>%
    filter(ttoyear(t)<=2100 & ttoyear(t)>=2015) %>%
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

get_witch_simple("Y_DIST")
gini <- inner_join(Y_DIST,pop) %>% inner_join(ykali_dist) %>%
  group_by(t) %>%
  summarise(gini=reldist::gini(value*1e6/pop/10,weights=pop/10)-reldist::gini(ykali*1e6/pop/10,weights=pop/10))

theil <- inner_join(Y_DIST,pop) %>% inner_join(ykali_dist) %>%
  group_by(t) %>%
  summarise(theil=dineq::mld_decomp(value*1e6/pop/10, n, pop/10)$mld_decomp$mld_total -dineq::mld_decomp(ykali*1e6/pop/10, n, pop/10)$mld_decomp$mld_total)

ggplot(shapleyref %>% group_by(t,group) %>% summarise(value=sum(value))) +
  geom_area(aes(x=ttoyear(t),y=value*100,fill=as.factor(group)))  +
  geom_line(data=.%>% group_by(t) %>% summarise(value=sum(value)),aes(x=ttoyear(t),y=value*100)) +
  geom_line(data=gini %>% filter(ttoyear(t)<=2100),aes(x=ttoyear(t),y=gini*100),linetype=2,color="blue")

ggplot(shapleyreftheil %>% group_by(t,group,dec) %>% summarise(value=sum(value))) +
  geom_area(aes(x=ttoyear(t),y=value*100,fill=as.factor(group),alpha=dec)) +
  geom_line(data=.%>% group_by(t) %>% summarise(value=sum(value)),aes(x=ttoyear(t),y=value*100)) +
  geom_line(data=theil %>% filter(ttoyear(t)<=2100),aes(x=ttoyear(t),y=theil*100),linetype=2,color="blue")

# ggplot(shapleyrefall %>% filter(ttoyear(t)==2020)) +
#   geom_point(aes(x=n,y=value*100,color=factor))

# sd_dist -> standard deviation to generate an income distribution (lognormal)
# etacdr -> how skewed is ownership of CDR relative to the income distribution (1->distribution neutral,>1 more concentrated to the top)
# etatax -> how regressive or progressive are income taxes used to pay for CDR revenues
# prof_mar -> profit margin of CDR, i.e. cprice/avg_cost 
# rev_gdp -> revenues of CDR over GDP
red_model <- function(sd_dist=1, etacdr=2, etatax=1, prof_mar=2, rev_gdp= 0.05){
  dist <- qlnorm(p=seq(0.05,0.95,by=0.1),mean=1,sdlog= sd_dist)
  dist <- dist/sum(dist)
  out0 <- reldist::gini(dist)
  out <-  reldist::gini(dist + rev_gdp * ((1-1/prof_mar)*dist^etacdr/sum(dist^etacdr) - dist^etatax/sum(dist^etatax)) ) 
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
                           prof_mar=seq(1,10,by=0.5),
                           rev_gdp=seq(0.05,0.2,by=0.05))) %>%
  rowwise() %>%
  mutate(gini=red_model(sd_dist=sd_dist, etacdr=etatax+deta, etatax=etatax, prof_mar=prof_mar, rev_gdp=rev_gdp)["out"]) %>% 
  inner_join(gini0) %>% mutate(dgini=gini-gini0)
  
require(ggrepel)

ggplot(map_toy %>% ungroup() %>% filter(sd_dist==0.5 & rev_gdp==0.1) %>%
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
  facet_wrap(.~paste0(Taxation,": ",etatax)) + 
  ylab('Profit margin for CDR [cprice/avg cost]') +
  xlab(paste0('Intrinstic regressivity of financing CDR [etacdr-etatax]')) +
  labs(fill = 'Gini - gini0',linetype="Equity concentration (relative to income distribution)")
