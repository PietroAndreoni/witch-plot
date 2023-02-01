get_witch_simple("ABATECOST")
get_witch_simple("COST_CDR")
get_witch_simple("REV_CDR")
get_witch_simple("CTX")
get_witch_simple("TRANSFER")
get_witch_simple("GENTAX")
get_witch_simple("CPRICE")
get_witch_simple("CPRICE_NEG")
get_witch_simple("Y")
get_witch_simple("ykali")
get_witch_simple("YGROSS")
get_witch_simple("YGROSS_DIST")
get_witch_simple("Y_DIST")
get_witch_simple("ineq_weights")
get_witch_simple("quantiles_ref")
get_witch_simple("E_NEG")
get_witch_simple("ABATEDEMI")
get_witch_simple("EIND")
get_witch_simple("ELAND")
get_witch_simple("pop")

pop <- pop %>% make_scen() %>% rename(pop=value) 

YGROSS <- YGROSS %>% make_scen()  %>% make_global_sum() %>% rename(ygross=value)

ABATECOST <- ABATECOST %>% make_scen() %>% rename(abcost=value)

COST_CDR <- COST_CDR %>% make_scen() %>% rename(cdrcost=value) 

CTX <- CTX %>% make_scen() %>% rename(ctx=value) 

REV_CDR <- REV_CDR %>% make_scen() %>% rename(cdrrev=value)
         
TRANSFER <- TRANSFER %>% make_scen() %>% rename(transfer=value)

GENTAX <- GENTAX %>% make_scen() %>% rename(gentax=value)

CPRICE <- CPRICE %>% make_scen() %>% 
  
CPRICE <- CPRICE %>% 
  rbind(CPRICE %>%
  inner_join(YGROSS) %>%
  group_by(t,file,pathdir,ssp,Scenario,B,DIST,COST,TAX,NEG,O,E) %>%
  summarise(value=weighted.mean(value,ygross)) %>% 
  mutate(n="World"))

CPRICE_NEG <- CPRICE_NEG %>% make_scen() 

PROF_CDR <- full_join(REV_CDR,COST_CDR) %>% mutate(prof=cdrrev-cdrcost)

Y <- Y %>% make_scen() %>% make_global_sum() %>% rename(gdp=value)

Y_DIST <- make_scen(Y_DIST)

YGROSS_DIST <- YGROSS_DIST %>% make_scen()  %>% rename(ygrossd=value)

ykali_dist <- right_join(ykali %>% make_scen() %>% rename(ykali=value),quantiles_ref %>% make_scen()) %>% mutate(ykali=ykali*value) %>% select(-value)

ineq_weights <- ineq_weights %>% make_scen()

ALL_FLOWS <- inner_join(ABATECOST,ineq_weights %>% filter(ineq_elast=="abatement") %>% select(-ineq_elast) ) %>%
  mutate(abcost=abcost*value) %>% select(-value) %>%
  inner_join( inner_join(COST_CDR,ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% select(-ineq_elast) ) %>%
               mutate(cdrcost=cdrcost*value) %>% select(-value)) %>%
  inner_join( inner_join(CTX,ineq_weights %>% filter(ineq_elast=="abatement") %>% select(-ineq_elast) ) %>%
               mutate(ctx=ctx*value) %>% select(-value)) %>%
  inner_join( inner_join(REV_CDR,ineq_weights %>% filter(ineq_elast=="carbon_rev") %>% select(-ineq_elast) ) %>%
               mutate(cdrrev=cdrrev*value) %>% select(-value)) %>%
  inner_join(TRANSFER) %>%
  inner_join(GENTAX) %>%
  inner_join(YGROSS_DIST) %>%
  mutate(yab=ygrossd-abcost,ynet=yab+cdrrev-cdrcost,y=ynet+transfer-ctx-gentax) 

FLOWS <- ALL_FLOWS %>% 
  group_by_at(c("t","pathdir","n",file_group_columns)) %>%
  summarise(transfer=sum(transfer),gentax=sum(-gentax),cdrcost=sum(-cdrcost),ctx=sum(-ctx),abcost=sum(-abcost),cdrrev=sum(cdrrev))

FLOWS <-  rbind(FLOWS, FLOWS %>%
                  group_by_at(c("t","pathdir",file_group_columns)) %>%
                  summarise(transfer=sum(transfer),gentax=sum(gentax),cdrcost=sum(cdrcost),ctx=sum(ctx),abcost=sum(abcost),cdrrev=sum(cdrrev)) %>%
                  mutate(n="World") ) %>%
  full_join(YGROSS %>% filter(t %in% unique(FLOWS$t))) 


E_NEG <- make_global_sum(E_NEG %>% make_scen()) %>%
  rename(use=value)

all_scenarios <- E_NEG %>% ungroup() %>% select_at(file_group_columns) %>% unique() 

dacum <- E_NEG %>% 
  filter(ttoyear(t) <= 2100) %>%
  group_by_at(c("n", "pathdir", file_group_columns)) %>% 
  complete(t=seq(min(t), max(t), 0.2)) %>% mutate(use=approxfun(t, use)(t)) %>%
  group_by_at(c("n", "pathdir", file_group_columns)) %>% 
  summarise(use=sum(use)) %>%
  group_by_at(file_group_columns) %>% 
  mutate(useper = use/use[n=="World"]) 

EIND <- EIND %>% make_scen() %>% make_global_sum() %>% rename(eind=value)

ELAND <- ELAND %>% make_scen() %>% make_global_sum() %>% rename(eland=value)

EMITOT <- full_join(EIND,E_NEG) %>% full_join(ELAND) %>% mutate(use=-use) %>%
  pivot_longer(c(eind,eland,use),names_to="Source")

ABATEDEMI <- make_global_sum(ABATEDEMI %>% make_scen()) %>%
  rename(abate=value)

emabcum <- ABATEDEMI %>%
  filter(ttoyear(t) <= 2100) %>%
  group_by_at(c("n", "pathdir", file_group_columns)) %>% 
  complete(t=seq(min(t), max(t), 0.2)) %>% mutate(abate=approxfun(t, abate)(t)) %>%
  group_by_at(c("n", "pathdir", file_group_columns)) %>% 
  summarise(abate=sum(abate)) 

ginit <- Y_DIST %>%
  group_by_at(c("t","n",file_group_columns)) %>%
  summarise(gini=reldist::gini(value,weights=rep(0.1,10))) %>%
  rbind(Y_DIST %>%
    inner_join(pop %>% mutate(pop=pop/10)) %>%
    group_by_at(c("t",file_group_columns)) %>%
    summarise(gini=reldist::gini(value*1e6/pop,weights=pop)) %>% 
    mutate(n="World")) %>% 
  inner_join(ykali_dist %>%
  group_by_at(c("t","n",file_group_columns)) %>%
  summarise(gini0=reldist::gini(ykali,weights=rep(0.1,10))) %>%
  rbind(ykali_dist %>%
          inner_join(pop %>% mutate(pop=pop/10)) %>%
          group_by_at(c("t",file_group_columns)) %>%
          summarise(gini0=reldist::gini(ykali*1e6/pop,weights=pop)) %>% 
          mutate(n="World")) ) %>%
  mutate(ginirel=gini-gini0)

COSTS_DIST <- YGROSS_DIST %>% 
  full_join(Y_DIST %>% rename(y=value)) %>% 
  make_scen() %>%
  mutate(cost=(ygrossd-y)/ygrossd) 

COSTS <-  YGROSS %>%
  full_join(Y %>% filter(t>=1)) %>%
  mutate(cost=(ygross-gdp)/ygross)
