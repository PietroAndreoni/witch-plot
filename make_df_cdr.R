get_witch_simple("TATM")
get_witch_simple("E_NEG")
get_witch_simple("ABATEDEMI")
get_witch_simple("MIU")
get_witch_simple("Y")
get_witch_simple("E")
get_witch_simple("EIND")
get_witch_simple("ELAND")
get_witch_simple("CPC_DIST")
get_witch_simple("Y_DIST")
get_witch_simple("l") 
get_witch_simple("YGROSS")
get_witch_simple("YGROSS_DIST")

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

rank_dac <- dacum %>% filter(n!="World" & B!="ref") %>% group_by_at(file_group_columns) %>% 
  mutate(rank=rank(desc(useper))) %>% 
  arrange(rank,by.group=TRUE) %>% 
  mutate(cumsum = cumsum(useper)) %>% ungroup() %>%
  mutate(major_dac=ifelse(cumsum<.8,"yes","no"))

EIND <- make_global_sum(EIND %>% make_scen()) %>%
  rename(eind=value)

ELAND <- make_global_sum(ELAND %>% make_scen()) %>%
  rename(eland=value)

E <- make_global_sum(E %>% make_scen()) %>%
  rename(e=value)

Y <- make_global_sum(Y %>% make_scen()) %>%
  rename(gdp=value)

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

Y_DIST <- make_scen(Y_DIST)

ginit <- Y_DIST %>%
  group_by_at(c("t","n","pathdir",file_group_columns)) %>%
  summarise(gini=reldist::gini(value,weights=rep(0.1,10))) %>%
  group_by_at(c("n", file_group_columns) ) %>% 
  mutate(gini0 = gini - gini[ttoyear(t)==2020] ) %>%
  group_by(t,n,ssp) %>% 
  mutate(ginirel = (gini - gini[Scenario=="Baseline"]))

ineq_global <- compute_global_inequality()

giniw <- ineq_global %>%
  filter(dist=="D10" & file!="historical") %>% 
  select(t,file,gini_total) %>%
  inner_join(all_scenarios) %>%
  group_by_at(c("t","ssp") ) %>%
  mutate(ginirel = gini_total  - gini_total[Scenario=="Baseline"])

YGROSS_DIST <- YGROSS_DIST %>% make_scen() %>%
  rename(ygrossd=value)

COSTS_DIST <- YGROSS_DIST %>% 
  full_join(Y_DIST %>% rename(y=value)) %>% 
  make_scen() %>%
  mutate(cost=(ygrossd-y)/ygrossd) 

YGROSS <- YGROSS %>% make_scen() %>% make_global_sum() %>% rename(ygross=value)

COSTS <-  YGROSS %>%
  full_join(Y %>% filter(t>=1)) %>% ungroup() %>%
  mutate(cost=(ygross-gdp)/ygross) %>%  
  group_by_at(c("t","ssp") ) %>%
  mutate(costrel = cost  - cost[Scenario=="Baseline"] )

get_witch_simple("ABATECOST")
get_witch_simple("COST_CDR")
get_witch_simple("REV_CDR")
get_witch_simple("CTX")
get_witch_simple("TRANSFER")
get_witch_simple("GENTAX")
get_witch_simple("CPRICE")
get_witch_simple("CPRICE_NEG")
get_witch_simple("ineq_weights")

ABATECOST <- ABATECOST %>% 
  make_scen() %>% 
  make_global_sum() %>%
  rename(abcost=value) %>%
  full_join(YGROSS %>% filter(t %in% unique(ABATECOST$t)) ) %>%
  mutate(abcostgdprel=abcost/ygross) %>% select(-ygross) 

COST_CDR <- COST_CDR %>% 
  make_scen() %>% 
  make_global_sum() %>%
  rename(cdrcost=value) %>%
  full_join(YGROSS %>% filter(t %in% unique(COST_CDR$t)) ) %>%
  mutate(cdrcostgdprel=cdrcost/ygross) %>% select(-ygross) 

CTX <- CTX %>% 
  make_scen() %>%
  make_global_sum() %>%
  rename(ctx=value) %>%
  full_join(YGROSS %>% filter(t %in% unique(CTX$t)) ) %>%
  mutate(ctxgdprel=ctx/ygross) %>% select(-ygross)

REV_CDR <- REV_CDR %>% 
  make_scen() %>% 
  make_global_sum() %>%
  rename(cdrrev=value) %>%
  full_join(YGROSS %>% filter(t %in% unique(REV_CDR$t)) ) %>%
  mutate(cdrrevgdprel=cdrrev/ygross) %>% select(-ygross)

TRANSFER <- TRANSFER %>% make_scen() %>%
  rename(transfer=value)

GENTAX <- GENTAX %>% make_scen() %>%
  rename(gentax=value)

CPRICE <- CPRICE %>% make_scen()
CPRICE <- rbind(CPRICE, full_join(Y %>% filter(n!="World"),CPRICE) %>% 
                  group_by_at(c("t","pathdir",file_group_columns)) %>%
                  summarise(value=weighted.mean(value,gdp,na.rm=TRUE)) %>%
                  mutate(n="World") ) %>%
  rename(cprice=value) 

CPRICE_NEG <- CPRICE_NEG %>% make_scen()
CPRICE_NEG <- rbind(CPRICE_NEG, full_join(E_NEG %>% filter(n!="World"),CPRICE_NEG) %>% 
                      group_by_at(c("t","pathdir",file_group_columns)) %>%
                      summarise(value=weighted.mean(value,use)) %>%
                      mutate(n="World") ) %>%
  rename(cpriceneg=value)

PROF_CDR <- full_join(REV_CDR,COST_CDR) %>%
  mutate(prof=cdrrev-cdrcost)

get_witch_simple("ykali")
get_witch_simple("quantiles_ref")
ykali <- ykali %>% make_scen()
quantiles_ref <- quantiles_ref %>% make_scen()
ineq_weights <- ineq_weights %>% make_scen()

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

get_witch_simple("pop")
gini_dec <- ALL_FLOWS %>%
  inner_join(pop %>% select(t,n,value) %>% unique() %>% rename(pop=value) %>% mutate(pop=pop/10)) %>%
  inner_join(ykali_dist) %>%
  group_by_at(c("t",file_group_columns)) %>%
  summarise(gini0=reldist::gini(ykali*1e6/pop,weights=pop),
            ginig=reldist::gini(ygrossd*1e6/pop,weights=pop),
            gini_yab=reldist::gini(yab*1e6/pop,weights=pop), 
            gini_ynet=reldist::gini(ynet*1e6/pop,weights=pop), 
            gini=reldist::gini(y*1e6/pop,weights=pop)) %>% 
  ungroup() %>% 
  mutate(d0=ginig-gini0,d1=gini_yab-ginig,d2=gini_ynet-gini_yab,d3=gini-gini_ynet,tot=gini-gini0)

FLOWS <- ALL_FLOWS %>% 
  group_by_at(c("t","pathdir","n",file_group_columns)) %>%
  summarise(transfer=sum(transfer),gentax=sum(-gentax),cdrcost=sum(-cdrcost),ctx=sum(-ctx),abcost=sum(-abcost),cdrrev=sum(cdrrev))

FLOWS <-  rbind(FLOWS, FLOWS %>%
                  group_by_at(c("t","pathdir",file_group_columns)) %>%
                  summarise(transfer=sum(transfer),gentax=sum(gentax),cdrcost=sum(cdrcost),ctx=sum(ctx),abcost=sum(abcost),cdrrev=sum(cdrrev)) %>%
                  mutate(n="World") ) %>%
  full_join(Y %>% filter(t %in% unique(FLOWS$t))) %>%
  mutate(trgdprel=transfer/gdp,txgdprel=gentax/gdp) 

get_witch_simple("CPC")

eff <- full_join(emabcum,CPC %>% make_scen() %>% rename(cpc=value) %>% filter(ttoyear(t)==2100) %>% select(-t)) %>% 
  full_join(., dacum) %>%
  mutate(tot = use+abate) %>%
  group_by_at(file_group_columns) %>%
  mutate(share=tot/tot[n=="World"],
         sharedac=use/use[n=="World"],
         shareab=abate/abate[n=="World"]) 

ratio <- ineq_global %>%
  filter(dist %in% c("D1","D10") ) %>%
  group_by_at(c("t","file")) %>%
  mutate(ratio= decile_global /decile_global[dist=="D1"]) %>%
  filter(dist=="D10") %>% select(file,t,ratio) %>% full_join(E_NEG %>% filter(n=="World")) %>% select(-use)

ratioreg <- Y_DIST %>%
  filter(dist %in% c("D1","D10") ) %>%
  group_by_at(c("t","n",file_group_columns)) %>%
  mutate(ratio= value/value[dist=="D1"]) %>%
  filter(dist=="D10") %>% select(-dist,-value) 
