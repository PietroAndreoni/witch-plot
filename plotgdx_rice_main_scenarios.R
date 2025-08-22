rm(list = ls())
main_folder = "../Results_secondround/Main" #Where you're RICE/DICE/RICE50x code is located
witch_folder = main_folder #Where you're RICE/DICE/RICE50x code is located
subdir = c("") #can be multiple directories
gdxtools::igdx("/Library/Frameworks/GAMS.framework/Resources/")

reg_id = "maxiso3sai" #for historical data folder
year0 = 2015
tstep = 5

restrict_files = c("INJsymmetric_","INJfree_") #to all scenarios matching partly at least one of its arguments
exclude_files = c("IMPbhmbest_")
removepattern = c("")

yearmin = 1980
yearmax = 2300

#Initialize default options, load all witch and other functionsget
source('R/witch_functions.R')

#mapping of variables to historical and validation statistics and unit conversion to WITCH units
map_var_hist <- fread("varname_model, set_model, element_model, var_witch, set_witch, element_witch, conv
Y, , , SOCECON, *, gdp-ppp, 1
E, , , Q_EMI, e, co2, 0.2727273
EIND, , , Q_EMI, e, co2ffi, 0.2727273
ELAND, , , Q_EMI, e, co2lu, 0.2727273
pop, , , l, , , 1e-3
K, , , K, g, fg, 1
I, , , I, g, fg, 1
")

#gdxcompaR(Standard gdxcompaR based on typical variables, otherwise edit in gdxcompaR/server.R)
#runApp(appDir = "gdxcompaR/rice")

sanitize <- function(.x) {
.x %>% 
  mutate(COOP=case_when(str_detect(file,"noncoop")~"noncoop",
                 str_detect(file,"coop")~"coop"),
  aggr=case_when(str_detect(file,"maxiso3sai")~"iso3",
                 str_detect(file,"ed58")~"ed58",
                 .default = "maxiso3"),
  POL=str_extract(file,"(?<=POL).+?(?=_)"),
  nsrm=str_extract(file,"(?<=SAI).+?(?=_)"),
  zinj=str_extract(file,"(?<=INJ).+?(?=_)"),
  impacts=str_extract(file,"(?<=IMP).+?(?=_)"),
  scentemp=str_extract(file,"(?<=T).+?(?=_)"),
  scenprec=str_extract(str_remove_all(file,"_POL|_IMP"),"(?<=P).+?(?=_)"),
  trade=str_extract(file,"(?<=TRD).*")) %>%
  mutate(nsrm=case_when(nsrm=="brics"~"BRICS",
                   nsrm=="sc"~"UN Security Council",
                   nsrm=="scbrics"~"UN Security Council and BRICS",
                   nsrm=="wp"~"Cooperative",
                   nsrm=="usa"~"USA",
                   nsrm=="ind"~"India",
                   nsrm=="idn"~"Indonesia",
                   nsrm=="bra"~"Brazil",
                   nsrm=="fra"~"France",
                   nsrm=="nga"~"Nigeria",
                   nsrm=="gbr"~"Great Britain",
                   nsrm=="rus"~"Russia",
                   nsrm=="chn"~"China",
                   nsrm=="aus"~"Australia",
                   nsrm=="usabra"~"USA+Brazil",
                   nsrm=="usaindbrachn"~"USA+Brazil+India+China",
                   nsrm=="usachn"~"USA+China",
                   nsrm=="indbra"~"Brazil+India",
                   nsrm=="chnind"~"China+India",
                   nsrm=="all"~"Cooperative",
                   .default = "no SRM" ),
          POL = ifelse(is.na(POL),"cba",POL),
          zinj = ifelse(zinj=="no","no SAI",zinj),
         ci_imp=str_extract(str_remove(impacts,"bhmspec|bhmtspec|bhmpspec|spec|bhm"),"best|mlo|mhi|lo|hi"),
         ci_p=str_extract(scenprec,"best|obs|lo|up"),
         ci_t=str_extract(scentemp,"best|obs|lo|up"),
         pers_t=str_extract(scentemp,"\\d+\\.?\\d*"),
         pers_p=str_extract(scenprec,"\\d+\\.?\\d*"),
         impacts=str_extract(impacts,"bhmspec|bhmtspec|bhmpspec|spec|bhm"),
         dsc_p=str_extract(scenprec,"area|pop"),
         dsc_t=str_extract(scentemp,"area|pop")) %>% select(-scentemp,-scenprec) %>%
    mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Mitigation",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Mitigation + SAI",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
           impacts=case_when(impacts=="bhm" ~ "BHM",
                             impacts=="bhmspec" ~ "MAIN",
                             impacts=="spec" ~ "SPEC",
                             impacts=="bhm" ~ "BHM",
                             impacts=="bhmpspec" ~ "BHM-P+SPEC",
                             impacts=="bhmtspec" ~ "BHM-T+SPEC",
                             impacts=="ada" ~ "ADA",
                             impacts=="bhmada" ~ "BHM+ADA",
                             .default=impacts),
           pers_p=ifelse(is.na(pers_p) | pers_p==300, "inf", pers_p),
           pers_t=ifelse(is.na(pers_t) | pers_t==300, "inf", pers_t ) ) 
}

injton <- function(.x) {
as.numeric(ifelse(str_detect(.x,"N"),str_remove(.x,"N"),paste0("-",str_remove(.x,"S"))))
}

SAI <- get_witch("SAI")
W_SAI <- get_witch("W_SAI")
N_SAI <- get_witch("N_SAI")
Z_SAI <- get_witch("Z_SAI")
MIU <- get_witch("MIU")
sai_only <- get_witch("sai_only_region")
IMPACT <- get_witch("IMPACT")
DPRECIP_SAI <- get_witch("DPRECIP_REGION_SAI")
DTEMP_SAI <- get_witch("DTEMP_REGION_SAI")
TEMP <- get_witch("TEMP_REGION")
PREC <- get_witch("PRECIP_REGION")
DAMFRAC <- get_witch("DAMFRAC")
DAMAGES <- get_witch("DAMAGES")
TATM <- get_witch("TATM")
coef <- get_witch("climate_region_coef")
Y <- get_witch("Y")
YGROSS <- get_witch("YGROSS")
ykali <- get_witch("ykali")
pop <- get_witch("pop")
TATM_SAI <- get_witch("TATM_SAI")
TATM_GHG <- get_witch("TATM_GHG")

sanitized_names <- as.data.frame(unique(W_SAI %>% select(file)) %>% sanitize()) 
sc <- c("usa","chn","fra","gbr","rus")
brics <-  c("ind","chn","rus","bra","zaf")
wp <-  c("usa","ind","chn","rus")
nsingle <- c("usa","gbr","ind","idn","nga","fra","gbr","rus","chn")

valid_data <- gdx('../data_maxiso3sai/data_validation.gdx')
area <- valid_data["socecon_valid_wdi_sum"] %>% 
  filter(V1=="land" & t=="2") %>% 
  rename(area=value) %>%
  select(n,area)

land_temp <- TEMP %>%
  inner_join(area) %>%
  group_by(t,file) %>%
  summarise(value=weighted.mean(value,area))

tend <- 2150

land_temp0 <- as.numeric(coef %>%
  filter(V1=="alpha_temp") %>%
  select(-file) %>%
  unique() %>%
  inner_join(area) %>%
  summarise(ltemp0=weighted.mean(value,area)))


theme_set(theme_pubr(base_size = 7))

#Add additional region mappings
region_mapping <- witch_region_mapping("../data_maxiso3sai/maxiso3sai.inc")
region_mapping <- rbind(region_mapping,data.table(maxiso3sai = "row", iso3 = "YEM"))
maps <- map_data("world")
maps=data.table(maps)
maps$iso3 = countrycode(maps$region, origin = 'country.name', destination =  'iso3c')
maps=as_tibble(maps)
reg <- left_join(maps,region_mapping) %>% rename(n=maxiso3sai)

countries_map <- reg %>% 
  filter(iso3!="ATA") %>%
  group_by(n) %>%
  summarise(minlat=min(lat),maxlat=max(lat),meanlat=mean(lat),
            minlong=min(long),maxlong=max(long),meanlong=mean(long) )%>%  
  mutate(latitude=abs(round(meanlat/15)*15) ) %>% 
  mutate(hemishpere=ifelse(latitude<15,"Southern","Northern")) %>%
  mutate(latitude=case_when((latitude==15 | n=="ind") & n!="bra"  ~ "Tropical",
                            latitude==0 | n=="bra" ~ "Equatorial",
                            latitude==30 ~ "Subtropical",
                            latitude==45 ~ "Mid latitudes",
                            latitude %in% c(60,75) ~ "High latitudes")) %>%
  mutate(latitude=ordered(latitude,
                          c("Equatorial",
                            "Tropical",
                            "Subtropical",
                            "Mid latitudes",
                            "High latitudes"))) %>%
  inner_join(inner_join(pop %>% filter(t==2) %>% rename(pop=value),
                ykali %>% filter(t==2) %>% rename(gdp=value)) %>% 
  mutate(gdpc=gdp/pop*1e6) %>%
  select(n,gdpc) %>% unique() %>%
  group_by(n) %>% 
  summarise(income_bracket=case_when(gdpc < 1135 ~ "Low",
                             gdpc >= 1135 & gdpc< 4495 ~ "Low-middle",
                             gdpc >= 4495 & gdpc < 13935~ "Middle",
                             gdpc >= 13935 ~ "High")) )

regpalette_srm <- c("Mitigation + SAI"="#121B54",
                    "Mitigation"="#00A36C",
                    "USA"="#c71585",
                    "China"="#377EB8",
                    "India"="#E41A1C",
                    "Brazil"="#FF7F00",
                    "Others"="white",
                    "Free-riding"="black")

dr <- 0.03
NPVgdploss <- Y %>%
  full_join(YGROSS %>% rename(ykali=value)) %>%
  filter(ttoyear(t)<=2100) %>%
  group_by(n,file) %>%
  summarise(value = sum( (ykali-value)/(1+dr)^(t-1) ) / sum( (ykali)/(1+dr)^(t-1) ) )

####### figure 1
PREC <- get_witch("PRECIP_REGION") %>%
  inner_join(get_witch("impact_clivars")  %>%
               pivot_wider(names_from="V2") ) %>%
  mutate(value=value/base_precip) %>% select(t,n,file,value)

##### build damages dataframe
damfrac_type <- get_witch("damfrac_type") %>% 
  pivot_wider(names_from="d") %>%
  inner_join(get_witch("ABATECOST") %>%
  inner_join(get_witch("YGROSS") %>% rename(y0=value)) %>%
  mutate(ab=value/y0)) %>% select(-value) %>%
  inner_join(countries_map) 

gdploss <- Y %>%
  full_join(YGROSS %>% rename(ykali=value)) %>%
  mutate(value=(ykali-value)/ykali )  %>%
  inner_join(sanitized_names) %>%
  group_by_at(c("t","n",setdiff(colnames(sanitized_names),c("nsrm","COOP","Scenario","file","pathdir","zinj"))) ) %>%
  mutate(valuerel_norm=(value-value[nsrm=="Cooperative" & COOP=="coop" & zinj=="free"])/(value[nsrm=="no SRM" & COOP=="coop" & zinj=="free"]-value[nsrm=="Cooperative" & COOP=="coop" & zinj=="free"]),
         valuerel_saicoop=(value-value[nsrm=="Cooperative" & COOP=="coop" & zinj=="free"]),
         valuerel_paris=(value-value[nsrm=="no SRM" & COOP=="coop" & zinj=="free"]))

gdploss_g <- Y %>%
  full_join(YGROSS %>% rename(ykali=value)) %>%
  group_by(file,t) %>%
  summarise(value=sum(ykali-value)/sum(ykali) )  %>%
  inner_join(sanitized_names) %>%
  group_by_at(c("t",setdiff(colnames(sanitized_names),c("nsrm","COOP","Scenario","file","pathdir","zinj"))) ) %>%
  mutate(valuerel=(value-value[nsrm=="Cooperative" & COOP=="coop" & zinj=="free"])/(value[nsrm=="no SRM" & COOP=="coop" & zinj=="free"]-value[nsrm=="Cooperative" & COOP=="coop" & zinj=="free"]),
         valuerel2=(value-value[nsrm=="no SRM" & COOP=="coop" & zinj=="free"])  ) 

damfrac_g <- DAMAGES %>%
  full_join(YGROSS %>% rename(ykali=value)) %>%
  group_by(file,t) %>%
  summarise(value=sum(value)/sum(ykali) ) 

abatefrac <- get_witch("ABATECOST") %>%
  group_by(file,t,n) %>%
  summarise(value=sum(value)) %>%
  inner_join(YGROSS %>% rename(ykali=value)) %>%
  mutate(value = value/ykali,source="ab") %>%
  select(file,n,value,source,t) %>%
  complete()

abatefrac_g <- get_witch("ABATECOST") %>%
  group_by(file,t,n) %>%
  summarise(value=sum(value)) %>%
  inner_join(YGROSS %>% rename(ykali=value)) %>%
  group_by(file,t) %>%
  summarise(value=sum(value),ykali=sum(ykali)) %>%
  ungroup() %>%
  mutate(value = value/ykali,source="ab") 

saifrac <- get_witch("COST_SAI") %>%
  group_by(file,t,n) %>%
  summarise(value=sum(value)) %>%
  inner_join(YGROSS %>% rename(ykali=value)) %>%
  mutate(value = value/ykali,source="sai") %>%
  select(file,n,value,source,t) %>%
  complete()

perc_impact <- get_witch("damfrac_type") %>%
  mutate(source=case_when(str_detect(d,"temp")~"temp",
                          str_detect(d,"prec")~"prec",
                          str_detect(d,"spill")~"spill",
                          .default="others")) %>%
  group_by(t,n,file,source) %>%
  summarise(value=sum(value)) %>%
  ungroup() %>%
  bind_rows(abatefrac) %>%
  bind_rows(saifrac) %>%
  filter(ttoyear(t)==2100) %>%
  group_by(file,n,t) %>%
  mutate(perc=value/sum(value)) %>% 
  complete() %>%
  group_by(file,t,n) %>%
  mutate(Main_source=case_when(perc[source=="temp"]>0.75~"temp",
                               perc[source=="prec"]>0.75~"prec",
                               perc[source=="ab"]>0.75~"ab",
                               .default="mixed"))

brackets <- damfrac_type %>%
  pivot_longer(c(ab,temp,prec),names_to="type") %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t) == 2100) %>%
  group_by(n,type) %>%
  mutate(value=(value-value[nsrm=="Cooperative" & COOP=="coop"])*100 ) %>%
  filter(!(nsrm=="Cooperative" & COOP=="coop")) %>%
  ungroup() %>%
  group_by(type) %>%
  mutate(bracket = arules::discretize(value,
                                      breaks=c(-50,-20,-10,-5,-1,-0.1,0.1,1,5,10,20,50),
                                      method="fixed"))
TATM <- get_witch("TATM")
land_temp_nogeong <- TATM %>% 
  select(-n) %>%
  inner_join(coef %>% 
               filter(V1 %in% c("alpha_temp","beta_temp")) %>% 
               pivot_wider(names_from=V1)) %>%
  inner_join(area) %>%
  group_by(file,t) %>%
  summarise(value=weighted.mean(alpha_temp+beta_temp*value,area))

sec_data <- gdx('../data_maxiso3sai/data_baseline.gdx')
climate_regional_data <- gdx('../data_maxiso3sai/data_mod_climate_regional.gdx')
sai_regional_data <- gdx('../data_maxiso3sai/data_mod_sai.gdx')

clim <- climate_regional_data["climate_region_coef_cmip6_area"]
pop2 <- sec_data["ssp_l"] %>% filter(V1=="ssp2") %>% mutate(t=as.numeric(t)) %>% rename(pop2=value,ssp=V1)
gdp <- sec_data["ssp_ykali"] %>% filter(V1=="ssp2") %>% mutate(t=as.numeric(t)) %>% rename(gdp=value,ssp=V1)
sd_prec <- get_witch("impact_clivars")  %>%
  pivot_wider(names_from="V2") %>%
  mutate(sd=sd_prec/base_precip) %>%
  select(n,sd) %>% unique()
base_prec <- get_witch("impact_clivars")  %>%
  pivot_wider(names_from="V2") %>% 
  mutate(prec0=base_precip/1000) %>% select(n,prec0)%>% unique()
base_temp <- get_witch("impact_clivars")  %>%
  pivot_wider(names_from="V2") %>% 
  mutate(temp0=base_temp) %>% select(n,temp0) %>% unique()
optimal_temp <- get_witch("impact_coef")  %>%
  pivot_wider(names_from="coefs",values_fill = 0) %>%  select(-n) %>%
  full_join(get_witch("impact_clivars")  %>%
  pivot_wider(names_from="V2",values_fill = 0)) %>%
  mutate(opttemp=(TM-2*dev_TM_all_2*base_temp/sd_temp^2)/-(2*(TM_2+dev_TM_all_2/sd_temp^2)) ) 
optimal_prec <- get_witch("impact_coef")  %>%
  pivot_wider(names_from="coefs",values_fill = 0) %>%  select(-n) %>%
  full_join(get_witch("impact_clivars")  %>%
              pivot_wider(names_from="V2",values_fill = 0)) %>%
mutate(optprec=((RR-2*dev_RR_all_2*base_precip/1000/(sd_prec/1000)^2)/-(2*(RR_2+dev_RR_all_2/(sd_prec/1000)^2)) ) )

