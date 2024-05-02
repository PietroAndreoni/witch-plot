rm(list = ls())
witch_folder = "../Results_srm/Mixedforplots_2" #Where you're RICE/DICE/RICE50x code is located
#main directory of your results files
main_directory <- witch_folder # by default, the witch source folder
subdir = c("") #can be multiple directories

reg_id = "ed57" #for historical data folder
year0 = 2015
tstep = 5

restrict_files = c("results_") #to all scenarios matching partly at least one of its arguments
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
  aggr=case_when(str_detect(file,"maxiso3")~"maxiso3",
                 str_detect(file,"ed57")~"ed57",
                 .default = "maxiso3"),
  POL=str_extract(file,"(?<=POL).+?(?=_)"),
  nsrm=str_extract(file,"(?<=SRM).+?(?=_)"),
  zinj=str_extract(file,"(?<=INJ).+?(?=_)"),
  timp=str_extract(file,"(?<=IMPT).+?(?=[a-z])"),
  ttype=str_extract(file,"(?<=IMPT[0-9]).+?(?=_)"),
  pimp=str_extract(file,"(?<=IMPP).+?(?=[a-z])"),
  ptype=str_extract(file,"(?<=IMPP[0-9]).+?(?=_)"),
  tend=str_extract(file,"(?<=GE).*"),
  spread=str_extract(file,"(?<=TSPR).*")) %>%
  mutate( nsrm=case_when(nsrm=="brics"~"BRICS",
                   nsrm=="sc"~"UN Security Council",
                   nsrm=="scbrics"~"UN Security Council and BRICS",
                   nsrm=="wp"~"Major Powers",
                   nsrm=="usa"~"USA",
                   nsrm=="ind"~"India",
                   nsrm=="idn"~"Indonesia",
                   nsrm=="bra"~"Brazil",
                   nsrm=="fra"~"France",
                   nsrm=="nga"~"Nigeria",
                   nsrm=="gbr"~"Great Britain",
                   nsrm=="rus"~"Russia",
                   nsrm=="chn"~"China",
                   .default = "no SRM" ),
          POL = ifelse(is.na(POL),"cba",POL),
          timp = ifelse(is.na(timp),"1",timp),
          pimp = ifelse(is.na(pimp),"no SRM",pimp),
          ptype = ifelse(is.na(ptype),"modified",ptype),
          ttype = ifelse(is.na(ttype),"modified",ttype),
          zinj = ifelse(is.na(zinj),"no SRM",zinj),
          spread = ifelse(is.na(spread),"3",spread),
          tend = ifelse(is.na(tend),"2200",tend) ) %>%
    filter(nsrm %in% c("no SRM","USA","Brazil","India","Russia","China") )
}

injton <- function(.x) {
.x %>%
    mutate(injn = as.numeric(ifelse(str_detect(inj,"N"),str_remove(inj,"N"),paste0("-",str_remove(inj,"S")))))
}

SRM <- get_witch("SRM")
W_SRM <- get_witch("W_SRM")
N_SRM <- get_witch("N_SRM")
Z_SRM <- get_witch("Z_SRM")
MIU <- get_witch("MIU")
srm_only <- get_witch("srm_only_region")
IMPACT <- get_witch("IMPACT")
DPRECIP_SRM <- get_witch("DPRECIP_REGION_SRM")
DTEMP_SRM <- get_witch("DTEMP_REGION_SRM")
TEMP <- get_witch("TEMP_REGION")
DAMFRAC <- get_witch("DAMFRAC")
TATM <- get_witch("TATM")
E <- get_witch("E")
coef <- get_witch("climate_region_coef")
coef_T <- get_witch("coeff_T") %>% select(n,V2,value) %>% unique() %>% rename(Coefficient=V2)
coef_P <- get_witch("coeff_P") %>% select(n,V2,value) %>% unique() %>% rename(Coefficient=V2)
pop <- get_witch("pop")
Y <- get_witch("Y")
YGROSS <- get_witch("YGROSS")
ykali <- get_witch("ykali")

sanitized_names <- as.data.frame(unique(W_SRM %>% select(file)) %>% sanitize())
sc <- c("usa","chn","fra","gbr","rus")
brics <-  c("ind","chn","rus","bra","zaf")
wp <-  c("usa","ind","chn","rus")
nsingle <- c("usa","gbr","ind","idn","nga","fra","gbr","rus","chn")

valid_data <- gdx('../data_maxiso3/data_validation.gdx')
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

theme_set(theme_gray(base_size = 7))
theme_set(theme_pubr(base_size = 7))

maps <- map_data("world")
maps=data.table(maps)
maps$iso3 = countrycode(maps$region, origin = 'country.name', destination =  'iso3c')
maps=as_tibble(maps)
reg <- left_join(maps,witchtools::region_mappings$maxiso3) %>% rename(n=maxiso3)

countries_map <- reg %>% 
  filter(iso3!="ATA") %>%
  group_by(n) %>%
  summarise(minlat=min(lat),maxlat=max(lat),meanlat=mean(lat),
            minlong=min(long),maxlong=max(long),meanlong=mean(long) ) 

regpalette_srm <- c("USA"="#E41A1C","China"="#377EB8","India"="#4DAF4A","China"="#984EA3","Brazil"="#FF7F00","Russia"="#5A5A5A","Others"="white","no SRM"="black")

dr <- 0.03
NPVgdploss <- Y %>%
  full_join(YGROSS %>% rename(ykali=value)) %>%
  filter(ttoyear(t)<=2100) %>%
  group_by(n,file) %>%
  summarise(value = sum( (ykali-value)/(1+dr)^(t-1) ) / sum( (ykali)/(1+dr)^(t-1) ) )

####### figure 1
optimal_temperature <- coef_T %>%
  group_by(n) %>%
  summarise(opttemp=-value[Coefficient=="b"]/(2*value[Coefficient=="c"])) 

### main figure: underprovision of SAI vs overprovision
damageshighlat <- NPVgdploss %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)<=2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sanitized_names) %>%
  filter(pimp %in% c("no SRM","1") & nsrm %in% c("no SRM","USA","China","Russia","India","Brazil") ) %>%
  inner_join(countries_map) %>%
  group_by(n) %>%
  mutate(damrel = value - value[COOP=="coop" & nsrm=="no SRM"]) %>%
  filter(COOP=="noncoop" & nsrm!="no SRM") %>%
  ggplot() + 
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point(aes(x=meanlat,
                 y=damrel*100,
                 color=nsrm),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=damrel*100,
                  color=nsrm,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  theme(legend.position="bottom") + 
  theme_pubr() + 
  scale_color_manual(values=regpalette_srm,
                     name="Free driver") +
  scale_fill_manual(values=regpalette_srm,
                    name="Free driver") +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("Damages [% of GDP]") + theme(legend.position = "right",
                                     text=element_text(size=7))

NPVgdploss %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)<=2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sanitized_names) %>%
  filter(pimp %in% c("no SRM","1") & nsrm %in% c("no SRM","USA","India","Brazil","China") ) %>%
  inner_join(countries_map) %>%
  group_by(n) %>%
  mutate(damrel = value - value[COOP=="coop" & nsrm=="USA"]) %>%
  filter(COOP=="noncoop" & !nsrm %in% c("no SRM") ) %>%
  ggplot() + 
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point(aes(x=meanlat,
                 y=damrel*100,
                 color=nsrm),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=damrel*100,
                  color=nsrm,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  theme(legend.position="bottom") + 
  theme_pubr() + 
  scale_color_manual(values=regpalette_srm,
                     name="Free driver") +
  scale_fill_manual(values=regpalette_srm,
                    name="Free driver") +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("Damages [% of GDP]") + theme(legend.position = "right",
                                     text=element_text(size=7))


TEMP %>% 
  rename(temp=value) %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp %in% c("1") & nsrm %in% c("USA","India","Brazil","China") & COOP=="noncoop" ) %>%
  inner_join(optimal_temperature) %>%
  mutate(value=-(temp-opttemp) ) %>%
  full_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = value ),color='black',size=.1) +
  scale_fill_gradient2() + theme_void()+ theme(panel.background = element_rect(fill="white",color="white")) +
  facet_wrap(nsrm~.,)

W_SRM %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2280 & pimp %in% c("1") & nsrm %in% c("USA","India","Brazil","China") ) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),y=value,color=nsrm,linetype=COOP))

get_witch("W_EMI") %>% 
  inner_join(sanitized_names) %>%
  filter(ghg=="co2" & ttoyear(t)<=2280 & pimp %in% c("1") & nsrm %in% c("USA","India","Brazil","China") ) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),y=value,color=nsrm,linetype=COOP))

DPRECIP_SRM %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp %in% c("1") & nsrm %in% c("USA","India","Brazil","China") & COOP=="noncoop" ) %>%
  full_join(reg %>% filter(iso3!='ATA')) %>% 
  #mutate(value=arules::discretize(value,breaks=5)) %>%
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = value ),color='black',size=.1) +
  scale_fill_gradient2() + 
  theme_void()+ theme(panel.background = element_rect(fill="white",color="white")) +
  facet_wrap(nsrm~.,)


DPRECIP_SRM %>%
  filter(ttoyear(t) == 2100) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sanitized_names) %>%
  filter(pimp %in% c("1") & nsrm %in% c("USA","India","Brazil","China") & COOP=="noncoop" ) %>%
  inner_join(countries_map) %>%
  ggplot() + 
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point(aes(x=meanlat,
                 y=value*100,
                 shape=pimp,
                 color=nsrm),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=value*100,
                  color=nsrm,
                  linetype=pimp,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  theme(legend.position="bottom") + 
  theme_pubr() + 
  scale_color_manual(values=regpalette_srm,
                     name="Free driver") +
  scale_fill_manual(values=regpalette_srm,
                    name="Free driver") +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("Damages [% of GDP]") + theme(legend.position = "right",
                                     text=element_text(size=7))# +
  facet_wrap(d~.,)

