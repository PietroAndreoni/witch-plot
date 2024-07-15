rm(list = ls())
witch_folder = "../Results_srm/Coops290424" #Where you're RICE/DICE/RICE50x code is located
#main directory of your results files
main_directory <- witch_folder # by default, the witch source folder
subdir = c("") #can be multiple directories

reg_id = "maxiso3" #for historical data folder
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
E <- get_witch("E")
MIU <- get_witch("MIU")
IMPACT <- get_witch("IMPACT")
DPRECIP_SRM <- get_witch("DPRECIP_REGION_SRM")
DTEMP_SRM <- get_witch("DTEMP_REGION_SRM")
DAMFRAC <- get_witch("DAMFRAC")
TATM <- get_witch("TATM")
TEMP <- get_witch("TEMP_REGION")
coef <- get_witch("climate_region_coef") %>% select(n,V1,value) %>% unique()
coef_T <- get_witch("coeff_T") %>% select(n,V2,file,value) %>% unique() %>% rename(Coefficient=V2)
coef_P <- get_witch("coeff_P") %>% select(n,V2,file,value) %>% unique() %>% rename(Coefficient=V2)

            