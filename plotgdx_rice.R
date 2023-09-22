rm(list = ls())
witch_folder = "../RICE50x" #Where you're RICE/DICE/RICE50x code is located
#main directory of your results files
main_directory <- "../Results_05292023" # by default, the witch source folder
subdir = c("") #can be multiple directories

region_id = "ed57" #for historical data folder
year0 = 2015
tstep = 5

restrict_files = c("") #to all scenarios matching partly one of its arguments
exclude_files = c("ext_fin_pos")
removepattern = c("")

file_separate <- c(type="separate", sep="_", names=c("ssp","B","DIST","COST","TAX","NEG"))

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

library(cowplot)

#sanitize name of the scenarios
make_scen <- function(.x) {
  .x %>%
    filter(ttoyear(t)>=2015 & ttoyear(t)<=2100) %>%
    mutate(O = ifelse(str_detect(B,"p"),"no","yes" )) %>%
    mutate(CP = case_when(str_detect(B,"lin") ~ "linear",
                          str_detect(B,"lg") ~ "low growth",
                          str_detect(B,"dc") ~ "differentiated",
                          str_detect(B,"hi") ~ "no ramp-up",
                          str_detect(B,"1300") ~ "2°C",
                          .default="Central")) %>%
    mutate(TECH = case_when(str_detect(DIST,"high") ~ "high storage",
                          str_detect(DIST,"low") ~ "low storage",
                          str_detect(COST,"lgt") ~ "low market growth rate",
                          str_detect(COST,"hgt") ~ "high market growth rate",
                          str_detect(B,"lmiu") ~ "low residual emissions",
                          str_detect(B,"hmiu") ~ "high residual emissions",
                          .default="Central")) %>%
    mutate(COST=str_remove_all(COST,"lgt|hgt"),
           B=str_remove_all(B,"lmiu|hmiu|lin|lg|dc|hi"),
           DIST=str_remove_all(DIST,"high|low") ) %>% 
    mutate(Scenario = case_when(B=="700" ~ "1.5C full",
                                B=="700p" ~ "1.5C peak",
                                B=="1300" ~ "2C full",
                                B=="1300p" ~ "2C peak",
                                B=="ref" ~ "Baseline"),B = str_remove_all(B,"p")) }

file_group_columns <- c(file_group_columns,"O","CP","TECH","pathdir","Scenario")

#sum global values
make_global_sum <- function(.x,vars=c("value")) {
  .x %>% bind_rows(.x %>% 
          group_by_at(c("t",file_group_columns)) %>%
          summarise_at(vars,sum) %>%
          mutate(n="World")) }

#analyze data for plots
source("make_df.R")
source("make_shapleys.R")

#main figures
source("Main_figures.R") #Figure 1-5 and extended Figure 1 (methods)

#Supplementary information figures
source("SI_cp.R") #ANNEX A
