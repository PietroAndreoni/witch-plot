rm(list = ls())
witch_folder = "../RICE50x" #Where you're RICE/DICE/RICE50x code is located
#main directory of your results files
main_directory <- "../Results_01092023" # by default, the witch source folder
subdir = c("") #can be multiple directories

region_id = "ed57" #for historical data folder
year0 = 2015
tstep = 5

restrict_files = c("") #to all scenarios matching partly one of its arguments
exclude_files = c("")
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

make_scen <- function(.x) {
  .x %>%
    filter(ttoyear(t)>=2015 & ttoyear(t)<=2100) %>%
    mutate(O = ifelse(str_detect(B,"p"),"no","yes" )) %>%
    mutate(E = ifelse(str_detect(B,"e"),"yes","no" )) %>%
    mutate(Scenario = case_when(B=="700" ~ "1.5C full",
                                B=="700p" ~ "1.5C peak",
                                B=="1300" ~ "2C full",
                                B=="1300p" ~ "2C peak",
                                B=="ref" ~ "Baseline"),B = str_remove(B,"p"),B = str_remove(B,"e")) }

file_group_columns <- c(file_group_columns,"O","E","pathdir","Scenario")

make_global_sum <- function(.x,vars=c("value")) {
  rbind(.x, .x %>% 
          group_by_at(c("t",file_group_columns)) %>%
          summarise_at(vars,sum) %>%
          mutate(n="World"),fill=TRUE) }

make_global_mean <- function(.x,vars=c("value"),w=c("weight") ) {
  rbind(.x, .x %>% 
          group_by_at(c("t","pathdir",file_group_columns)) %>%
          summarise_at(vars, weighted.mean(.,w=w) ) %>%
          mutate(n="World")) }

make_cumulative <- function(.x,columns=c("n","file"),vars=c("value"),yearmax=2100) {      
  .x %>%
    filter(ttoyear(t) <= yearmax) %>%
    group_by_at(columns) %>% 
    complete(t=seq(min(t), max(t), 0.2)) %>% 
    mutate(vars, approxfun(t, .)(t)) %>%
    group_by_at(columns) %>% 
    summarise_at(vars, sum, na.rm=TRUE) }

source("make_df.R")
source("make_shapleys.R")
