require(forcats)
require(tidyverse)
require(ggpubr)
require(ggrepel)
require(scales)
require(data.table)
require(gdxtools)
require(dineq)

rm(list = ls())
witch_folder = "../../RICE50x" #Where you're RICE/DICE/RICE50x code is located
#main directory of your results files
main_directory <- "../Results_dec22/" # by default, the witch source folder
#main_directory <- "../Results_sens/" # by default, the witch source folder
subdir = c("") #can be multiple directories
#flag to plot scenarios
transfer <- "neutral"
impacts <- "off"
quant <- "tdep"
ssp <- c("ssp1","ssp2","ssp3","ssp4","ssp5")
#ssp <- c("ssp2")
coop <- "noncoop"
budget <-"650"
cdr_opt <- "geo"
region_id = "ed57" #for historical data folder
year0 = 2015
tstep = 5

exclude_files = c("")
restrict_files = c("")
subset_files = c("") 

for (i in ssp) {
restrict_files = append(restrict_files,paste("results_",i,"_C",coop,"_B",budget,"_CDR",cdr_opt,sep=""))
restrict_files = append(restrict_files,paste("results_",i,"_C",coop,"_B",budget,"p_CDR",cdr_opt,sep=""))
restrict_files = append(restrict_files,paste("results_",i,"_C",coop,"_Bref_CDRno",sep=""))
}
restrict_files = restrict_files[-1]

removepattern = c("results_")

#If you want to have significant separations or parts of file names, specify file_separate <- c(type="first|last|separate", sep="_", names="c("file_new"))
file_separate <- c(type="separate", sep="_", names=c("ssp","C","B","CDR"))

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

palette <- region_palette_ed57

#for map plotting
require(tibble)
require(countrycode)
maps <- map_data("world")
maps=data.table(maps)
maps$iso3 = countrycode(maps$region, origin = 'country.name', destination =  'iso3c')
maps=as_tibble(maps)

reg <- left_join(maps,witchtools::region_mappings$enerdata56) %>% rename(n=enerdata56)

cleanreg <- inner_join(witchtools::region_mappings$enerdata56 %>% rename(n=enerdata56),read.csv("external_data/country-and-continent-codes-list.csv") %>% 
                         rename(iso3=Three_Letter_Country_Code) %>% 
                         select(Continent_Name,iso3) ) %>% 
  unique() %>% filter(Continent_Name!="Antarctica" & 
                      !(n=="tur"& Continent_Name=="Europe") & 
                      !(n=="oeu" & Continent_Name %in% c("Asia","North America")) & 
                      !(n=="rjan57"& Continent_Name %in% c("Europe","Asia"))  &
                      !(n=="ris"& Continent_Name=="Europe") &
                      !(n=="rus" & Continent_Name=="Europe") )

europe <- cleanreg %>% filter(Continent_Name=="Europe") %>% select(n) %>% as.list() %>% .[[1]]
westeu <- c("ita","fra","bel","esp","prt","nld")
northeu <- c("gbr","irl","swe","nor","fin","dnk")
mittleu <- c("rfa","aut","sui","pol","rsl","hun","rcz")
esteu <- setdiff(europe,c(westeu,northeu,mittleu) )

asia <- cleanreg %>% filter(Continent_Name=="Asia") %>% select(n) %>% as.list() %>% .[[1]]
eastasia <- c("jpn","cor","chn","tha","vnm","idn","mys","osea")
westasia <- setdiff(asia,eastasia)

cleanreg <- cleanreg %>% mutate(Continent_Name=ifelse(n %in% eastasia, "East Asia",
                                               ifelse(n %in% westasia, "West Asia",
                                               ifelse(n %in% esteu, "East Europe",
                                               ifelse(n %in% westeu, "West Europe",
                                               ifelse(n %in% mittleu, "Central Europe",
                                               ifelse(n %in% northeu, "Northern Europe",Continent_Name))))))) %>% 
  mutate(North = ifelse(n %in% c("usa","can","rus","tur","aus","jpn","kor","chn") | 
                        str_detect(Continent_Name,"Europe"),"yes","no" ))


multiregions <- c("rcam","rsas","rsaf","golf57","rjan57","rsam","meme","noap","noan","osea","oeu","ris","blt")
multinames <- c("Central America","South-Central Asia","Sub-Saharian Africa","Gulf states","Pacific Islands","South America","Middle East","North-East Africa","North-West Africa","South-East Asia","Rest of Europe","Central Asia","Baltic States")
multimatch <- setNames(multinames,multiregions)

names <- reg %>% select(n,region) %>% filter(!is.na(n)) %>% mutate(region = ifelse(n %in% multiregions,multimatch[n],region)) %>% unique() 

cleanreg <- cleanreg %>% inner_join(names)

names_ed57 <- setNames(names %>% select(region) %>% as.list() %>% .[[1]], names %>% select(n) %>% as.list() %>% .[[1]])

file_group_columns <- append(file_group_columns,c("O","Scenario"))

source("make_df_cdr.R")
source("make_shapleys.R")
#source("make_shapleys_sens.R")
