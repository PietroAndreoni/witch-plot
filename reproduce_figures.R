# run this script to reproduce all relevant data, figures and SI figures (might take a while)

# reproduce figure 1-4 of main text
main_folder = "../Results_newdata/Main"

source("plotgdx_rice_main_scenarios.R")

imp_select <- "MAIN"
ci_sel <- "best"
downscaling_sel <-"area" 

for(i in seq(1,4)) source(paste0("Figure",i,".R"))

# reproduce SI ANNEX A figures
source("SI_wemi.R")

# reproduce figure 5
main_folder = "../Results_newdata/Injection"

source("plotgdx_rice_main_scenarios.R")
source("Figure5.R")

# reproduce SI ANNEX B figures (impact functions sensitivity)
rm(list = ls())
source("SI_impacts.R")
main_folder = "../Results_newdata/Impacts"

source("plotgdx_rice_main_scenarios.R")

ci_sel <- "best"
downscaling_sel <-"area" 

imp_select <- "SPEC"
for(i in seq(1,4)) source(paste0("Figure",i,".R"))

imp_select <- "BHM"
for(i in seq(1,4)) source(paste0("Figure",i,".R"))

# reproduce SI ANNEX C figures (population weights)

# reproduce SI ANNEX D figures (no constraints)

# reproduce SI ANNEX E figures (substitution)
rm(list = ls())
source("SI_crowdout.R")
