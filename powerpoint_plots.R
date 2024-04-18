rm(list = ls())
witch_folder = "../Results_srm/Mixedforplots" #Where you're RICE/DICE/RICE50x code is located
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
  .x %>% mutate(file=str_remove(file,"results")) %>%
    mutate(COOP=case_when(str_detect(file,"noncoop")~"noncoop",
                          str_detect(file,"coop")~"coop"),
           POL=case_when(str_detect(file,"cba")~"cba",
                         str_detect(file,"bau_impact")~"bau_impact" ),
           aggr=case_when(str_detect(file,"maxiso3")~"maxiso3",
                          str_detect(file,"ed57")~"ed57",
                          .default = "maxiso3"),
           nsrm=case_when(str_detect(file,"_brics_")~"BRICS",
                          str_detect(file,"_sc_")~"UN Security Council",
                          str_detect(file,"scbrics")~"UN Security Council and BRICS",
                          str_detect(file,"wp")~"Major Powers",
                          str_detect(file,"_usa_")~"USA",
                          str_detect(file,"_ind_")~"India",
                          str_detect(file,"_idn_")~"Indonesia",
                          str_detect(file,"_bra_")~"Brazil",
                          str_detect(file,"_fra_")~"France",
                          str_detect(file,"_nga_")~"Nigeria",
                          str_detect(file,"_gbr_")~"Great Britain",
                          str_detect(file,"_rus_")~"Russia",
                          str_detect(file,"_chn_")~"China",
                          .default = "no SRM" ),
           zinj=case_when(str_detect(file,"sovereign")~"sovereign",
                          str_detect(file,"free")~"free",
                          str_detect(file,"equator")~"equator",
                          .default = "no SRM" ),
           timp=case_when(str_detect(file,"kalkuhl")~"KW",
                          str_detect(file,"burke")~"original BHM",
                          str_detect(file,"burkemod")~"modified BHM",
                          .default = "burkemod"),
           pimp=case_when(str_detect(file,"P0")~"zero",
                          str_detect(file,"P1")~"low",
                          str_detect(file,"P3")~"medium",
                          str_detect(file,"P5")~"high",
                          .default = "no SRM") ) %>%
    filter(nsrm %in% c("no SRM","USA","Brazil","India","Russia","China")  & pimp %in% c("no SRM","zero","high") )
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
pop <- get_witch("pop")

sanitized_names <- as.data.frame(unique(SRM %>% select(file)) %>% sanitize())
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



######## high latitude free-drivers
temp_vbase1 <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(file,n) %>%
  mutate(temp=temp-preind) %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & COOP=="noncoop" & pimp %in% c("high","zero") & nsrm %in% c("no SRM","USA","Russia","China")  ) %>% 
  inner_join(N_SRM %>% rename(srm=value))  %>% 
  inner_join(countries_map) %>% 
  ggplot() +
  geom_point(data=.%>%filter(srm!=0 & nsrm !="no SRM"),
             aes(x=meanlat,
                 y=temp,
                 color=nsrm,shape=pimp),size=5) + 
  stat_smooth(aes(x=meanlat,
                  y=temp,
                  color=nsrm,
                  linetype=pimp), se = FALSE, size = 2 ) +
  stat_smooth(data=
                TEMP %>% 
                rename(temp=value) %>%
                inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
                group_by(file,n) %>%
                mutate(temp=temp-preind) %>%
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & pimp %in% c("no SRM") & nsrm %in% c("no SRM")  & COOP=="noncoop")  %>% 
                inner_join(countries_map),
              aes(x=meanlat,
                  y=temp), 
              color="black", 
              se = FALSE ) +
  stat_smooth(data=
                TEMP %>% 
                rename(temp=value) %>%
                inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
                group_by(file,n) %>%
                mutate(temp=temp-preind) %>%
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & pimp %in% c("no SRM") & nsrm %in% c("no SRM")  & COOP=="coop")  %>% 
                inner_join(countries_map),
              aes(x=meanlat,
                  y=temp), 
              color="black", 
              linetype=1,
              se = FALSE ) +
  stat_smooth(data=
                TEMP %>% 
                rename(temp=value) %>%
                inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
                group_by(file,n) %>%
                mutate(temp=temp-preind) %>%
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & pimp %in% c("zero","high") & nsrm %in% c("USA")  & COOP=="coop")  %>% 
                inner_join(countries_map),
              aes(x=meanlat,
                  y=temp,
                  linetype=pimp), 
              color="#7393B3", 
              se = FALSE ) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom",legend.box="vertical") +
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") +
  guides(shape = "none") +
  xlab("") + ylab("Temperature variation \n[°C relative to preindustrial]") + 
  theme_pubr() +
  xlim(c(-60,85))


precip_vbase1 <- DPRECIP_SRM %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & COOP=="noncoop" & pimp %in% c("zero","high") & nsrm %in% c("USA","China","Russia")  ) %>%   
  inner_join(N_SRM %>% rename(srm=value))  %>% 
  inner_join(countries_map) %>% 
  ggplot() +
  geom_point(data=.%>%filter(srm!=0 & nsrm !="no SRM"),aes(x=meanlat,y=prec*100,color=nsrm,shape=pimp),size=5) + 
  stat_smooth(data=.%>%filter(nsrm !="no SRM"),aes(x=meanlat,y=prec*100,color=nsrm,linetype=pimp), se = FALSE, size = 2 ) +
  stat_smooth(data=DPRECIP_SRM %>% rename(prec=value) %>% 
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & COOP=="coop" & pimp %in% c("zero","high") & nsrm %in% c("USA")  ) %>%   
                inner_join(countries_map),
              aes(x=meanlat,
                  y=prec*100,
                  linetype=pimp), 
              color="#7393B3", 
              se = FALSE ) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),aes(xintercept=lats),linetype=2,color="grey",alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=regpalette_srm) +
  theme(legend.position="bottom",legend.box="vertical") +
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") +
  guides(shape = "none") +
  xlab("") + ylab("Precipitation variation \n[% of baseline precipitation]") + 
  theme_pubr() + 
  xlim(c(-60,85)) 

fig1_noncoops1 <- ggarrange(temp_vbase1, precip_vbase1, common.legend = TRUE, labels=c("a","b") )
ggsave("noncoops_1.png",plot=fig1_noncoops1,units=c("cm"),width = 20,height=11.5)

##### equatorial latitude free-drivers
temp_vbase <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(file,n) %>%
  mutate(temp=temp-preind) %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & COOP=="noncoop" & pimp %in% c("high","zero") & nsrm %in% c("India","Brazil")  ) %>% 
  inner_join(N_SRM %>% rename(srm=value))  %>% 
  inner_join(countries_map) %>% 
  ggplot() +
  geom_point(data=.%>%filter(srm!=0 & nsrm !="no SRM"),
             aes(x=meanlat,
                 y=temp,
                 color=nsrm,shape=pimp),size=5) + 
  stat_smooth(aes(x=meanlat,
                  y=temp,
                  color=nsrm,
                  linetype=pimp), se = FALSE, size = 2 ) +
  stat_smooth(data=
                TEMP %>% 
                rename(temp=value) %>%
                inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
                group_by(file,n) %>%
                mutate(temp=temp-preind) %>%
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & pimp %in% c("no SRM") & nsrm %in% c("no SRM")  & COOP=="noncoop")  %>% 
                inner_join(countries_map),
              aes(x=meanlat,
                  y=temp), 
              color="black", 
              se = FALSE ) +
  stat_smooth(data=
                TEMP %>% 
                rename(temp=value) %>%
                inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
                group_by(file,n) %>%
                mutate(temp=temp-preind) %>%
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & pimp %in% c("no SRM") & nsrm %in% c("no SRM")  & COOP=="coop")  %>% 
                inner_join(countries_map),
              aes(x=meanlat,
                  y=temp), 
              color="black", 
              linetype=1,
              se = FALSE ) +
  stat_smooth(data=
                TEMP %>% 
                rename(temp=value) %>%
                inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
                group_by(file,n) %>%
                mutate(temp=temp-preind) %>%
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & pimp %in% c("zero","high") & nsrm %in% c("USA")  & COOP=="coop")  %>% 
                inner_join(countries_map),
              aes(x=meanlat,
                  y=temp,
                  linetype=pimp), 
              color="#7393B3", 
              se = FALSE ) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom",legend.box="vertical") +
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") +
  guides(shape = "none") +
  xlab("") + ylab("Temperature variation \n[°C relative to preindustrial]") + 
  theme_pubr() +
  xlim(c(-60,85))


precip_vbase <- DPRECIP_SRM %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & COOP=="noncoop" & pimp %in% c("zero","high") & nsrm %in% c("India","Brazil")  ) %>%   
  inner_join(N_SRM %>% rename(srm=value))  %>% 
  inner_join(countries_map) %>% 
  ggplot() +
  geom_point(data=.%>%filter(srm!=0 & nsrm !="no SRM"),aes(x=meanlat,y=prec*100,color=nsrm,shape=pimp),size=5) + 
  stat_smooth(data=.%>%filter(nsrm !="no SRM"),aes(x=meanlat,y=prec*100,color=nsrm,linetype=pimp), se = FALSE, size = 2 ) +
  stat_smooth(data=DPRECIP_SRM %>% rename(prec=value) %>% 
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & COOP=="coop" & pimp %in% c("zero","high") & nsrm %in% c("USA")  ) %>%   
                inner_join(countries_map),
              aes(x=meanlat,
                  y=prec*100,
                  linetype=pimp), 
              color="#7393B3", 
              se = FALSE ) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),aes(xintercept=lats),linetype=2,color="grey",alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=regpalette_srm) +
  theme(legend.position="bottom",legend.box="vertical") +
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") +
  guides(shape = "none") +
  xlab("") + ylab("Precipitation variation \n[% of baseline precipitation]") + 
  theme_pubr() + 
  xlim(c(-60,85)) 

fig1_noncoops <- ggarrange(temp_vbase, precip_vbase, common.legend = TRUE, labels=c("a","b") )
ggsave("noncoops_2.png",plot=fig1_noncoops,units=c("cm"),width = 20,height=11.5)


###### only base 
temp_vbasec <- ggplot() +
  stat_smooth(data=
                TEMP %>% 
                rename(temp=value) %>%
                inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
                group_by(file,n) %>%
                mutate(temp=temp-preind) %>%
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & pimp %in% c("no SRM") & nsrm %in% c("no SRM")  & COOP=="noncoop")  %>% 
                inner_join(countries_map),
              aes(x=meanlat,
                  y=temp), 
              color="black", 
              se = FALSE ) +
  stat_smooth(data=
                TEMP %>% 
                rename(temp=value) %>%
                inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
                group_by(file,n) %>%
                mutate(temp=temp-preind) %>%
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & pimp %in% c("no SRM") & nsrm %in% c("no SRM")  & COOP=="coop")  %>% 
                inner_join(countries_map),
              aes(x=meanlat,
                  y=temp), 
              color="black", 
              linetype=1,
              se = FALSE ) +
  stat_smooth(data=
                TEMP %>% 
                rename(temp=value) %>%
                inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
                group_by(file,n) %>%
                mutate(temp=temp-preind) %>%
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & pimp %in% c("zero","high") & nsrm %in% c("USA")  & COOP=="coop")  %>% 
                inner_join(countries_map),
              aes(x=meanlat,
                  y=temp,
                  linetype=pimp), 
              color="#7393B3", 
              se = FALSE ) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom",legend.box="vertical") +
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") +
  guides(shape = "none") +
  xlab("") + ylab("Temperature variation \n[°C relative to preindustrial]") + 
  theme_pubr() +
  xlim(c(-60,85))


precip_vbasec <-  ggplot() +
  stat_smooth(data=DPRECIP_SRM %>% rename(prec=value) %>% 
                inner_join(sanitized_names) %>%
                filter(ttoyear(t)==2100 & COOP=="coop" & pimp %in% c("zero","high") & nsrm %in% c("USA")  ) %>%   
                inner_join(countries_map),
              aes(x=meanlat,
                  y=prec*100,
                  linetype=pimp), 
              color="#7393B3", 
              se = FALSE ) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),aes(xintercept=lats),linetype=2,color="grey",alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=regpalette_srm) +
  theme(legend.position="bottom",legend.box="vertical") +
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") +
  guides(shape = "none") +
  xlab("") + ylab("Precipitation variation \n[% of baseline precipitation]") + 
  theme_pubr() + 
  xlim(c(-60,85)) 

fig1_noncoopsclean <- ggarrange(temp_vbasec, precip_vbasec, common.legend = TRUE, labels=c("a","b") )
ggsave("noncoops_3.png",plot=fig1_noncoopsclean,units=c("cm"),width = 20,height=11.5)
