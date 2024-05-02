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
pop <- get_witch("pop")

sanitized_names <- as.data.frame(unique(SRM %>% select(file)) %>% sanitize() %>% filter(COOP=="coop"))
sc <- c("usa","chn","fra","gbr","rus")
brics <-  c("ind","chn","rus","bra","zaf")
wp <-  c("usa","ind","chn","rus")

valid_data <- gdx('../data_maxiso3/data_validation.gdx')
area <- valid_data["socecon_valid_wdi_sum"] %>% 
  filter(V1=="land" & t=="2") %>% 
  rename(area=value) %>%
  select(n,area)

land_temp <- TEMP %>%
  inner_join(area) %>%
  group_by(t,file) %>%
  summarise(value=weighted.mean(value,area))

land_temp0 <- as.numeric(coef %>%
                           filter(V1=="alpha_temp") %>%
                           inner_join(area) %>%
                           summarise(ltemp0=weighted.mean(value,area)))

theme_set(theme_gray(base_size = 7))
theme_set(theme_pubr(base_size = 7))
coop_palette <- c("no SRM"="#00A36C","0"="#7393B3","1"="#0096FF","2"="#000080")

####### figure 1
optimal_temperature <- coef_T %>%
  group_by(n,file) %>%
  summarise(opttemp=-value[Coefficient=="b"]/(2*value[Coefficient=="c"])) 

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

main_scenarios <- sanitized_names %>% filter(spread==3 & tend==2200 & ptype=="modified" & ttype=="modified")

srm2100 <- Z_SRM %>%
  inner_join(main_scenarios) %>%
  filter(ttoyear(t)==2100 & 
           !pimp %in% c("no SRM","5") & 
           !inj %in% c("60N","60S")) %>% 
  mutate(inj=ifelse(str_detect(inj,"S"), -as.numeric(str_remove(inj,"S")),as.numeric(str_remove(inj,"N")))) %>%
  ggplot() +
  geom_bar(aes(x=as.factor(inj),
               y=value,
               fill=pimp),
           position="dodge",stat="identity",color="black") +
  scale_fill_manual(values=coop_palette,
                     name="Precipitation impacts") +
  xlab("") + ylab("SAI [TGS/yr]") +
  theme_pubr() +
  theme(legend.position = "none",
        text=element_text(size=7))

regtemp2100 <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(file,n) %>%
  mutate(temp=temp-preind) %>%
  inner_join(main_scenarios) %>%
  filter(ttoyear(t)==2100 & pimp %in% c("no SRM","0","1","2")) %>% 
  inner_join(N_SRM %>% rename(srm=value))  %>% 
  inner_join(countries_map) %>% 
  inner_join(optimal_temperature) %>%
  inner_join(pop %>% rename(pop=value)) %>%
  ggplot() +
  geom_point(aes(x=meanlat,
                 y=temp-(opttemp-preind),
                 color=pimp),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=temp-(opttemp-preind),
                  color=pimp,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=coop_palette,
                     name="Precipitation impacts") +
  xlab("") + ylab("Distance from local optimal temperature [°C]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=7))

precip2100 <- DPRECIP_SRM %>% rename(prec=value) %>% 
  inner_join(main_scenarios) %>%
  filter(ttoyear(t)==2100 & pimp %in% c("0","1","2") ) %>%   
  inner_join(countries_map) %>% 
  inner_join(pop %>% rename(pop=value)) %>%
  ggplot() +
  geom_hline(yintercept=0) +
  geom_ribbon(aes(x=meanlat,
                  ymin=-1,
                  ymax=1),
              color="grey",
              linewidth=1,
              alpha=0.2) +
  geom_point(data=.%>%filter(nsrm !="no SRM"),
             aes(x=meanlat,y=prec/0.12,color=pimp),
             alpha=0.2) + 
  stat_smooth(data=.%>%filter(nsrm !="no SRM"),
              aes(x=meanlat,
                  y=prec/0.12,
                  color=pimp,
                  weight=pop), 
              se = FALSE,
              linewidth=2 ) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=coop_palette,
                     name="Precipitation impacts") +
  xlab("") + ylab("Precipitation variation [standard deviations]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=7))


Y <- get_witch("Y")
ykali <- get_witch("ykali")
YGROSS <- get_witch("YGROSS")
ABATECOST <- get_witch("ABATECOST")

dr <- 0.03
NPVgdploss <- Y %>%
  full_join(YGROSS %>% rename(ykali=value)) %>%
  filter(ttoyear(t)<=2100) %>%
  group_by(n,file) %>%
  summarise(value = sum( (ykali-value)/(1+dr)^(t-1) ) / sum( (ykali)/(1+dr)^(t-1) ) )

gdploss <- Y %>%
  filter(ttoyear(t)==2100) %>%
  inner_join(YGROSS %>% rename(ykali=value)) %>%
  inner_join(DAMFRAC %>% rename(dam=value)) %>%
  inner_join(ABATECOST %>% rename(ab=value)) %>%
  mutate(value = (ykali-value)/ykali, abgdp = ab/ykali )

damages2100 <- gdploss %>% 
  inner_join(pop %>% rename(pop=value) %>% filter(ttoyear(t)==2100)) %>%
  inner_join(main_scenarios) %>%
  filter(pimp %in% c("no SRM","0","1","2")) %>%
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
                 color=pimp),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=value*100,
                  color=pimp,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  theme(legend.position="bottom") + 
  theme_pubr() + 
  scale_color_manual(values=coop_palette,
                     name="Precipitation impacts") +
  scale_fill_manual(values=coop_palette,
                     name="Precipitation impacts") +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("Damages [% of GDP]") + theme(legend.position = "right",
                                     text=element_text(size=7))

void <- ggplot() + theme_void() + theme(panel.background = element_rect(fill="white",color="white"))
fig2_coops <- ggarrange(srm2100,
                        ggarrange(regtemp2100,precip2100,nrow=1),
                        ggarrange(void,damages2100,void,nrow=1,widths=c(0.4,1,0.1)),
              nrow=3,heights=c(0.35,1,1))
ggsave("fig2_coops.png",plot=fig2_coops,width=18, height=16, units="cm")
            