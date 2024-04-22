rm(list = ls())
witch_folder = "../Results_srm/Coops13112023" #Where you're RICE/DICE/RICE50x code is located
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
  mutate(
  COOP=case_when(str_detect(file,"_noncoop")~"noncoop",
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
    filter(nsrm!="UN Security Council and BRICS" & zinj!="equator" & pimp!="medium")
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
impact_coeffs <- get_witch("coeff_burke") %>% select(n,V2,value) %>% unique() %>% rename(Coefficient=V2)
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
coop_palette <- c("no SRM"="#00A36C","zero"="#7393B3","low"="#0096FF","high"="#000080")

####### figure 1
optimal_temperature <- impact_coeffs %>%
  group_by(n) %>%
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

srm2100 <- Z_SRM %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & !pimp %in% c("no SRM","low") & !inj %in% c("60N","60S")) %>% 
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
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp %in% c("no SRM","zero","high")) %>% 
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

adjtemp <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(file,n) %>%
  mutate(temp=temp-preind) %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100) %>% 
  inner_join(optimal_temperature) %>%
  mutate(value=abs(temp-(opttemp-preind)) ) %>%
  filter(pimp %in% c("zero","high")) %>%
  ungroup() %>% dplyr::select(-file,-temp,-opttemp) %>%
  pivot_wider(names_from="pimp") %>%
  full_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = zero-high ),color='black',size=.1) +
  scale_fill_gradient2() + theme_void()+ theme(legend.position = "none",
                                               panel.background = element_rect(fill="white",color="white"))


adjprecip <- DPRECIP_SRM %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100) %>%   
  inner_join(sd_prec) %>%
  filter(pimp %in% c("zero","high")) %>%
  mutate(value=abs(prec/sd) ) %>%
  ungroup() %>% dplyr::select(-file,-prec,-sd) %>%
  pivot_wider(names_from="pimp") %>%
  full_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = zero-high ),color='black',size=.1) +
  scale_fill_gradient2() + theme_void() + theme(legend.position = "none",
                                               panel.background = element_rect(fill="white",color="white"))

  
precip2100 <- DPRECIP_SRM %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp %in% c("zero","high") ) %>%   
  inner_join(sd_prec) %>%
  inner_join(N_SRM %>% rename(srm=value))  %>% 
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
             aes(x=meanlat,y=prec/sd,color=pimp),
             alpha=0.2) + 
  stat_smooth(data=.%>%filter(nsrm !="no SRM"),
              aes(x=meanlat,
                  y=prec/sd,
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

damages2100 <- DAMFRAC %>% 
  inner_join(pop %>% rename(pop=value)) %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp %in% c("no SRM","zero","high")) %>%
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
#                        ggarrange(adjtemp,adjprecip,nrow=1),
                        ggarrange(void,damages2100,void,nrow=1,widths=c(0.4,1,0.1)),
              nrow=3,heights=c(0.35,1,1))
ggsave("fig2_coops.png",plot=fig2_coops,width=18, height=16, units="cm")



###### figure 4
Y <- get_witch("Y")
ykali <- get_witch("ykali")

gini <- Y %>% 
  inner_join(pop %>% rename(pop=value)) %>%
  group_by(t,file) %>%
  summarise(gini=reldist::gini(value/pop,pop) )

giniplot <- ggplot(gini %>% 
         inner_join(sanitized_names) %>% 
         filter(ttoyear(t)<=2150)) +
  geom_line(aes(x=ttoyear(t),
                y=gini,
                color=pimp), 
            linewidth=1.2) +
  scale_color_manual(values=coop_palette,
                     name="Precipitation impacts") +
  xlab('') + ylab('Gini') + theme_pubr()

fig4_coops <- ggarrange(globdam,giniplot,nrow=1,common.legend = TRUE)
ggsave("fig4_coops.png",plot=fig4_coops,width=18, height=9, units="cm")



ABATE <- get_witch("ABATECOST")
IMPACT <- get_witch("IMPACT")

make_damages <- function(gt=c(),dg=c(),persistence=1) {
  gdp <- rep(1,length(gt))
  gdp0 <- gdp
  dgdp <- gdp
  for(i in seq(1,length(gt))) {gdp[i+1] <- gdp[i] * (1 + gt[i] + persistence*dg[i] )^5}
  for(i in seq(1,length(gt))) {gdp[i+1] <- gdp0[i] * (1 + gt[i] + persistence*dg[i])^5 }
  for(i in seq(1,length(gt))) {gdp0[i+1] <- gdp0[i] * (1 + gt[i] )^5}
  for(i in seq(1,length(gt))) {dgdp[i] <- (gdp0[i] - gdp[i]) }
  
  return(dgdp) 
}

##### calculate damages
basegt <- inner_join(ykali %>% 
                       filter(file=="ssp2_cba_noncoop"),
                     pop %>% rename(pop=value)) %>%
  group_by(n,file) %>%
  mutate(pc = value/pop, 
         pclead = lead(value/pop,n=1,order_by=t)) %>%
  ungroup() %>%
  mutate(gt = ( pclead / pc ) ^ (1/5) - 1 ) %>% select(t,n,pop,gt)

damages <- full_join(basegt,
                     IMPACT %>% 
                       filter(d %in% c("prec","temp") )) %>%
  group_by(n,d,file) %>%
  do(value = make_damages(.$gt,.$value,persistence=0.2) ) %>%
  unnest(value) %>%
  group_by(n,d,file) %>%
  mutate(t=row_number()) %>%
  ungroup() %>%
  inner_join(pop %>% rename(pop=value)) %>%
  mutate(value=value*pop) %>%
  bind_rows(ABATE %>% mutate(d="abcost"))

damfracs <- damages %>% 
  inner_join(ykali %>% rename(ykali=value)) %>%
  group_by(t,file,d) %>%
  summarise(gdploss=(sum(ykali)-sum(value))/sum(ykali))

globdam <- damfracs %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t) <= 2150 & COOP=="coop") %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),y=-gdploss,fill=d), 
            linewidth=1.2 ) +
  facet_wrap(pimp~.,) +
  xlab("") + ylab("Damages [% of baseline GDP]") + 
  xlab("") + ylab("GDP loss [%]") + theme_pubr()

IMPACT %>%
  filter(d %in% c("prec","temp") & 
           n %in% c("usa","ind","bra","chn","can","fra","mex") & 
           ttoyear(t)<=2100) %>%
  group_by(file,t,d) %>%
 # summarise(med=median(value),min=quantile(value,0.33),max=quantile(value,0.66)) %>%
  inner_join(sanitized_names) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),y=value,color=pimp,linetype=d)) +
  facet_wrap(n~.,)

DPRECIP_SRM %>%
  filter(n %in% c("usa","ind","bra","chn","can","fra","mex") & 
           ttoyear(t)<=2100) %>%
  # summarise(med=median(value),min=quantile(value,0.33),max=quantile(value,0.66)) %>%
  inner_join(sanitized_names) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),y=value,color=pimp)) +
  facet_wrap(n~.,)
  

##### lorenz curve
gdpc <- inner_join(Y,pop %>% rename(pop=value)) %>% 
  mutate(gdpc=value/pop*1e3) %>%
  group_by(file,t) %>% 
  arrange(gdpc) %>%
  group_by(file,t) %>% 
  mutate(cumpop=cumsum(pop)) %>%
  mutate(popmax=max(cumpop))


reg <- left_join(maps,witchtools::region_mappings$maxiso3) %>% rename(n=maxiso3)

countries_map <- reg %>% 
  filter(iso3!="ATA") %>%
  group_by(n) %>%
  summarise(minlat=min(lat),maxlat=max(lat),meanlat=mean(lat),
            minlong=min(long),maxlong=max(long),meanlong=mean(long) )%>%  
  mutate(latitude=abs(round(meanlat/15)*15) ) %>% 
  mutate(latitude=case_when(latitude==15 | n=="ind" ~ "Tropical",
                            latitude==30 ~ "Horse",
                            latitude==45 ~ "45th",
                            latitude==0 ~ "Equatorial",
                            latitude %in% c(60,75) ~ "Polar")) 

ggplot(gdpc %>% 
         filter(ttoyear(t) %in% c(2100) ) %>% 
         inner_join(countries_map) %>% 
         inner_join(sanitized_names)) +
  geom_path(aes(x=cumpop/popmax,
                y=gdpc,
                color=pimp ),
            fill=NA,
            position="dodge",
            linewidth=1.2) +
  scale_color_manual(values=coop_palette) 



###### precipitation impact vs temperature analysis
sanitized_names_noncoop <- as.data.frame(unique(SRM %>% select(file)) %>% sanitize())

main2 <- DPRECIP_SRM %>% 
  cross_join(data.frame(dam=c(1,5))) %>%
  mutate(precdg = - dam*0.01*value^2) %>%
  inner_join(IMPACT %>% 
               filter(d=="temp") %>% 
               rename(tempdg=value)) %>%
  select(-value,-d) %>%
  pivot_longer(c(tempdg,precdg))  %>%
  inner_join(sanitized_names_noncoop) %>%
  mutate(Scenario=case_when(COOP=="noncoop"~"Free-rider",
                            COOP=="coop" & pimp=="no SRM"~"Cooperative",
                            COOP=="coop" & pimp=="zero"~"SAI, asymmetric injection",
                            COOP=="coop" & pimp=="high"~"SAI, symmetric injection",
                            .default = NA) ) %>%
  filter(ttoyear(t) %in% c(2100) & 
           !is.na(Scenario) &
           ( (Scenario=="Cooperative" & name=="tempdg" & dam==1)|
           (Scenario=="Free-rider" & name=="tempdg" & dam==1)|
           (Scenario=="SAI, asymmetric injection" & name=="precdg")|
           (Scenario=="SAI, symmetric injection" & name=="precdg") )
         ) %>%
  ggplot() +
  geom_boxplot(aes(x=Scenario,y=- value*100,fill=name,linetype=as.factor(dam) )) + 
  scale_linetype_manual(values=c(1,2),labels=c("low","high"),name="Precipitation impacts" ) +
  scale_fill_manual(values=c("darkblue","darkred"),labels=c("Precipitation","Temperature"),name="Impact type" ) +
  ylab("growth loss [% GDP]") + facet_wrap(ttoyear(t)~.,)
  
insert <- DPRECIP_SRM %>% 
  cross_join(data.frame(dam=c(1,5))) %>%
  mutate(precdg = - dam*0.01*value^2) %>%
  inner_join(IMPACT %>% 
               filter(d=="temp") %>% 
               rename(tempdg=value)) %>%
  select(-value,-d) %>%
  pivot_longer(c(tempdg,precdg))  %>%
  inner_join(sanitized_names_noncoop) %>%
  mutate(Scenario=case_when(COOP=="noncoop"~"Free-rider",
                            COOP=="coop" & pimp=="no SRM"~"Cooperative",
                            COOP=="coop" & pimp=="zero"~"SAI, asymmetric injection",
                            COOP=="coop" & pimp=="high"~"SAI, symmetric injection",
                            .default = NA) ) %>%
  filter(ttoyear(t)==2100 & 
           !is.na(Scenario) &
           ((Scenario=="SAI, asymmetric injection" & name=="precdg")|
               (Scenario=="SAI, symmetric injection" & name=="precdg") )
  ) %>%
  ggplot() +
  geom_boxplot(aes(x=Scenario,y=- value*100,fill=name,linetype=as.factor(dam) ),
               outlier.shape = NA,alpha=0.5) +
  ylim(c(-0,0.25)) + theme(legend.position="none",axis.title.x=element_blank(),
                           axis.text.x=element_blank(),
                           axis.ticks.x=element_blank()) + ylab("")+
  scale_linetype_manual(values=c(1,2),labels=c("low","high"),name="Precipitation impacts" ) +
  scale_fill_manual(values=c("darkblue","darkred"),labels=c("Precipitation","Temperature"),name="Impact type" ) 

fig_precipdamages <- cowplot::ggdraw(main2) + 
  cowplot::draw_plot(plot=insert,x=0.7,y=0.5,width=0.25,height=0.25)
ggsave("fig_precdam.png",plot=fig_precipdamages,width=15, height=9, units="cm")

fig_countryfocus <- DPRECIP_SRM %>% 
  cross_join(data.frame(dam=c(1,5))) %>%
  mutate(precdg = - dam*0.01*value^2) %>%
  inner_join(IMPACT %>% 
               filter(d=="temp") %>% 
               rename(tempdg=value)) %>%
  select(-value,-d) %>%
  pivot_longer(c(tempdg,precdg))  %>%
  inner_join(sanitized_names_noncoop) %>%
  mutate(Scenario=case_when(COOP=="noncoop"~"Free-rider",
                            COOP=="coop" & pimp=="no SRM"~"Cooperative",
                            COOP=="coop" & pimp=="zero"~"SAI, asymmetric injection",
                            COOP=="coop" & pimp=="high"~"SAI, symmetric injection",
                            .default = NA),
         `Precipitation impacts`=case_when(dam==1~"low",
                        dam==3~"medium",
                        dam==5~"high") ) %>%
  filter(ttoyear(t) == 2100 & 
           !is.na(Scenario) &
           ( (Scenario=="Cooperative" & name=="tempdg" & dam==1)|
                   (Scenario=="SAI, asymmetric injection" & name=="precdg")|
               (Scenario=="SAI, symmetric injection" & name=="precdg") )
  ) %>% 
  filter(n %in% c("ind","rus","usa","chn","bra","idn","mex","nga","pak","mar","fra") ) %>%
  ggplot() +
  geom_point(aes(x=n,
                y=-value*100,
                color=Scenario,
                shape=as.factor(dam)) ) + ylab("growth loss [% GDP]") + xlab("country") +
  scale_linetype_manual(values=c(1,2),labels=c("low","high"),name="Precipitation impacts" ) +
  scale_fill_manual(values=c("darkblue","darkred"),labels=c("Precipitation","Temperature"),name="Impact type" ) 
ggsave("SIfig_countryprecdam.png",plot=fig_countryfocus,width=8.8, height=8, units="cm")


#############
globtemp <- ggplot(land_temp %>%
                     inner_join(sanitized_names) %>%
                     filter(ttoyear(t) <= 2150)) + 
  geom_line(aes(x=ttoyear(t),
                y=value-land_temp0,
                color=pimp), 
            linewidth=1.2) +
  scale_color_manual(values=coop_palette,
                     name="Precipitation impacts") +
  xlab("") + ylab("Average land temperature, pre-industrial increase [°C]")

globemi <- ggplot(E %>% 
                    filter(ttoyear(t) <= 2150) %>%
                    group_by(file,t) %>% 
                    summarise(value=sum(value)) %>%
                    inner_join(sanitized_names)) + 
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=pimp), 
            linewidth=1.2) +
  scale_color_manual(values=coop_palette,
                     name="Precipitation impacts") +
  xlab("") + ylab("Emisssions [CtCO2/yr]")

zonalsrm <- ggplot(Z_SRM %>% 
                     inner_join(sanitized_names) %>% 
                     filter(ttoyear(t) <= 2150 & !is.na(nsrm)) %>%
                     group_by(file,inj) %>%
                     filter(!(min(value)==0 & max(value)==0))) + 
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=pimp),
            linewidth=1) +
  geom_line(data=.%>% 
              group_by(t,file,pimp) %>%
              summarise(value=sum(value)) %>%
              mutate(inj="Global"),
            aes(x=ttoyear(t),
                y=value,
                color=pimp),
            linewidth=1.2 ) +
  scale_color_manual(values=coop_palette,
                     name="Precipitation impacts") +
  facet_wrap(inj~.,) +
  guides(color="none") +
  xlab("") + ylab("SAI injection [TgS/yr]")

fig1_coops <- ggarrange(ggarrange(globtemp,
                                  globemi + theme(legend.position = "none")),
                        zonalsrm,nrow=2,heights=c(1,0.95,1)) 
ggsave("fig1_coops.png",plot=fig1_coops,width=18, height=16, units="cm")

            