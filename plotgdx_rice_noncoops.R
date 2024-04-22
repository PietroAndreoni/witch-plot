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

######### Figure 1
# main <- ggplot(reg %>%
#                  filter(iso3!='ATA') %>% 
#                  mutate(nsrm=case_when(n=="usa"~"USA",
#                                        n=="rus"~"Russia",
#                                        n=="bra"~"Brazil",
#                                        n=="ind"~"India",
#                                        n=="chn"~"China",
#                                        .default="Others"))) +
#   geom_polygon(aes(x = long, y = lat,group = group, fill=nsrm),color='grey',size=.1)+
#   geom_tile(data= Z_SRM %>% 
#               rename(injsrm=value) %>% 
#               inner_join(sanitized_names) %>%
#               filter(ttoyear(t)==2100 & pimp %in% c("zero") & nsrm %in% c("USA","India","Brazil","Russia","China") & injsrm!=0  ) %>% 
#               injton(), 
#             aes(y=injn,height=injsrm/4,x=0,width=360,color=nsrm),fill="white",alpha=0,linewidth=1.2,linetype=1) +
#   geom_tile(data= Z_SRM %>% 
#               rename(injsrm=value) %>% 
#               inner_join(sanitized_names) %>%
#               filter(ttoyear(t)==2100 & pimp %in% c("high") & nsrm %in% c("USA","India","Brazil","Russia","China") & injsrm!=0 ) %>% 
#               injton(), 
#             aes(y=injn,height=injsrm/4,x=0,width=360,color=nsrm),fill="white",alpha=0,linewidth=1.2,linetype=3) +
#   geom_hline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),aes(yintercept=lats),linetype=2,color="grey",alpha=0.5) +
#   theme_void() +
#   theme(legend.position = "none", strip.text.x = element_text(size=12, face="bold"),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) + 
#   scale_fill_manual(values=regpalette_srm) +
#   scale_color_manual(values=regpalette_srm)+ 
#   ylim(c(-60,85))


main <- ggplot(Z_SRM %>% 
         inner_join(sanitized_names) %>% 
         filter(ttoyear(t) <= 2150 & inj!="15S") %>%
         group_by(file,inj) %>%
         filter(!(min(value)==0 & max(value)==0))) + 
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=nsrm,
                linetype=pimp),
            linewidth=1) +
  geom_line(data=.%>% 
              group_by(t,file,pimp,nsrm) %>%
              summarise(value=sum(value)) %>%
              mutate(inj="Global"),
            aes(x=ttoyear(t),
                y=value,
                color=nsrm,
                linetype=pimp),
            linewidth=1.2 ) +
  scale_color_manual(values=regpalette_srm,
                     name="Free-driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") + 
  facet_wrap(inj~.,) +
  xlab("") + ylab("SAI injection [TgS/yr]") + theme(legend.position="none")

temp_vbase <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(file,n) %>%
  mutate(temp=temp-preind) %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp %in% c("high","zero") & nsrm %in% c("no SRM","USA","Russia","China")  ) %>% 
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
                  linetype=pimp), se = FALSE ) +
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
              linetype=2,
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
  filter(ttoyear(t)==2100 & pimp %in% c("zero","high") & nsrm %in% c("USA","China","Russia")  ) %>%   
  inner_join(N_SRM %>% rename(srm=value))  %>% 
  inner_join(countries_map) %>% 
  ggplot() +
  geom_point(data=.%>%filter(srm!=0 & nsrm !="no SRM"),aes(x=meanlat,y=prec*100,color=nsrm,shape=pimp),size=5) + 
  stat_smooth(data=.%>%filter(nsrm !="no SRM"),aes(x=meanlat,y=prec*100,color=nsrm,linetype=pimp), se = FALSE ) +
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
  xlim(c(-60,85)) + ylim(c(-50,50))
# 
# fig1_noncoops_vertical <- cowplot::plot_grid(temp_vbase +
#                                                theme(legend.position="none",
#                                                      axis.line.y=element_blank(),
#                                                      axis.title.y = element_blank(),
#                                                      axis.text.y = element_blank(),
#                                                      axis.ticks.y = element_blank())+
#                                                rotate() +
#                                                scale_y_reverse(),
#                                              main,
#                                              precip_vbase +
#                                                theme(legend.position="none",
#                                                      axis.line.y=element_blank(),
#                                                      axis.title.y = element_blank(),
#                                                      axis.text.y = element_blank(),
#                                                      axis.ticks.y = element_blank()) +
#                                                rotate(),
#                                              ncol = 3,
#                                              align = "h",
#           rel_widths = c(1, 3, 1), rel_heights = c(1, 1, 1))

fig1_noncoops <- cowplot::plot_grid(ggarrange(temp_vbase, precip_vbase, common.legend = TRUE, labels=c("a","b") ), main, ncol = 1, nrow=2,
                                             rel_widths = c(1, 1), rel_heights = c(1, 1.2)) 

ggsave("fig1_noncoops.png",plot=fig1_noncoops,units=c("cm"),width = 20,height=20)

######### Figure 2
right <- DAMFRAC %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp %in% c("no SRM","high") & nsrm %in% c("no SRM","USA","India","Brazil","Russia","China")  ) %>% 
  inner_join(DTEMP_SRM %>% rename(temp=value))  %>%
  inner_join(N_SRM %>% rename(srm=value))  %>% 
  inner_join(countries_map) %>% 
  ggplot() +
#  geom_point(data=.%>%filter(srm==0),aes(x=meanlat,y=value*100,color=nsrm),size=0.5,alpha=0.5) + 
#  geom_point(data=.%>%filter(srm!=0 & nsrm !="no SRM"),aes(x=meanlat,y=value*100,color=nsrm),size=2) + 
  stat_smooth(aes(x=meanlat,y=value*100,color=nsrm,linetype=COOP), se = FALSE ) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") +
  xlab("") + ylab("Damages [% of baseline GDP]") + 
  theme(legend.position="none") + 
  xlab("Damages, high precipitation impacts \n [% GDP]") + ylab("")


left <- DAMFRAC %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp %in% c("no SRM","zero") & nsrm %in% c("no SRM","USA","India","Brazil","Russia","China")  ) %>% 
  inner_join(DTEMP_SRM %>% rename(temp=value))  %>%
  inner_join(N_SRM %>% rename(srm=value))  %>% 
  inner_join(countries_map) %>% 
  ggplot() +
#  geom_point(data=.%>%filter(srm==0),aes(x=meanlat,y=value*100,color=nsrm),size=0.5,alpha=0.5) + 
#  geom_point(data=.%>%filter(srm!=0 & nsrm !="no SRM"),aes(x=meanlat,y=value*100,color=nsrm),size=2,shape=17) + 
  stat_smooth(aes(x=meanlat,y=value*100,color=nsrm,linetype=COOP), se = FALSE ) +
#  geom_point(aes(x=meanlat,y=value*100,color=nsrm), alpha = 0.5 ) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") +
  xlab("") + ylab("Damages [% of baseline GDP]") + 
  theme(legend.position="none") +
  xlab("Damages, no precipitation impacts \n [% GDP]") + ylab("")

delta <- DAMFRAC %>% 
  inner_join(sanitized_names) %>% 
  filter(ttoyear(t)==2100 & pimp %in% c("zero","high") & nsrm %in% c("USA","India","Brazil","Russia","China") ) %>% 
  inner_join(N_SRM %>% rename(srm=value))  %>% 
  inner_join(countries_map) %>% 
  select(-file) %>%
  pivot_wider(names_from=pimp,values_from=value) %>%
  ggplot() +
  stat_smooth(aes(x=meanlat,y=(high-zero)*100,color=nsrm)) +
#  geom_point(aes(x=meanlat,y=(high-zero)*100,color=nsrm),alpha=0.5) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  scale_color_manual(values=regpalette_srm,
                     name="Free driver" ) +
  theme(legend.position="bottom") + 
  xlab("Difference in damages, high - no precipitation \n [% GDP]") + ylab("")

void <- ggplot() + theme_void()
fig2_noncoops <- cowplot::plot_grid(ggarrange(void,left,void,ncol=1,heights=c(0.35,1,0.35)), 
                       delta, 
                       ggarrange(void,right,void,ncol=1,heights=c(0.35,1,0.35)), 
                       ncol = 3, rel_widths = c(1, 1.35, 1), rel_heights = c(1, 1, 1), align="hv") 

ggsave("fig2_noncoops.png",plot=fig2_noncoops,units=c("cm"),width = 18,height=9)

######## temperature and emissions
tempg <- ggplot(land_temp %>%
         inner_join(sanitized_names) %>%
         filter(ttoyear(t) <= tend)) + 
  geom_line(data=.%>%filter(nsrm!="no SRM"),
            aes(x=ttoyear(t),
                y=value-land_temp0,
                color=nsrm,
                linetype=pimp), 
            linewidth=1.2) +
  geom_line(data=.%>%filter(nsrm=="no SRM" & COOP=="noncoop"),
            aes(x=ttoyear(t),
                y=value-land_temp0), 
            linewidth=1.2,color="black") +
  geom_line(data=.%>%filter(nsrm=="no SRM" & COOP=="coop"),
            aes(x=ttoyear(t),
                y=value-land_temp0), 
            linewidth=1.2,color="black",linetype=2) +
  xlab("") + ylab("Average land temperature [°C]") +
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") 

emig <- ggplot(E %>% 
         filter(ttoyear(t) <= tend) %>%
         group_by(file,t) %>% 
         summarise(value=sum(value)) %>%
         inner_join(sanitized_names) ) + 
  geom_line(data=.%>%filter(nsrm!="no SRM"),
            aes(x=ttoyear(t),
                y=value,
                color=nsrm,
                linetype=pimp), 
            linewidth=1.2) +
  geom_line(data=.%>%filter(nsrm=="no SRM" & COOP=="noncoop"),
            aes(x=ttoyear(t),
                y=value), 
            linewidth=1.2,color="black" ) +
  
  geom_line(data=.%>%filter(nsrm=="no SRM" & COOP=="coop"),
            aes(x=ttoyear(t),
                y=value), 
            linewidth=1.2,color="black",linetype=2 ) +
  xlab("") + ylab("Emisssions [CtCO2/yr]")+
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") 

fig3_noncoops <- ggarrange(tempg,emig,nrow=1,common.legend = TRUE)
ggsave("fig3_noncoops.png",plot=fig3_noncoops,width=18, height=9, units="cm")



###### gini index/lorenz curve/welfare analysis
Y <- get_witch("Y")
ykali <- get_witch("ykali")

gini <- Y %>% 
  inner_join(pop %>% rename(pop=value)) %>%
  group_by(t,file) %>%
  summarise(gini=reldist::gini(value/pop,pop),
            theil=dineq::theil.wtd(value/pop,pop))

giniplot <- ggplot(gini %>% 
         inner_join(sanitized_names) %>% 
         filter(ttoyear(t)<=tend)) +
  geom_line(data=.%>%filter(nsrm!="no SRM"),
            aes(x=ttoyear(t),
                y=gini,
                color=nsrm,
                linetype=pimp), 
            linewidth=1.2) +
  geom_line(data=.%>%filter(nsrm=="no SRM" & COOP=="noncoop"),
            aes(x=ttoyear(t),
                y=gini), 
            linewidth=1.2,color="black") +
  geom_line(data=.%>%filter(nsrm=="no SRM" & COOP=="coop"),
            aes(x=ttoyear(t),
                y=gini), 
            linewidth=1.2,color="black",linetype=2) +
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") + 
  xlab("") + ylab("Gini index (between country inequality)")

gdploss <- Y %>% 
  inner_join(ykali %>% rename(ykali=value)) %>%
  group_by(t,file) %>%
  summarise(gdploss=(sum(ykali)-sum(value))/sum(ykali))

globdam <- gdploss %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=tend & pimp %in% c("no SRM","zero","high") & nsrm %in% c("no SRM","USA","India","Brazil","Russia","China")  ) %>%
  ggplot() +
  geom_line(data=.%>%filter(nsrm!="no SRM"),
            aes(x=ttoyear(t),
                y=gdploss,
                color=nsrm,
                linetype=pimp), 
            linewidth=1.2) +
  geom_line(data=.%>% filter(nsrm=="no SRM" & COOP=="noncoop"),
            aes(x=ttoyear(t),
                y=gdploss), 
            linewidth=1.2,color="black") +
  geom_line(data=.%>% filter(nsrm=="no SRM" & COOP=="coop"),
            aes(x=ttoyear(t),
                y=gdploss), 
            linewidth=1.2,color="black",linetype=2) +
  scale_color_manual(values=regpalette_srm,name="Free driver") +
  scale_linetype_manual(values=c(2,1),name="Precipitation impacts") + 
  xlab("") + ylab("Fraction of GDP loss")

fig4_noncoops <- ggarrange(globdam,giniplot,nrow=1,common.legend = TRUE)
ggsave("fig4_noncoops.png",plot=fig4_noncoops,width=18, height=9, units="cm")

#########
IMPACT %>%
  filter(d %in% c("prec","temp") & 
           n %in% c("usa","ind","bra","chn","can","fra","mex") & 
           ttoyear(t)<=2150) %>%
  group_by(file,t,d) %>%
  # summarise(med=median(value),min=quantile(value,0.33),max=quantile(value,0.66)) %>%
  inner_join(sanitized_names) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),y=value,color=file,linetype=d)) +
  facet_wrap(n~.,)


###### precipitation impact vs temperature analysis

DPRECIP_SRM %>% 
  mutate(precdg = - 0.05*value^2) %>%
  inner_join(IMPACT %>% 
               filter(d=="temp") %>% rename(tempdg=value)) %>%
  select(-value,-d) %>%
  pivot_longer(c(tempdg,precdg))  %>%
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 & pimp=="zero") %>%
  ggplot() +
  geom_boxplot(aes(x=nsrm,y=- value,fill=name))
