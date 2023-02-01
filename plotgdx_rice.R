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

## FIGURE 1
fig1a <- FLOWS %>% mutate(net = transfer + gentax + dacost + ctx + abcost + dacrev) %>% 
  pivot_longer(c(transfer,gentax,dacost,ctx,abcost,dacrev),names_to="Flow") %>% mutate(valuerel=value/gdp,netrel=net/gdp) %>% 
  filter(n=="World" & ttoyear(t) >= 2020 & ttoyear(t) <= 2100 & setting==setting_select & O=="yes" & B=="700" & ssp==2) %>%
  mutate(Source=case_when( Flow=="abcost" ~ "Abatement costs",
                           Flow=="dacost" ~ "Cost of NETs",
                           Flow=="ctx" ~ "Carbon tax",
                           Flow=="gentax" ~ "Other taxes",
                           Flow=="transfer" ~ "Recycling",
                           Flow=="dacrev" ~ "NET revenues" )) %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),y=valuerel,fill=Source),color="black") + 
  geom_line(aes(x=ttoyear(t),y=netrel),color="blue",size=1.2,linetype="dotted") +
  theme_pubr() +
  scale_y_continuous(labels=scales::percent) +
  ylab('Costs and gains, \n global aggregate [% GDP]') + xlab('') + theme(legend.position = "bottom")

###1b: elasticities characterization
fig1b <- ggplot(ineq_weights %>% filter(ttoyear(t) %in% c(2075)) %>%
                  mutate(ineq_elast=case_when(ineq_elast=="carbon_rev" ~ "NET revenus and costs",
                                              ineq_elast=="abatement" ~ "Carbon tax, abatement costs and recycling",
                                              ineq_elast=="tax" ~ "Other taxes" )) %>% 
                  mutate(dist=as.factor(dist)) %>% mutate(dist=fct_relevel(dist,paste0("D",seq(1,10)))) ) +
  geom_line(data=quantiles_ref %>% mutate(dist=as.factor(dist)) %>% mutate(dist=fct_relevel(dist,paste0("D",seq(1,10)))) %>% filter(ttoyear(t) %in% c(2075)) %>% group_by(t,ssp,dist) %>% summarise(med=median(value)),aes(x=dist,y=med,group=t),size=1,linetype=2) +
  geom_line(data=. %>% group_by(ineq_elast,t,ssp,dist) %>% summarise(med=median(value)),aes(x=dist,y=med,color=ineq_elast,group=interaction(t,ineq_elast)),size=2) +
  geom_point(data=. %>%group_by(ineq_elast,t,ssp,dist) %>% summarise(med=median(value)),
             aes(x=dist,y=med,color=ineq_elast),size=2,shape=21,fill="white") + theme_pubr() + xlab('Income distribution') + ylab('% of flow falling on decile d, \n country median') + theme(legend.title = element_blank()) +
  scale_y_continuous(labels=scales::percent) +
  scale_color_manual(values=c("#F8766D","#00BA38","#619CFF")) + theme(legend.position = "bottom")+ guides(color=guide_legend(nrow=2))

fig1 <- ggarrange(fig1a,fig1b)
ggsave("fig1.png",width=13,height=6,dpi=320)

## FIGURE 2
fig2 <- ggplot(EMITOT %>% filter(n %in% c("World") & ttoyear(t) <= 2100 & 
                                      file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                                                  "ssp2_B700p_DISTgeo_COSTbest_TAXbest_NEGbest") ) %>% 
                    mutate(Source=case_when(Source=="eind"~"Industrial emissions",Source=="eland"~ "Land use change",Source=="use"~ "Carbon removal"  ), 
                           O=case_when(O=="yes"~"With overshoot",
                                       O=="no"~"Without overshoot") )  %>% mutate(O=as.factor(O)) %>% 
                    mutate(O=fct_relevel(O,c("With overshoot","Without overshoot"))) )  +
  geom_area(aes(x=ttoyear(t), y=value,fill=Source)) +
  geom_line(data=.%>%group_by(O,file,n,t)%>%summarise(e=sum(value)),aes(x=ttoyear(t), y=e), size=1.2) +
  geom_line(data=CPRICE %>% filter(n %in% c("World") & ttoyear(t) <= 2100 & 
                 file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                             "ssp2_B700p_DISTgeo_COSTbest_TAXbest_NEGbest") ) %>% 
         mutate(O=case_when(O=="yes"~"With overshoot", O=="no"~"Without overshoot")) %>% 
         mutate(O=as.factor(O)) %>% mutate(O=fct_relevel(O,c("With overshoot","Without overshoot"))), 
         aes(x=ttoyear(t),y=2*value/100),color="red",size=1.2,linetype="dotted") +
  geom_text(data=CPRICE  %>% filter(n %in% c("World") & ttoyear(t) <= 2100 & 
                 file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                             "ssp2_B700p_DISTgeo_COSTbest_TAXbest_NEGbest") ) %>% 
            mutate(O=case_when(O=="yes"~"With overshoot", O=="no"~"Without overshoot"))  %>% 
            mutate(O=as.factor(O)) %>% mutate(O=fct_relevel(O,c("With overshoot","Without overshoot"))) %>% 
            filter(n=="World" & B=="700"  & ssp==2) %>% group_by(B,O) %>% filter(value==max(value) | ttoyear(t)==2030),
          aes(x=ttoyear(t),y=2*value/100+3,label=paste(as.character(round(value)),"$/tCO2")),color="red") +
  facet_wrap(O~.,) +
  theme_pubr() +
  scale_fill_manual(values=c("#66FFFF","#FFCC99","#474826")) +
  ylab('Emissions and removal [GtCO2/yr]') + xlab('') + theme(legend.position = "bottom", legend.title = element_blank())

ggsave("fig2.png",width=13,height=6,dpi=320)

## FIGURE 3
scenarios <- E_NEG %>% select_at(file_group_columns) %>% unique()

abcum <- ABATEDEMI %>%
  filter(ttoyear(t) <= 2100) %>%
  group_by(file,n) %>% 
  complete(t=seq(min(t), max(t), 0.2)) %>% 
  mutate(abate=approxfun(t, abate)(t)) %>%
  group_by(file,n) %>% 
  summarise(abate=sum(abate)) 

dacum <- E_NEG %>%
  filter(ttoyear(t) <= 2100) %>%
  group_by(file,n) %>% 
  complete(t=seq(min(t), max(t), 0.2)) %>% 
  mutate(use=approxfun(t, use)(t)) %>%
  group_by(file,n) %>% 
  summarise(use=sum(use)) 

share <- inner_join(abcum,dacum) %>% 
  group_by(file) %>%
  mutate(breaks=discretize(use/(use+abate)*100,breaks=4))

maps <- map_data("world")
maps=data.table(maps)
maps$ISO = countrycode(maps$region, origin = 'country.name', destination =  'iso3c')
maps=as_tibble(maps)
reg <- left_join(maps %>% rename(iso3=ISO),witchtools::region_mappings$enerdata56 %>% rename(n=enerdata56))


require(ggpattern)

main <- inner_join(share,
                    shapleyrefall %>% filter(ttoyear(t)==2070) %>%  
                      group_by(t,n,file) %>% 
                      summarise(cdrineq=sum(value[group %in% c(2,3)]),rdineq=sum(value[group %in% c(3)]),tot=sum(value)) %>% 
                      ungroup() %>% mutate(abineq=tot-cdrineq)) %>%
  mutate(breaks=discretize(use/(use+abate)*100,breaks=4)) %>% ggplot() +
  geom_point(aes(x=use/(use+abate)*100,y=tot*100,fill=breaks,size=use,shape=ttoyear(t)),shape=21,color="black") +
  geom_smooth(aes(x=use/(use+abate)*100,y=tot*100),color="grey",fill=NA,method="lm",linetype=2) +
  geom_bar(data=.%>%
             group_by(breaks) %>% 
             summarise(minx=min(use/(use+abate)*100),
                       maxx=max(use/(use+abate)*100),
                       medy=median(tot*100)),
           aes(x=(minx+maxx)/2,width=maxx-minx,y=medy,color=breaks),stat="identity",fill=NA) +
  geom_bar(data=.%>%
             group_by(breaks) %>% 
             summarise(minx=min(use/(use+abate)*100),
                       maxx=max(use/(use+abate)*100),
                       medy=median(cdrineq*100)),
           aes(x=(minx+maxx)/2,width=maxx-minx,y=medy,color=breaks,fill=breaks),stat="identity",alpha=0.2) +
  geom_bar_pattern(data=.%>%
             group_by(breaks) %>% 
             summarise(minx=min(use/(use+abate)*100),
                       maxx=max(use/(use+abate)*100),
                       medy=median(rdineq*100)),
           aes(x=(minx+maxx)/2,width=maxx-minx,y=medy,color=breaks,fill=breaks,pattern_colour=breaks,pattern_fill=breaks),stat="identity",alpha=0.2,pattern="stripe") +
  geom_text(data=.%>%
             group_by(breaks) %>% 
             summarise(minx=min(use/(use+abate)*100),
                       maxx=max(use/(use+abate)*100),
                       medy=median(cdrineq*100),
                       medytot=median(tot*100)),
           aes(x=(minx+maxx)/2,y=medy+0.05,color=breaks,label=paste0(round(medy/medytot*100,1),"%") )) +
  scale_size_continuous(range=c(3,12)) +
  labs(size="Carbon sequestred [GtCO2]") +
  guides(color="none",fill="none",pattern_fill="none",pattern_colour="none") +
  xlab('Carbon removed \n[% abated + removed]') + ylab('Change in gini index, net-zero year \n [points]') +
  theme_pubr()

map1 <- share %>% 
  filter(file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>%
  inner_join(reg) %>% filter(iso3!="ATA") %>%
  ggplot(aes(x=long,y=lat)) +
  geom_polygon(aes(group = group, fill = breaks),color='black',size=.1) +
  theme_void()+
  theme(legend.position = "none", strip.text.x = element_text(size=12, face="bold"),plot.title = element_text(hjust = 0.5)) +
  labs(fill="% of global carbon removed")

require(cowplot)
fig3a <- ggdraw() +
  draw_plot(main) +
  draw_plot(map1, x = 0.1, y = .6, width = .3, height = .3)

fig3b <- share %>%
  inner_join(shapleyrefall %>%
               group_by(t,n,file,group) %>% 
               summarise(value=sum(value)))  %>%
  group_by(t,breaks,group) %>%
  summarise(med=median(value)) %>%
  filter(ttoyear(t)==2070) %>%
  ggplot() +
  geom_bar(aes(x=breaks,y=med*100,fill=as.factor(group) ),stat="identity",position="stack",color="black") +
  theme_pubr() + xlab("Carbon removed \n[% abated + removed]") + ylab("Change in gini index, net-zero year \n [points]")

  
main <- share %>%
        inner_join(shapleyrefall %>%  
        group_by(t,n,file) %>% 
        summarise(cdrineq=sum(value[group %in% c(2,3)]),tot=sum(value)) %>% 
        ungroup() %>% mutate(abineq=tot-cdrineq)) %>%
        group_by(t,breaks,file) %>%
        summarise(med=median(tot),min=quantile(tot,probs=c(0.33)),max=quantile(tot,probs=c(0.66))) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),y=med*100,color=breaks),size=1.5) +
  geom_ribbon(aes(x=ttoyear(t),ymin=min*100,ymax=max*100,fill=breaks),size=1,alpha=0.2) +
  geom_hline(yintercept=0,color="grey") +
  geom_vline(xintercept=2045,linetype=2) +
  geom_text(x=2042,y=0.25,label="Breakeven year",angle=90) +
  labs(color="Carbon removed \n[% abated + removed]",fill="Carbon removed \n[% abated + removed]") +
  theme_pubr() + xlab("") + ylab("Change in gini index [points]")

insert <- inner_join(PROF_CDR,E_NEG) %>%
  inner_join(YGROSS) %>%
  mutate(cost_avg=cdrcost/use) %>%
  inner_join(CPRICE) %>%
  filter(file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest" & use>=1e-6 & ttoyear(t)>=2030) %>%
  ggplot() +
    geom_line(data=.%>%group_by(t) %>% summarise(cost_avg=median(value*1e-3/cost_avg)),aes(x=ttoyear(t),y=cost_avg)) +
    geom_ribbon(data=.%>%group_by(t) %>% summarise(cost_min=quantile(value*1e-3/cost_avg,0.33),cost_max=quantile(value*1e-3/cost_avg,0.66)),
              aes(x=ttoyear(t),ymin=cost_min,ymax=cost_max),alpha=0.2) +
  geom_hline(yintercept=1) + xlab('') + ylab('Profit margin') + theme_pubr()

fig3c <- ggdraw() +
  draw_plot(main) +
  draw_plot(insert, x = 0.1, y = .6, width = .3, height = .3)

fig3 <- ggarrange(fig3a,fig3c)
ggsave("fig3.png",width=13,height=6,dpi=400)

## FIG 4: overshoot vs no overshoot
fig4 <- share %>% filter(file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest") %>% ungroup() %>% select(-file) %>%
  inner_join(shapleyOall %>%  
               group_by(t,n) %>% 
               summarise(rdineq=sum(value[group %in% c(3)]),cdrineq=sum(value[group %in% c(2,3)]),tot=sum(value)) %>% 
               ungroup() %>% mutate(abineq=tot-cdrineq)) %>%
  group_by(t,breaks) %>%
  summarise(medrd=median(rdineq),med=median(tot),medcdr=median(cdrineq),min=quantile(tot,probs=c(0.33)),max=quantile(tot,probs=c(0.66))) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),y=med*100,color=breaks),linewidth=1.5) +
  geom_ribbon(aes(x=ttoyear(t),ymin=min*100,ymax=max*100,fill=breaks),size=1,alpha=0.2) +
  geom_bar(data=.%>%filter(ttoyear(t) %in% c(2050,2075,2100)),aes(x=ttoyear(t),y=medcdr*100,fill=breaks,color=breaks),stat="identity",position="dodge",width=5,alpha=0.2) +
  geom_bar(data=.%>%filter(ttoyear(t) %in% c(2050,2075,2100)),aes(x=ttoyear(t),y=med*100,color=breaks),fill=NA,stat="identity",position="dodge",width=5) +
#  geom_bar_pattern(data=.%>%filter(ttoyear(t) %in% c(2050,2075,2100)),aes(x=ttoyear(t),y=medrd*100,color=breaks),fill=NA,stat="identity",position="dodge",width=5) +
  geom_hline(yintercept=0,color="grey") +
  labs(color="Carbon removed \n[% abated + removed]",fill="Carbon removed \n[% abated + removed]") +
  theme_pubr() + xlab("") + ylab("Change in gini index [points]")
ggsave("fig4.png",width=7,height=6,dpi=400)

#FIG 5: global inequality and sensitivity
shapleyref %>% inner_join(scenarios) %>% 
  group_by(t,file,group,DIST) %>% summarise(value=sum(value)) %>%
  filter(file %in% c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest")) %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),y=value*100,fill=as.factor(group),color=as.factor(group)),stat="identity") 


a <- shapleyreftheil %>% inner_join(scenarios) %>% 
  group_by(t,file,group,dec,DIST,COST) %>% summarise(value=sum(value)) %>%
  filter(file %in% c("ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest",
                     "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                     "ssp2_B700_DISTepc_COSThigh_TAXbest_NEGbest",
                     "ssp2_B700_DISTgeo_COSThigh_TAXbest_NEGbest") & 
           ttoyear(t) %in% c(2075,2100)) %>% mutate(DIST=as.factor(DIST),group=as.factor(group),COST=as.factor(COST))
levels(a$DIST) <- list("Global north"="geo","Global south"="epc")
levels(a$group) <- list("Abatement costs"="1","CDR costs"="2","CDR transfers"="3")
levels(a$COST) <- list("Low costs"="best","High costs"="high")

ggplot(a) +
  geom_bar(data=. %>% filter(value>0) %>% group_by(t,file,DIST,COST) %>% summarise(value=sum(value)),aes(x=DIST,y=value*100),fill=NA,stat="identity",color="grey",linetype=2) +
  geom_bar(data=.%>% filter(group %in% c("CDR costs","CDR transfers")),aes(x=DIST,y=value*100,fill=group,color=group,alpha=dec),stat="identity",color="black") +
  geom_point(data=.%>% filter(group %in% c("CDR costs","CDR transfers")) %>% group_by(t,file,DIST,COST) %>% summarise(value=sum(value)),
             aes(x=DIST,y=value*100),shape=3,size=2) +
  geom_point(data=. %>% group_by(t,file,DIST,COST) %>% summarise(value=sum(value)),
             aes(x=DIST,y=value*100),shape=1,size=2) +
  facet_grid(COST~ttoyear(t) ,) + xlab('') + ylab('') +
  guides(fill=guide_legend(title="Inequality driver"),alpha=guide_legend(title="Inequality contribution")) +
  scale_fill_manual(values=c("#00BA38","#619CFF")) +
#  geom_text(data=.%>%filter(value>0 & group=="Low costs" & ttoyear(t)==2075 & DIST=="Global north") %>% 
#              group_by(t,file,DIST,COST) %>% summarise(value=sum(value)), aes(x=DIST,y=value*1.05),label="Abatement costs contribution") +
  theme_pubr()
ggsave("fig5.png",width=13,height=10,dpi=400)

## METHODS FIGURE 1: 

# sd_dist -> standard deviation to generate an income distribution (lognormal)
# etacdr -> how skewed is ownership of CDR relative to the income distribution (1->distribution neutral,>1 more concentrated to the top)
# etatax -> how regressive or progressive are income taxes used to pay for CDR revenues
# prof_mar -> profit margin of CDR, i.e. cprice/avg_cost 
# rev_gdp -> revenues of CDR over GDP
red_model <- function(sd_dist=1, etacdr=2, etatax=1, prof_mar=2, rev_gdp= 0.05){
  dist0 <- qlnorm(p=seq(0.05,0.95,by=0.1),mean=1,sdlog= sd_dist)
  dist0 <- dist0/sum(dist0)
  out0 <- reldist::gini(dist0)
  dist <- dist0 + rev_gdp * ((1-1/prof_mar)*dist0^etacdr/sum(dist0^etacdr) - dist0^etatax/sum(dist0^etatax)) 
  dist <- dist/sum(dist)
  out <-  reldist::gini(dist) 
  return(c("out0"=out0*100,"out"=out*100))
}

gini0 <- tibble(sd_dist=c(),gini0=c())
for (sd in seq(0.5,1.5,by=0.5)) {
  gini0 <- rbind(gini0,
                 tibble(sd_dist=sd,gini0=red_model(sd_dist=sd)["out0"]))
}
map_toy <- tibble(expand.grid(sd_dist=seq(0.5,1.5,by=0.5),
                              deta=seq(-1,2,by=0.1),
                              etatax=seq(0.6,1.8,by=0.4),
                              prof_mar=seq(1,10,by=0.5),
                              rev_gdp=seq(0.05,0.2,by=0.05))) %>%
  rowwise() %>%
  mutate(gini=red_model(sd_dist=sd_dist, etacdr=etatax+deta, etatax=etatax, prof_mar=prof_mar, rev_gdp=rev_gdp)["out"]) %>% 
  inner_join(gini0) %>% mutate(dgini=gini-gini0)

require(ggrepel)

ggplot(map_toy %>% ungroup() %>% filter(sd_dist==1 & rev_gdp==0.05) %>%
         mutate(Taxation=as.factor(case_when(round(etatax,1)==0.6 ~ "Regressive tax",
                                             round(etatax,1)==1 ~ "Neutral tax",
                                             round(etatax,1)==1.4 ~ "Best fit",
                                             round(etatax,1)==1.8 ~ "Progressive tax")))) +
  geom_contour_filled(aes(x=deta,y=prof_mar,z=dgini)) +
  geom_contour(aes(x=deta,y=prof_mar,z=dgini),color="red",breaks=c(-1000,0,+1000),size=1.3) +
  geom_vline(data=.%>%
               mutate(etacdr=etatax+deta) %>%
               filter(round(etacdr,1) %in% c(1,1.4,1.8,2.2)) %>%
               mutate(`Equity concentration`=case_when(round(etacdr,1)==1 ~ "Neutral",
                                                       round(etacdr,1)==1.4 ~ "Low",
                                                       round(etacdr,1)==1.8 ~ "Best fit",
                                                       round(etacdr,1)==2.2 ~ "High") ),
             aes(xintercept=deta,linetype=paste0(`Equity concentration`,": ",etacdr)),size=1.3,color="blue") +
  facet_wrap(.~paste0(Taxation,": ",etatax)) + theme_pubr() +
  ylab('Profit margin for CDR [cprice/avg cost]') +
  xlab(paste0('Intrinstic regressivity of financing CDR [etacdr-etatax]')) +
  labs(fill = 'Gini - gini0',linetype="Equity concentration (relative to income distribution)")
ggsave("mfig1.png",width=15,height=10,dpi=320)
