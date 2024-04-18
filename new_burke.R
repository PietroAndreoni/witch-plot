###### functions
solve_linear <- function(base_temp,temp_opt_rescaled,dg) {
  solve(t(matrix(c(0,1,2*base_temp,1,base_temp,base_temp^2,0,1,2*(temp_opt_rescaled)), nrow=3,ncol=3)), c(dg,0,0)) 
}


make_damages <- function(gt=c(),dg=c(),persistence=1) {
  gdp <- rep(1,length(gt))
  gdp0 <- gdp
  dgdp <- gdp
  for(i in seq(1,length(gt))) {gdp[i+1] <- gdp[i] * (1 + gt[i] + persistence*dg[i] )^5}
  for(i in seq(1,length(gt))) {gdp[i] <- gdp[i] * (1 + persistence*dg[i]) }
  for(i in seq(1,length(gt))) {gdp0[i+1] <- gdp0[i] * (1 + gt[i] )^5}
  for(i in seq(1,length(gt))) {dgdp[i] <- (gdp0[i] - gdp[i])/gdp0[i] }
  
  return(dgdp) 
}

climate <- "cmip5"

maps <- map_data("world")
maps=data.table(maps)
maps$iso3 = countrycode(maps$region, origin = 'country.name', destination =  'iso3c')
maps=as_tibble(maps)
reg <- left_join(maps,witchtools::region_mappings$maxiso3) %>% rename(n=maxiso3)

srm_regional_data <- gdx('../data_maxiso3/data_mod_srm_regional.gdx')
climate_regional_data <- gdx('../data_maxiso3/data_mod_climate_regional.gdx')
sec_data <- gdx('../data_maxiso3/data_baseline.gdx')
hist_data <- gdx('../data_maxiso3/data_historical_values.gdx')

if(climate=="cmip6") tatm0 <- 1.13643
if(climate=="cmip5") tatm0 <- 0.977784
max_delta <- 3

temp <- srm_regional_data["srm_temp_full"] %>% mutate(inj_lat=as.numeric(inj_lat),country_lat=as.numeric(country_lat))
prec <- srm_regional_data["srm_precip_full"] %>% mutate(inj_lat=as.numeric(inj_lat),country_lat=as.numeric(country_lat))
if(climate=="cmip5") clim <- climate_regional_data["climate_region_coef_cmip5"]
if(climate=="cmip6") clim <- climate_regional_data["climate_region_coef_cmip6_pop"]

pop <- sec_data["ssp_l"] %>% filter(V1=="ssp2") %>% mutate(t=as.numeric(t)) %>% rename(pop=value,ssp=V1)
gdp <- sec_data["ssp_ykali"] %>% filter(V1=="ssp2") %>% mutate(t=as.numeric(t)) %>% rename(gdp=value,ssp=V1)

a <- seq(-1,2,by=0.05)
b <- seq(-1,0,by=0.01)
t <- seq(1,20,by=1)

tgmin <- -10
tgmax <- 10
curves <- data.frame(expand.grid(a,b,t)) %>% 
  mutate(tg=tatm0+Var1*(Var3-tatm0)+Var2*(Var3-tatm0)^2) %>%
  group_by(Var1,Var2) %>%
  mutate(curve = cur_group_id()) %>% 
  rename(t=Var3) %>%
  group_by(curve) %>%
  filter(max(tg)<tgmax & min(tg)>tgmin) %>%
  ungroup() %>% select(curve,t,tg) 

ggplot(curves %>% filter(curve == 2625)) +
  geom_line(aes(x=t,y=tg,color=curve,group=curve))

gt <- clim %>% 
  filter(V1 %in% c("alpha_temp","base_temp","beta_temp")) %>% 
  pivot_wider(names_from=V1,values_from=value) %>%
  cross_join(curves) %>%
  mutate(temp = alpha_temp + tg*beta_temp) %>%
  mutate(bhmg = 0.0127*(temp-base_temp) - 0.0004871*(temp^2-base_temp^2))

fit <- gt %>%
  mutate(bhmg=bhmg,tg=temp) %>%
  group_by(n) %>%
  do(a = lm(bhmg~I(tg^2)+I(tg),data=.)[[1]][[1]],
     b = lm(bhmg~I(tg^2)+I(tg),data=.)[[1]][[3]],
     c = lm(bhmg~I(tg^2)+I(tg),data=.)[[1]][[2]] ) 
fit$a <- unlist(fit$a)
fit$b <- unlist(fit$b)
fit$c <- unlist(fit$c)
write.csv(fit %>%
            select(n,a,b,c) %>%
            pivot_longer(c(a,b,c),names_to="Coefficient"),"burke_coefficients_original.csv")

temps <- clim %>% 
  filter(V1 %in% c("alpha_temp","beta_temp","base_temp")) %>%
  pivot_wider(names_from=V1) %>%
  mutate(dtemp_opt=alpha_temp + tatm0*beta_temp - 13.0552)%>%
  mutate(dtemp_opt_rescaled=(dtemp_opt)*max_delta/(max(dtemp_opt)-min(dtemp_opt))  ) %>%
  mutate(temp_opt_rescaled=alpha_temp + tatm0*beta_temp - dtemp_opt_rescaled
       # - min(0,quantile(dtemp_opt_rescaled,0.33)), 
       # dtemp_opt_rescaled= dtemp_opt_rescaled - min(0,quantile(dtemp_opt_rescaled,0.33))
       ) %>%
  pivot_longer(c(alpha_temp,beta_temp,base_temp,dtemp_opt,dtemp_opt_rescaled,temp_opt_rescaled)) %>%
  inner_join(fit) %>%
  mutate(g = a + b*value + c*value^2,dg=b+2*c*value) %>%
  select(-a,-b,-c)

maxdt <- max(abs((temps %>% filter(name=="dtemp_opt_rescaled"))$value))

burke_new <- pivot_wider(temps %>% select(-dg,-g)) %>%
  inner_join(temps %>% filter(name=="base_temp") %>% select(n,dg)) %>% 
  group_by(n) %>%
  summarise( a = solve_linear(base_temp,temp_opt_rescaled,dg)[[1]],
             b = solve_linear(base_temp,temp_opt_rescaled,dg)[[2]],
             c = solve_linear(base_temp,temp_opt_rescaled,dg)[[3]] ) %>%
  mutate(a = ifelse(c>0, -a, a ),
         b = ifelse(c>0, -b, b ),
         c = ifelse(c>0, -c, c )) %>% 
  pivot_longer(c(a,b,c),names_to="Coefficient")

rescale <- temps %>%
  filter(name=="dtemp_opt_rescaled") %>%
  mutate(tempfit=ifelse(value>0,2,-2)) %>%
  select(-value) %>%
  inner_join(burke_new %>% rename(newcoeff=value)) %>% 
  inner_join(fit %>% pivot_longer(c(a,b,c),names_to="Coefficient",values_to="oldcoeff")) %>%
  inner_join(temps %>%
               filter(name %in% c("alpha_temp","beta_temp") ) %>%
               group_by(n)%>%
               summarise(temp0 = value[name=="alpha_temp"]+tatm0*value[name=="beta_temp"])) %>%
  group_by(n) %>%
  summarise(new =  newcoeff[Coefficient=="a"] + newcoeff[Coefficient=="b"]*(temp0+tempfit) + newcoeff[Coefficient=="c"]*(temp0+tempfit)^2, 
            old = oldcoeff[Coefficient=="a"] + oldcoeff[Coefficient=="b"]*(temp0+tempfit) + oldcoeff[Coefficient=="c"]*(temp0+tempfit)^2 ) %>%
  unique() %>% mutate(rescale=new/old)
write.csv(burke_new %>% inner_join(rescale %>% select(n,rescale)),"burke_coefficients_modified.csv")

##### plot!
check_max <- burke_new %>%
  group_by(n) %>%
  summarise(max=-value[Coefficient=="b"]/(2*value[Coefficient=="c"])) %>%
  inner_join(clim %>% 
  filter(V1 %in% c("alpha_temp","base_temp","beta_temp")) %>% 
  pivot_wider(names_from=V1,values_from=value)) %>%
  mutate(direction=alpha_temp+beta_temp*tatm0 - max)

newplot <- burke_new %>%
  pivot_wider(names_from="Coefficient") %>%
  full_join(rescale) %>%
  mutate_if(is.numeric,coalesce,1) %>%
  cross_join(data.frame(T=-10:35) ) %>%
  mutate(dg=(a+b*T+c*T^2)/rescale )

oldplot <-  fit %>%
  cross_join(data.frame(T=-10:35) ) %>%
  mutate(dgold=a+b*T+c*T^2)


countries_plot <- c("usa","ind","rus","chn")
f1 <- ggplot(newplot %>% 
         inner_join(pivot_wider(temps %>% select(-dg,-g))) %>% 
         filter(n %in% countries_plot) ) +
  geom_smooth(aes(x=T-(alpha_temp+beta_temp*tatm0),y=dg,color=n)) +
  geom_smooth(data=oldplot %>% 
                inner_join(pivot_wider(temps %>% select(-dg,-g))) %>% 
                filter(n %in% countries_plot),
              aes(x=T-(alpha_temp+beta_temp*tatm0),y=dgold,color=n), 
              linetype=2 ) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  geom_vline(data=check_max %>% 
               filter(n %in% countries_plot),
             aes(xintercept=max-(alpha_temp+beta_temp*tatm0),color=n),linetype=2) +
  ylim(c(-0.1,0.05)) +
  xlim(c(-5,5)) +
  facet_wrap(n~.,) + theme(legend.position = "none") + 
  xlab("Difference with 2015 regional temperature") + ylab("Difference in income growth per capita")
ggsave("DAMAGES_f1.png",plot=f1,dpi=320,width=9,height=9,units="cm")

reg <- left_join(maps,witchtools::region_mappings$maxiso3) %>% rename(n=maxiso3)

countries_map <- reg %>% 
  filter(iso3!="ATA") %>%
  group_by(n) %>%
  summarise(minlat=min(lat),maxlat=max(lat),meanlat=mean(lat),
            minlong=min(long),maxlong=max(long),meanlong=mean(long) )%>%  
  mutate(latitude=abs(round(meanlat/15)*15) ) %>% 
  mutate(latitude=case_when(latitude==15 | n=="ind" ~ "Tropical",
                            latitude==30 ~ "Subtropical",
                            latitude==45 ~ "45th",
                            latitude==0 ~ "Equatorial",
                            latitude %in% c(60,75) ~ "Polar")) 

f21 <- ggplot(reg %>% 
         inner_join(countries_map) %>%
         filter(iso3!='ATA') %>% 
         mutate(latitude=ifelse(n=="row",NA,latitude))) +
  geom_polygon(aes(x = long, y = lat,group = group, fill=latitude),color='grey',size=.1)+
  geom_hline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60,75),
                             latitude=c("45th","Subtropical","Tropical","Equatorial","Tropical","Subtropical","45th","Polar","Polar")),
             aes(yintercept=lats,color=latitude),linetype=2,alpha=0.5) +
  scale_fill_brewer(palette = "Set2",name="Latitude") +
  scale_color_brewer(palette = "Set2",name="Latitude") +
  theme_void() +
  theme(legend.position = "none") 

f22 <- ggplot(pivot_wider(temps %>% select(-dg,-g)) %>% 
         inner_join(countries_map) %>%
         group_by(latitude) %>%
         summarise(opt=median((temp_opt_rescaled-alpha_temp)/beta_temp), 
       low=quantile((temp_opt_rescaled-alpha_temp)/beta_temp,0.25),
       high=quantile((temp_opt_rescaled-alpha_temp)/beta_temp,0.75) ) )  +
  geom_rect(aes(xmin=low,xmax=high,ymin=0,ymax=1,fill=as.factor(latitude) ),alpha=0.3,linewidth=1.2,color=NA) +
  geom_segment(aes(x=opt,xend=opt,y=0,yend=1,color=as.factor(latitude) ),linewidth=1.5) +
  geom_segment(aes(x=0,xend=0,y=0,yend=1),linetype=1,linewidth=1) +
  geom_segment(aes(x=tatm0,xend=tatm0,y=0,yend=1),linetype=2,linewidth=1) +
  xlab("Optimal global warming/cooling (relative to pre-industrial)") + ylab("") +
  scale_fill_brewer(palette = "Set2",name="Latitude") +
  scale_color_brewer(palette = "Set2",name="Latitude") +
  theme( axis.title.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    axis.line.y = element_blank())

f2 <- ggarrange(f21,f22,nrow=2,heights=c(0.6,0.4))
ggsave("DAMAGES_f2.png",plot=f2,dpi=320,width=8.8,height=10,units="cm")

##### calculate damages
basegt <- inner_join(gdp,pop) %>%
  group_by(ssp,n) %>%
  mutate(pc = gdp/pop, pclead = lead(gdp/pop,n=1,order_by=t)) %>%
  ungroup() %>%
  mutate(gt = ( pclead / pc ) ^ (1/5) - 1 )

trcp <- climate_regional_data["temp_region_valid_cmip5"] %>%
  rename(temp=value,rcp=V3) %>%
  inner_join(fit) %>%
  group_by(t,n,rcp,temp) %>%
  summarise(dg=(a+b*temp+c*temp^2), t=as.numeric(t) ) %>% 
  mutate(fit="old") %>%
  bind_rows(climate_regional_data["temp_region_valid_cmip5"] %>%
      rename(temp=value,rcp=V3) %>%
      inner_join(burke_new %>% pivot_wider(names_from=Coefficient)) %>%
      full_join(rescale) %>%  
      group_by(t,n,rcp,temp) %>%
      summarise(dg=(a+b*temp+c*temp^2)/rescale, t=as.numeric(t) ) %>%
      mutate(fit="modified"))

damages <- inner_join(basegt,trcp) %>%
  mutate(per = ifelse(fit=="modified",0.2,1)) %>%
  group_by(ssp,n,rcp,fit) %>%
  do(gdpbhm = make_damages(.$gt,.$dg,persistence=unique(.$per)) ) %>%
  unnest(gdpbhm) %>%
  group_by(ssp,n,rcp,fit) %>%
  mutate(t=row_number()) 

f3 <- ggplot(damages %>%   
         inner_join(trcp) %>%
         inner_join(pivot_wider(temps %>% select(-dg,-g))) %>%
         filter(ssp=="ssp2" & 
                  n %in% c("usa","fra","ind","rus","nga","chn","ita","can") ) ) +
  geom_path(aes(x=temp-(alpha_temp+beta_temp*tatm0),y=gdpbhm,color=rcp,linetype=fit),linewidth=2) +
  geom_point(data=.%>%filter(t %in% c(10,18) ),aes(x=temp-(alpha_temp+beta_temp*tatm0),y=gdpbhm,color=rcp,shape=as.factor(2010+t*5)),size=3) +
  facet_wrap(n~.,scales="free") + guides()
ggsave("DAMAGES_f3.png",plot=f3,dpi=320,width=18,height=16,units="cm")

tsintethic <- cross_join(curves,pivot_wider(temps %>% select(-dg,-g))) %>%
  mutate(temp=alpha_temp+beta_temp*tg) %>% 
  inner_join(fit) %>%
  group_by(t,n,curve,temp) %>%
  summarise(dg=(a+b*temp+c*temp^2), t=as.numeric(t)) %>% 
  mutate(fit="modified")

tglobaltoregmap <- cross_join(curves,pivot_wider(temps %>% select(-dg,-g))) %>%
  mutate(temp=alpha_temp+beta_temp*tg) %>% select(t,curve,n,temp,tg) %>%
  group_by(curve) %>%
  filter(min(tg)>-2 & max(tg)<4)
  
inner_join(basegt,tsintethic) %>%
 mutate(per = ifelse(fit=="modified",0.2,1)) %>%
 group_by(ssp,n,curve,fit) %>%
 do(gdpbhm = make_damages(.$gt,.$dg,persistence=unique(.$per)) ) %>%
 unnest(gdpbhm) %>%
 group_by(ssp,n,curve,fit) %>%
 mutate(t=row_number()) %>%
 inner_join(countries_map) %>%
 inner_join(tglobaltoregmap) %>%
 inner_join(pop) %>%
 # group_by(tg,latitude) %>%
 # summarise(med=median(gdpbhm),
 #           low=quantile(gdpbhm,0.25),
 #           high=quantile(gdpbhm,0.75)) %>%
 ggplot() +
 geom_smooth(aes(x=tg,y=gdpbhm,color=latitude,weight=pop))+
 geom_hline(yintercept=0) + xlab("Global temperature increase") + ylab("Damage fraction, % of gdp")
# geom_point(aes(x=tg,y=gdpbhm,color=latitude,weight=pop)) 
          
valid_data <- gdx('../data_maxiso3/data_validation.gdx')
area <- valid_data["socecon_valid_wdi_sum"] %>% 
  filter(V1=="land" & t=="2") %>% 
  rename(area=value) %>%
  select(n,area)

tglobaltoregmap <- inner_join(trcp,pivot_wider(temps %>% select(-dg,-g))) %>%
  inner_join(area) %>%
  group_by(t,rcp,fit) %>%
  summarise(tg=weighted.mean((temp-alpha_temp)/beta_temp,area))

inner_join(basegt,trcp) %>%
  mutate(per = ifelse(fit=="modified",0.2,1)) %>%
  group_by(ssp,n,rcp,fit) %>%
  do(gdpbhm = make_damages(.$gt,.$dg,persistence=unique(.$per)) ) %>%
  unnest(gdpbhm) %>%
  group_by(ssp,n,rcp,fit) %>%
  mutate(t=row_number()) %>%
  inner_join(countries_map) %>%
  inner_join(pop) %>%
  inner_join(tglobaltoregmap) %>%
  ggplot() +
  geom_smooth(aes(x=tg,
                  y=gdpbhm,
                  color=latitude,
                  weight=pop)) 

