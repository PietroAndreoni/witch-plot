topt_imp <- clim %>% 
  filter(V1 %in% c("alpha_temp","base_temp","beta_temp")) %>% 
  pivot_wider(names_from=V1,values_from=value) %>%
  rowwise() %>%
  mutate(topt_bhm = ( (25 - base_temp) - alpha_temp ) / beta_temp, 
         topt_kw =  (5.7 - alpha_temp) / beta_temp,
         dtbk = alpha_temp - 12.5)

a <- seq(-1,2,by=0.05)
b <- seq(-1,0,by=0.01)
t <- seq(1,20,by=1)

tgmin <- 0
tgmax <- 3
curves <- data.frame(expand.grid(a,b,t)) %>% 
  mutate(tg=1.14+Var1*(Var3-1)+Var2*(Var3-1)^2) %>%
  group_by(Var1,Var2) %>%
  mutate(curve = cur_group_id()) %>% 
  rename(t=Var3) %>%
  group_by(curve) %>%
  filter(max(tg)<tgmax & min(tg)>tgmin) %>%
  ungroup() %>% select(curve,t,tg) 

ggplot(curves) +
  geom_line(aes(x=t,y=tg,color=curve,group=curve))

gt <- clim %>% 
  filter(V1 %in% c("alpha_temp","base_temp","beta_temp")) %>% 
  pivot_wider(names_from=V1,values_from=value) %>%
  cross_join(curves) %>%
  mutate(temp = alpha_temp + tg*beta_temp) %>%
  group_by(n,curve) %>%
  mutate(tempm1 = lag(temp,n=1, order_by=t)) %>%
  mutate(bhmg = 0.0127*(temp-base_temp) - 0.0004871*(temp^2-base_temp^2), 
         kwg = (0.00641+0.00345)*(temp-tempm1) - (.00105+0.000718)*(temp-tempm1)/5*(5*temp + 2*(temp-tempm1)) ) %>%
  mutate(kwg=ifelse(is.na(kwg),0,kwg)) 

gtg <- gt %>%
  inner_join(inner_join(gdp,pop) %>% mutate(gdpc=gdp/pop*1e6)) %>%
  group_by(curve,t,tg) %>%
  summarise(kwg = weighted.mean(kwg,w=gdpc),bhmg = weighted.mean(bhmg,w=gdpc))

ggplot(gtg %>% filter(t<18)) +
  geom_point(aes(x=tg,y=bhmg)) + 
  geom_smooth(aes(x=tg,y=bhmg),fullrange=TRUE) +
  geom_hline(yintercept=0,color="grey")

basegt <- inner_join(gdp,pop) %>%
  group_by(ssp,n) %>%
  mutate(pc = gdp/pop, pclead = lead(gdp/pop,n=1,order_by=t)) %>%
  ungroup() %>%
  mutate(gt = ( pclead / pc ) ^ (1/5) - 1 )

damages <- inner_join(basegt,gt) %>% 
  filter(!is.na(gt)) %>%
  group_by(ssp,n,curve) %>%
  mutate(gdp0=gdp[t==1],
         gdplag=lag(gdp,n=1, order_by=t),
         poplag=lag(pop,n=1, order_by=t)) %>%
  mutate(poplag=ifelse(t==1,pop[t==1],poplag),
         gdplag=ifelse(t==1,gdp[t==1],gdplag) ) %>%
  group_by(ssp,n,curve) %>%
  arrange(t, .by_group = TRUE) %>%
  mutate(gdpbhm = gdp0 * cumprod(pop/poplag * (1 + gt + bhmg)^5),
         gdpkw = gdp0 * cumprod(pop/poplag * (1 + gt + kwg)^5)) %>%
  ungroup() %>% mutate(dbhm = (gdp - gdpbhm)/gdp, 
                       dkw = (gdp - gdpkw)/gdp) 

damages <- damages %>%   
  group_by(ssp,n,curve) %>%
  arrange(t, .by_group = TRUE) %>%
  mutate(warming = ifelse(temp - lag(temp,n=1,order_by=t)>=0 | is.na(lag(temp)),"yes","no")) 
  
ggplot(damages %>% filter(n %in% c("usa","can","ind","gbr","chn","nor") & t<18)) +
  geom_point(aes(x=temp,y=dbhm,color=n)) + 
  geom_hline(yintercept=0,color="grey") +
  geom_vline(aes(xintercept=alpha_temp,color=n)) +
  stat_smooth(aes(x=temp,y=dbhm,group=n),method="loess") +
  facet_wrap(n~.,) +
  ylim(c(-1,1))

ggplot(damages %>% filter(n %in% c("usa") & t<18)) +
  geom_point(aes(x=tg,y=dbhm,color=n)) + 
  geom_hline(yintercept=0,color="grey")

fit <- gt %>%
  mutate(bhmg=bhmg,tg=temp) %>%
  group_by(n) %>%
  do(a = lm(bhmg~I(tg^2)+I(tg),data=.)[[1]][[1]],
     b = unlist(lm(bhmg~I(tg^2)+I(tg),data=.)[[1]][[3]]),
     c = as.numeric(lm(bhmg~I(tg^2)+I(tg),data=.)[[1]][[2]]) ) 

fit$a <- unlist(fit$a)
fit$b <- unlist(fit$b)
fit$c <- unlist(fit$c)

temps <- clim %>% 
  filter(V1 %in% c("alpha_temp","base_temp")) %>%
  pivot_wider(names_from=V1) %>%
  mutate(dtemp_opt=alpha_temp - 12.5) %>%
  mutate(dtemp_opt_rescaled=dtemp_opt*3/(max(dtemp_opt)-min(dtemp_opt))  ) %>%
  pivot_longer(c(alpha_temp,base_temp,dtemp_opt,dtemp_opt_rescaled)) %>%
  inner_join(fit) %>%
  mutate(g = a + b*value + c*value^2,dg=b+2*c*value) 

ggplot(inner_join(reg,temps %>% filter(name %in% c("dtemp_opt_rescaled"))) %>%
         filter(iso3!='ATA')) +
  geom_polygon(aes(x = long, y = lat,group = group, fill = value),color='black',size=.1)+
  theme_void()+
  theme(legend.position = "bottom", strip.text.x = element_text(size=12, face="bold"),legend.title = element_blank(),plot.title = element_text(hjust = 0.5)) +
  scale_fill_gradient2()

new <- pivot_wider(temps %>% select(-dg,-g)) %>%
  inner_join(temps %>% filter(name=="base_temp") %>% select(n,dg)) %>% 
  group_by(n) %>%
  summarise( a = solve_linear(base_temp,dtemp_opt_rescaled,dg)[[1]],
           b = solve_linear(base_temp,dtemp_opt_rescaled,dg)[[2]],
           c = solve_linear(base_temp,dtemp_opt_rescaled,dg)[[3]]) %>%
  pivot_longer(c(a,b,c),names_to="Coefficient")

newplot <- new %>%
  pivot_wider(names_from="Coefficient") %>%
  cross_join(data.frame(T=-10:35) ) %>%
  mutate(dg=a+b*T+c*T^2)
      
oldplot <-  fit %>%
  cross_join(data.frame(T=-10:10) ) %>%
  mutate(dgold=a+b*T+c*T^2)

ggplot(newplot %>% 
         inner_join(pivot_wider(temps %>% select(-a,-b,-c,-dg,-g))) %>% 
         filter(n %in% c("usa","fra","ind","rus","idn") )) +
  geom_smooth(aes(x=T-base_temp,y=dg,color=n)) +
  geom_point(aes(x=T-base_temp,y=dg,color=n)) +
  geom_smooth(data=oldplot %>% 
                inner_join(pivot_wider(temps %>% select(-a,-b,-c,-dg,-g))) %>% 
                filter(n %in% c("usa","fra","ind","rus","idn")),
  aes(x=T-base_temp,y=dgold,color=n), 
  linetype=2 ) +
  ylim(c(-0.2,0.1)) +
  xlim(c(-5,5)) +
  facet_wrap(n~.,)

ggplot(gt %>% 
         filter(n %in% c("usa","can","ind","gbr","chn","nor") & t<18)) +
  geom_point(aes(x=temp,y=bhmg,color=n)) + 
  geom_smooth(aes(x=temp,y=bhmg,group=n),fullrange=TRUE) +
  geom_hline(yintercept=0,color="grey") 

ggplot(fit %>% 
         inner_join(inner_join(gdp,pop) %>% filter(t==2) %>% mutate(gdpc=gdp/pop*1e6)) %>%
         mutate(rich = ifelse(gdpc<10000, "no","yes"))) +
  geom_point(aes(x=temp,y=dg,color=gdpc)) +
  facet_wrap(rich~.,) +
  scale_color_viridis_c()
 

compare_temps <- climate_regional_data["temp_region_valid_cmip5"] %>%
  rename(temp=value,rcp=V3) %>% mutate(source="cmip5") %>%
  bind_rows(climate_regional_data["temp_region_valid_pop_cmip6"] %>%
              rename(temp=value,rcp=V3) %>% mutate(source="cmip6_pop") ) %>%
  bind_rows(climate_regional_data["temp_region_valid_area_cmip6"] %>%
              rename(temp=value,rcp=V3) %>% mutate(source="cmip6_area") )

ggplot(compare_temps  %>%
         filter(n %in% c("usa","fra","ind","rus","nga","chn","ita","can","fin") & ttoyear(t)<2100 ) %>%
         mutate(rcp=str_remove(rcp,"ssp[1-9]"))) +
  geom_line(aes(x=ttoyear(t),y=temp,color=rcp,linetype=source),linewidth=1.2) +
  facet_wrap(n~.,scales="free") + guides()

