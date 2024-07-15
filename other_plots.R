brackets %>%
  filter(type!="ab" & !is.na(bracket)) %>%
  inner_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = bracket ),color='black',size=.1) +
  scale_fill_brewer(palette="RdBu",direction=-1) + 
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white")) +
  facet_grid(nsrm~type,)

gdploss %>%
  filter(!is.na(bracket) & pimp==1 & ttoyear(t)==2100) %>%
  inner_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = bracket),color='black',size=.1) +
  scale_fill_manual(values=rev(c("#8B0000","#B2182B", "#D6604D", "#F4A582", "#FDDBC7", "#FFFFFF", "#92C5DE", "#4393C3", "#2166AC"))) + 
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white")) +
  facet_wrap(nsrm~.,)

damfrac_type %>%
  filter(ttoyear(t)==2100) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  pivot_longer(c(ab,temp,prec)) %>%
  inner_join(sanitized_names) %>%
  group_by(n,name,pimp) %>%
  mutate(value=value-value[nsrm=="Cooperative" & COOP=="coop"]) %>%
  group_by(file,latitude,name) %>%
  summarise(med=modi::weighted.quantile(value,pop,0.5),
            confm=modi::weighted.quantile(value,pop,0.33),
            confp=modi::weighted.quantile(value,pop,0.66),
            min=min(value),
            max=max(value)) %>%
  inner_join(sanitized_names) %>%
  filter(COOP=="noncoop" & pimp %in% c(0.2,1,5) ) %>%
  ggplot() +
  geom_bar(aes(x=name,y=med,fill=pimp),
           color="black",
           stat="identity",
           position="dodge") +
  geom_errorbar(aes(x=name,
                    ymin=min,
                    ymax=max,
                    group=pimp),
                width=0,
                position=position_dodge(width=0.9),
                linetype=2) +
  geom_errorbar(aes(x=name,
                    ymin=confm,
                    ymax=confp,
                    group=pimp),
                width=0.5,
                position=position_dodge(width=0.9),
                linewidth=0.7) +
  facet_grid(nsrm~latitude,) +
  scale_fill_manual(values=coop_palette)


### main figure: underprovision of SAI vs overprovision
fig3a <-NPVgdploss %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)<=2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sanitized_names) %>%
  filter(nsrm %in% c("no SRM","Cooperative","India","Brazil","China","Australia") ) %>%
  inner_join(countries_map) %>%
  group_by(n) %>%
  mutate(damrel = value - value[COOP=="coop" & nsrm=="Cooperative"]) %>%
  filter(pimp %in% c("1") & nsrm!="Cooperative") %>%
  ggplot() + 
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point(aes(x=meanlat,
                 y=damrel*100,
                 color=nsrm),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=damrel*100,
                  color=nsrm,
                  weight=pop),
              se=FALSE,
              linewidth=2) +
  theme(legend.position="bottom") + 
  theme_pubr() + 
  scale_color_manual(values=regpalette_srm,
                     name="Free driver") +
  scale_fill_manual(values=regpalette_srm,
                    name="Free driver") +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("Damages [% of GDP]") + 
  theme(legend.position = "right",
        text=element_text(size=7))# +
facet_wrap(pimp~.,) 

damfrac_type %>%
  filter(ttoyear(t)==2100) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  pivot_longer(c(ab,temp,prec)) %>%
  inner_join(sanitized_names) %>%
  group_by(n,name,pimp) %>%
  mutate(value=value-value[nsrm=="Cooperative" & COOP=="coop"]) %>%
  inner_join(sanitized_names) %>%
  filter( (COOP=="noncoop" | (COOP=="coop" & nsrm=="no SRM") )  & pimp %in% c("0.2","1","5") & name!="ab") %>%
  ggplot() +
  geom_hline(yintercept=0) +
  stat_boxplot(aes(x=latitude,
                   y=value,
                   fill=ordered(nsrm,c("USA","China","India","Brazil","Australia","no SRM")),
                   weight=pop),
               alpha=0.5,
               position=position_dodge()) +
  facet_grid(name~pimp,) +
  scale_color_manual(values=regpalette_srm,
                     name="Free driver") +
  scale_fill_manual(values=regpalette_srm,
                    name="Free driver")

NPVgdploss %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)<=2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sanitized_names) %>%
  filter(nsrm %in% c("USA","India","Brazil","China","Australia") ) %>%
  inner_join(countries_map) %>%
  group_by(n,nsrm) %>%
  mutate(damrel = value - value[pimp==1]) %>%
  filter(COOP=="noncoop" & pimp %in% c(0.2,0.5,2,5) ) %>%
  ggplot() + 
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point(aes(x=meanlat,
                 y=damrel*100,
                 color=nsrm),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=damrel*100,
                  color=nsrm,
                  weight=pop),
              se=FALSE,
              linewidth=2) +
  theme(legend.position="bottom") + 
  theme_pubr() + 
  scale_color_manual(values=regpalette_srm,
                     name="Free driver") +
  scale_fill_manual(values=regpalette_srm,
                    name="Free driver") +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("Damages [% of GDP]") +
  theme(legend.position = "right",
        text=element_text(size=7)) +
  facet_wrap(pimp~.,)

W_SRM %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & nsrm %in% c("Cooperative","USA","India","Brazil","China") & pimp==5) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=nsrm,
                linetype=pimp),
            linewidth=1) +
  scale_color_manual(values=regpalette_srm)


Z_SRM %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & nsrm %in% c("USA","India","Brazil","China","Australia") ) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=nsrm,
                linetype=pimp),
            linewidth=1) +
  scale_color_manual(values=regpalette_srm) +
  facet_wrap(inj~.,)


land_temp %>%
  filter(ttoyear(t)<=2200) %>%
  inner_join(sanitized_names) %>%
  ggplot(aes(x=ttoyear(t),y=value-land_temp0,color=nsrm,linetype=pimp,group=file)) +
  geom_line() +
  scale_color_manual(values=regpalette_srm)


optimal_temperature %>% 
  inner_join(coef %>%
               filter(V1=="alpha_temp") %>%
               select(-V1)) %>%
  inner_join(sanitized_names) %>%
  inner_join(countries_map) %>%
  ggplot() +
  geom_vline(aes(xintercept=opttemp-value,color=latitude)) +
  facet_wrap(spread~.,)




fig3a <- gdploss_g %>%
  filter( nsrm %in% c("no SRM","Cooperative","China","USA","India","Brazil") ) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm) ) %>%
  ggplot() +
  geom_ribbon(data=. %>%
                group_by(Scenario,t) %>%
                summarise(min=min(value*100),max=max(value*100)),
              aes(x=ttoyear(t),
                  ymin=min,
                  ymax=max,
                  fill=ordered(Scenario,c("Free-riding","Optimal, no SAI","Optimal","Brazil","India","China","USA")),
                  color=ordered(Scenario,c("Free-riding","Optimal, no SAI","Optimal","Brazil","India","China","USA"))),
              alpha=0.3)  +
  geom_line(data=.%>%
              filter(pimp==1),
            aes(x=ttoyear(t),
                y=value*100,
                color=ordered(Scenario,c("Free-riding","Optimal, no SAI","Optimal","Brazil","India","China","USA"))),
            linewidth=1) +
  scale_color_manual(values=regpalette_scenarios,name="Scenario") +
  scale_fill_manual(values=regpalette_scenarios,name="Scenario") +
  xlab("") + 
  ylab("Total GDP loss(+)/gain(-) [% of GDP]") 


fig3b <- gdploss %>% 
  filter(ttoyear(t)==2100) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(countries_map) %>%
  inner_join(sanitized_names) %>%
  filter(pimp %in% c("5") & 
           nsrm %in% c("no SRM","China","USA","India","Brazil") ) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  ggplot() + 
  stat_boxplot(aes(x=ordered(pimp,c(" ","0.2","1","5")),
                   y=value*100,
                   fill=ordered(Scenario,c("Free-riding","Optimal, no SAI","Optimal","Brazil","India","China","USA")),
                   weight=pop), 
               outlier.size = 0.1) + 
  scale_color_manual(values=regpalette_scenarios,
                     name="Scenario") +
  scale_fill_manual(values=regpalette_scenarios,
                    name="Scenario") +
  guides(shape="none") +
  xlab("Precipitation") + 
  ylab("Total GDP loss(+)/gain(-) \n in 2100 [% of GDP]") + 
  theme(legend.position = "right",
        text=element_text(size=7)) +
  facet_wrap(.~latitude,nrow=2)

void <- ggplot() + theme_void() + theme(panel.background = element_rect(fill="white",color="white"))
fig3 <- ggarrange(ggarrange(void,fig3a,void,widths=c(0.3,1,0.3),nrow=1),fig3b+theme(legend.position="none"), nrow=2, heights=c(0.4,0.6))


gdploss %>% 
  filter(ttoyear(t)==2100) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(countries_map) %>%
  inner_join(sanitized_names) %>%
  filter(pimp %in% c("0.2","1","5") & 
           nsrm %in% c("China","USA","Brazil","India") ) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  ggplot() + 
  stat_boxplot(aes(x=ordered(pimp,c(" ","0.2","1","5")),
                   y=valuerel,
                   fill=ordered(Scenario,c("Free-riding","Optimal, no SAI","Optimal","Brazil","India","China","USA")),
                   weight=pop), 
               outlier.size = 0.1) + 
  scale_color_manual(values=regpalette_scenarios,
                     name="Scenario") +
  scale_fill_manual(values=regpalette_scenarios,
                    name="Scenario") +
  guides(shape="none") +
  xlab("Precipitation") + 
  ylab("Total GDP loss(+)/gain(-) \n in 2100 [% of GDP]") + 
  theme(legend.position = "right",
        text=element_text(size=7)) +
  facet_wrap(.~latitude,nrow=2)


gdploss %>% 
  filter(ttoyear(t)==2100) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(countries_map) %>%
  inner_join(sanitized_names) %>%
  filter(pimp %in% c("5") & 
           nsrm %in% c("no SRM","Cooperative","China","USA","Brazil","India") ) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm),
         pimp=ifelse(nsrm=="no SRM"," ",pimp)  ) %>%
  filter(!Scenario %in% c("Optimal, no SAI","Optimal") & pimp==5) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize(-valuerel,breaks=6)) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),color='black',size=.1) +
  scale_fill_viridis_d() +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white")) +
  facet_wrap(Scenario~.,)

