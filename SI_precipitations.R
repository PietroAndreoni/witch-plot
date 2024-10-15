#### cooperative scenarios
main_scenarios_coop <- sanitized_names %>% 
  filter(COOP=="coop" & spread==1 & tend==2200 & ptype=="modified" & ttype=="modified" & nsrm!="no SRM") 

coop_palette <- c("Optimal, no SAI"="#00A36C",
                  "0.2"="#e8f4f8",
                  "0.5"="#add8e6",
                  "1"="#72bcd4",
                  "2"="#0000FF",
                  "5"="#121B54")

regtemp2100 <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(file,n) %>%
  mutate(temp=temp-preind) %>%
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)==2100) %>% 
  inner_join(countries_map) %>% 
  inner_join(optimal_temperature) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  mutate(Scenario=ifelse(nsrm=="no SRM","Optimal, no SAI",pimp)) %>%
  ggplot() +
  geom_point(aes(x=meanlat,
                 y=temp,
                 color=Scenario),
             alpha=0.2) + 
  geom_point(data=.%>%filter(nsrm!="no SRM"),
             aes(x=meanlat,
                 y=opttemp-preind),
             color="black",
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=temp,
                  color=Scenario,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  stat_smooth(data=.%>%filter(nsrm!="no SRM"),
              aes(x=meanlat,
                  y=opttemp-preind,
                  weight=pop),
              color="black",
              se = FALSE,
              linewidth=2,
              linetype=2) +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=coop_palette,
                     name="Precipitation impacts",
                     labels=c("5"="High impacts",
                              "1"="Low impacts")) +
  xlab("") + ylab("Local temperature increase to preindustrial [°C]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=12))

precip2100 <- PREC %>% rename(prec=value) %>% 
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)==2100) %>%   
  inner_join(countries_map) %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sd_prec) %>%
  mutate(Scenario=ifelse(nsrm=="no SRM","Optimal, no SAI",pimp)) %>%
  ggplot() +
  geom_hline(yintercept=0) +
  geom_ribbon(data=data.frame(lats=c(-50,75)),
              aes(x=lats,
                  ymin=-1,
                  ymax=1),
              color="grey",
              linewidth=1,
              alpha=0.2) +
  geom_point(aes(x=meanlat,
                 y=(prec-1)/sd,
                 color=Scenario),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=(prec-1)/sd,
                  color=Scenario,
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
                     name="Precipitation impacts",
                     labels=c("5"="High impacts",
                              "1"="Low impacts")) +
  xlab("") + ylab("Precipitation variation [STD]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=12))

damages2100 <- gdploss %>%  
  filter(ttoyear(t)==2100 ) %>% 
  inner_join(pop %>% rename(pop=value) ) %>%
  inner_join(main_scenarios_coop) %>%
  inner_join(countries_map) %>%
  mutate(Scenario=ifelse(nsrm=="no SRM","Optimal, no SAI",pimp)) %>%
  ggplot() + 
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point( aes(x=meanlat,
                  y=value*100,
                  color=Scenario),
              alpha=0.2) + 
  stat_smooth(data=.%>% filter(meanlat<=60),
              aes(x=meanlat,
                  y=value*100,
                  color=Scenario,
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  theme(legend.position="bottom") + 
  theme_pubr() + 
  scale_color_manual(values=coop_palette,
                     name="Scenario",
                     labels=c("5"="High impacts",
                              "1"="Low impacts")) +
  scale_fill_manual(values=coop_palette,
                    name="Scenario",
                    labels=c("5"="High impacts",
                             "1"="Low impacts")) +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("GDP loss [%]") + theme(legend.position = "right",
                               text=element_text(size=12))

void <- ggplot() + theme_void() + theme(panel.background = element_rect(fill="white",color="white"))
fig2_coops <- ggarrange(ggarrange(regtemp2100,precip2100,nrow=1),
                        ggarrange(void,damages2100,void,nrow=1,widths=c(0.4,1,0.1)),
                        nrow=2,heights=c(1,1))
ggsave("figSI_coop.png",plot=fig2_coops,width=18, height=16, units="cm")

names <- c("5"="High impacts",
           "1"="Low impacts")

scoop <- Z_SRM %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)<=2100 & nsrm %in% c("Cooperative") & !is.na(value) & !inj %in% c("60N","60S")) %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),
                y=value,
                fill=ordered(inj,c("45S","30S","15S","0","15N","30N","45N"))),
            linewidth=1,
            color="black") +
  xlab("") + ylab("SAI [TgS/yr]") + 
  theme_pubr() + 
  facet_wrap(names[pimp]~.,nrow=1) +
  scale_fill_manual(name="Injection latitude",
                    values=c("darkblue","#4a8dff","#CDDDFF","grey","#ffbaba","#ff5252","#a70000"))
ggsave("figSI_Scoop.png",plot=scoop,width=18, height=9, units="cm")


#### non cooperative scenarios
temperature_maps <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(sanitized_names) %>%
  inner_join(optimal_temperature) %>%
  group_by(n) %>%
  mutate(temp=temp-opttemp) %>%
  filter(ttoyear(t)==2100) %>% 
  inner_join(countries_map) %>% 
  ungroup() %>% 
  mutate(disc=arules::discretize(temp,method="fixed",
                                 breaks=c(-5,-2,-1,-0.2,0.2,1,2,5),
                                 labels=c("Extreme overcooling","Significant overcooling","Moderate overcooling","Optimal","Moderate undercooling","Significant undercooling","Extreme undercooling"))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),color='black',size=.1) +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = disc),color='red',size=.4) +
  scale_fill_manual(values=c("Extreme overcooling"="#4575B4","Significant overcooling"="#74ADD1","Moderate overcooling"="lightblue","Optimal"="white","Moderate undercooling"="#FDAE61","Significant undercooling"="#D73027","Extreme undercooling"="#800000"),name="Temperature") +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_grid(names[pimp]~ordered(Scenario,c("2°C","Cooperative","Brazil","India","China","USA","Free-riding")) )

ggsave("figSI_temp.png",plot=temperature_maps,width=18, height=10, units="cm")



precipitation_maps <- PREC %>% rename(prec=value) %>% 
  inner_join(sanitized_names) %>%
  filter(ttoyear(t)==2100 ) %>%
  inner_join(countries_map) %>% 
  inner_join(sd_prec) %>%
  ungroup() %>% 
  mutate(disc=arules::discretize((prec-1)/sd,method="fixed",
                                 breaks=c(-5,-2,-1,-0.2,0.2,1,2,5),
                                 labels=c("Extreme decrease","Significant decrease","Moderate decrease","No variation","Moderate increase","Significant increase","Extreme increase"))) %>%
  left_join(reg %>% filter(iso3!='ATA')) %>% 
  ggplot() +
  geom_polygon(aes(x = long, y = lat,group = group, fill = disc),size=.1,color="black") +
  geom_polygon(data= . %>% filter(n==n_to_name[nsrm]), 
               aes(x = long, y = lat,group = group, fill = disc),color='red',size=.4) +
  #  geom_polygon(aes(x = long, y = lat,group = group, color = latitude),size=.1, fill=NA) +
  scale_fill_manual(values=c("#800000","#D73027","#FDAE61","white","lightblue","#74ADD1","#4575B4"),
                    name="Precipitation") +
  scale_color_viridis_d() +
  theme_void()+ 
  theme(panel.background = element_rect(fill="white",color="white"),legend.position = "top") +
  facet_grid(names[pimp]~ordered(Scenario,c("2°C","Cooperative","Brazil","India","China","USA","Free-riding")))

ggsave("figSI_prec.png",plot=precipitation_maps,width=18, height=10, units="cm")


Snoncoop <- W_SRM %>%
  inner_join(sanitized_names) %>%
  mutate(Scenario=case_when(nsrm=="no SRM" & COOP=="coop" ~ "Optimal, no SAI",
                            nsrm=="Cooperative" & COOP=="coop" ~ "Optimal",
                            nsrm=="no SRM" & COOP=="noncoop" ~ "Free-riding",
                            .default=nsrm) ) %>%
  filter(ttoyear(t)<=2100 & pimp %in% c(1,5) & nsrm != "no SRM") %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),
                y=value,
                color=Scenario),
            linewidth=1) +
  ggrepel::geom_text_repel(data=.%>% inner_join(Z_SRM %>% 
                                                  inner_join(sanitized_names) %>%
                                                  filter(ttoyear(t)==2100 & 
                                                           pimp %in% c(1,5) & 
                                                           !nsrm %in% c("no SRM","Cooperative") & value!=0) %>%
                                                  rename(zonalinj=value) ),
                           aes(x=2100,
                               y= value,
                               label=paste0(round(zonalinj,0),"tgS/yr at ",inj), 
                               color=nsrm ) ) +
  scale_color_manual(values=regpalette_srm, name="Scenario") +
  guides(text="none") +
  facet_wrap(names[pimp]~.,) +
  xlab("") + ylab("SAI [TgS/yr]") + theme_pubr()
ggsave("figSI_Snoncoop.png",plot=Snoncoop,width=18, height=9, units="cm")
