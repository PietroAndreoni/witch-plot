main_scenarios_coop <- sanitized_names %>% 
  filter(COOP=="coop" & spread==1 & tend==2200 & ptype=="modified" & ttype=="modified" & pimp %in% c(1,5))

coop_palette <- c("Optimal, no SAI"="#00A36C",
                  "0.2"="#e8f4f8",
                  "0.5"="#add8e6",
                  "1"="#72bcd4",
                  "2"="#0000FF",
                  "5"="#121B54")

srm2100 <- Z_SRM %>%
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)==2100 & 
           !pimp %in% c("no SRM") & 
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
  inner_join(main_scenarios_coop) %>%
  filter(ttoyear(t)==2100 ) %>% 
  inner_join(countries_map) %>% 
  inner_join(optimal_temperature) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  mutate(Scenario=ifelse(nsrm=="no SRM","Optimal, no SAI",pimp)) %>%
  ggplot() +
  # geom_bar(data=Z_SRM %>%
  #            inner_join(main_scenarios_coop) %>%
  #            filter(ttoyear(t)==2100 & 
  #                     !pimp %in% c("no SRM") & 
  #                     !inj %in% c("60N","60S")) %>% 
  #            mutate(inj=ifelse(str_detect(inj,"S"), -as.numeric(str_remove(inj,"S")),as.numeric(str_remove(inj,"N")))),
  #          aes(x=inj,
  #              y=value/sum(value)*2),
  #          position="dodge",stat="identity",color="black",fill="#121B54",alpha=0.5,width=5) +
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
  geom_hline(yintercept=2,color="grey",linewidth=1) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  theme(legend.position="bottom") +
  scale_color_manual(values=coop_palette,
                     name="Precipitation impacts",
                     labels=c("SAI","2°C")) +
  xlab("") + ylab("Local temperature increase to preindustrial [°C]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=7))

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
  # geom_bar(data=Z_SRM %>%
  #            inner_join(main_scenarios_coop) %>%
  #            filter(ttoyear(t)==2100 & 
  #                     !pimp %in% c("no SRM") & 
  #                     !inj %in% c("60N","60S")) %>% 
  #            mutate(inj=ifelse(str_detect(inj,"S"), -as.numeric(str_remove(inj,"S")),as.numeric(str_remove(inj,"N")))),
  #          aes(x=inj,
  #              y=value/sum(value)),
  #          position="dodge",stat="identity",color="black",fill="#121B54",alpha=0.5,width=5) +
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
                     labels=c("SAI","2°C")) +
  xlab("") + ylab("Precipitation variation [STD]") + 
  theme_pubr() + theme(legend.position = "none",
                       text=element_text(size=7))

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
                     labels=c("SAI, low precipitation impacts","SAI, high precipitation impacts","2°C")) +
  scale_fill_manual(values=coop_palette,
                    name="Scenario",
                    labels=c("SAI","2°C")) +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("GDP loss [%]") + theme(legend.position = "right",
                                     text=element_text(size=7))

void <- ggplot() + theme_void() + theme(panel.background = element_rect(fill="white",color="white"))
fig2_coops <- ggarrange(ggarrange(regtemp2100,precip2100,nrow=1),
                        ggarrange(void,damages2100,void,nrow=1,widths=c(0.4,1,0.1)),
                        nrow=2,heights=c(1,1))
ggsave("fig_coop.png",plot=fig2_coops,width=18, height=16, units="cm")
