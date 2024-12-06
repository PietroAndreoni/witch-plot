#run first plotgdx_rice_main_scenarios with 
# witch_folder = "../Results_srm/All161024/Sovereignity" 
main_scenarios_bra <- sanitized_names %>% 
  filter(tend==2200 & nsrm!="no SRM" & (nsrm=="Brazil" | COOP=="coop") ) %>%
  mutate(ptype=ifelse(ptype=="kotzorg","Original","Symmetric")) 

brazil_palette <- c("free.coop"="#121B54",
                    "free.noncoop"="#FF7F00",
                    "sovereign.noncoop"="#FFD726")

snoncoop <- Z_SRM %>% 
  inner_join(main_scenarios_bra %>% filter(COOP=="noncoop")) %>%
  filter(ttoyear(t)<=2100  & !is.na(value) & !inj %in% c("60N","60S") ) %>%
  ggplot() +
  geom_area(aes(x=ttoyear(t),
                y=value,
                fill=ordered(inj,c("45S","30S","15S","0","15N","30N","45N"))),
            linewidth=1,
            color="black") +
  xlab("") + ylab("SAI [TgS/yr]") + 
  facet_grid(.~zinj) +
  scale_fill_manual(name="Injection latitude",
                    values=c("darkblue","#4a8dff","#CDDDFF","grey","#ffbaba","#ff5252","#a70000"))
ggsave("SI_figures/sov_strategynoncoop.png",plot=snoncoop,width=15, height=9, units="cm")

regtemp2100 <- TEMP %>% 
  rename(temp=value) %>%
  inner_join(coef %>% filter(V1=="alpha_temp") %>% rename(preind=value)) %>%
  group_by(file,n) %>%
  mutate(temp=temp-preind) %>%
  inner_join(main_scenarios_bra) %>%
  filter(ttoyear(t)==2100) %>% 
  inner_join(countries_map) %>% 
  inner_join(optimal_temperature) %>%
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  ggplot() +
  geom_point(aes(x=meanlat,
                 y=temp,
                 color=interaction(zinj,COOP)),
             alpha=0.2) +
  stat_smooth(aes(x=meanlat,
                  y=temp,
                  color=interaction(zinj,COOP),
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  stat_smooth(data=.%>%filter(nsrm!="no SRM"),
              aes(x=meanlat,
                  y=opttemp-preind,
                  weight=pop),
              linetype=2,
              se = FALSE,
              linewidth=2,color="black") +
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  scale_color_manual(values=brazil_palette,
                     name="Injection latitude",
                     labels=c("Cooperative","Free","Sovereign")) +
  xlab("") + ylab("Local temperature increase to preindustrial [°C]") + 
  theme(legend.position = "bottom")

precip2100 <- PREC %>% rename(prec=value) %>% 
  inner_join(main_scenarios_bra) %>%
  filter(ttoyear(t)==2100) %>%   
  inner_join(countries_map) %>% 
  inner_join(pop %>% 
               filter(ttoyear(t)==2100) %>% 
               group_by(n,file) %>%
               summarise(pop=mean(value)) ) %>%
  inner_join(sd_prec) %>%
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
                 color=interaction(zinj,COOP)),
             alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=(prec-1)/sd,
                  color=interaction(zinj,COOP),
                  weight=pop), 
              se = FALSE,
              linewidth=2 ) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  scale_color_manual(values=brazil_palette,
                     name="Injection latitude",
                     labels=c("Cooperative","Free","Sovereign")) +
  xlab("") + ylab("Precipitation variation [STD]") + 
  theme(legend.position = "bottom")

damages2100 <- gdploss %>%  
  filter(ttoyear(t)==2100 ) %>% 
  inner_join(pop %>% rename(pop=value) ) %>%
  ungroup() %>% select(-ptype) %>%
  inner_join(main_scenarios_bra) %>%
  inner_join(countries_map) %>%
  ggplot() + 
  geom_hline(yintercept=0) +
  geom_vline(data=data.frame(lats=c(-45,-30,-15,0,15,30,45,60)),
             aes(xintercept=lats),
             linetype=2,
             color="grey",
             alpha=0.5) +
  geom_point( aes(x=meanlat,
                  y=value*100,
                  color=interaction(zinj,COOP)),
              alpha=0.2) + 
  stat_smooth(aes(x=meanlat,
                  y=value*100,
                  color=interaction(zinj,COOP),
                  weight=pop), 
              se = FALSE,
              linewidth=2) +
  scale_color_manual(values=brazil_palette,
                     name="Injection latitude",
                     labels=c("Cooperative","Free","Sovereign")) +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("GDP loss [%]") + theme(legend.position = "bottom")

void <- ggplot() + theme_void() + theme(panel.background = element_rect(fill="white",color="white"))
fig_noncoops <- ggarrange(ggarrange(regtemp2100,precip2100,nrow=1,common.legend=TRUE),
                        ggarrange(void,damages2100+theme(legend.position = "none"),void,nrow=1,widths=c(0.3,1,0.3)),
                        nrow=2,heights=c(1,1))
ggsave("SI_figures/sov_noncoop.png",plot=fig_noncoops,width=18, height=16, units="cm")
