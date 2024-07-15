main_scenarios_coop <- sanitized_names %>% 
  filter(COOP=="coop" & spread==1 & tend==2200 & ptype=="modified" & ttype=="modified" & pimp %in% c(1,5))

coop_palette <- c("Optimal, no SAI"="#00A36C",
                  "0.2"="#e8f4f8",
                  "0.5"="#add8e6",
                  "1"="#72bcd4",
                  "2"="#0000FF",
                  "5"="#121B54")

f1 <- gdploss %>%  
  filter(ttoyear(t)==2100 ) %>% 
  inner_join(pop %>% rename(pop=value) ) %>%
  inner_join(main_scenarios_coop) %>%
  inner_join(countries_map) %>%
  mutate(Scenario=ifelse(nsrm=="no SRM","Optimal, no SAI",pimp)) %>%
  filter(Scenario=="Optimal, no SAI") %>%
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
  stat_smooth(data=.%>%filter(meanlat<60),
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
                     labels=c("2°C")) +
  scale_fill_manual(values=coop_palette,
                    name="Scenario",
                    labels=c("SAI","2°C")) +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("GDP loss [%]") + theme(legend.position = "right",
                               text=element_text(size=7))
ggsave("fig11.png",f1,height=4,width=5)

f2 <- gdploss %>%  
  filter(ttoyear(t)==2100 ) %>% 
  inner_join(pop %>% rename(pop=value) ) %>%
  inner_join(main_scenarios_coop) %>%
  inner_join(countries_map) %>%
  mutate(Scenario=ifelse(nsrm=="no SRM","Optimal, no SAI",pimp)) %>%
  filter(Scenario!="5") %>%
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
  stat_smooth(data=.%>%filter(meanlat<60),
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
                     labels=c("SAI, low prec","2°C")) +
  scale_fill_manual(values=coop_palette,
                    name="Scenario",
                    labels=c("SAI","2°C")) +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("GDP loss [%]") + theme(legend.position = "right",
                               text=element_text(size=7))
ggsave("fig12.png",f2,height=4,width=5)


f3 <- gdploss %>%  
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
  stat_smooth(data=.%>%filter(meanlat<60),
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
                     labels=c("SAI, low prec","SAI, high prec","2°C")) +
  scale_fill_manual(values=coop_palette,
                    name="Scenario",
                    labels=c("SAI","2°C")) +
  guides(shape="none") +
  xlab("Average country latitude") + 
  ylab("GDP loss [%]") + theme(legend.position = "right",
                               text=element_text(size=7))
ggsave("fig13.png",f3,height=4,width=5)
