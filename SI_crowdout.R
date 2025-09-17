gwp <- c("ch4"=25*1e-3,"n2o"=298*1e-3,"co2"=1)
files <- list.files("../Results_newdata/Persistency",pattern=".gdx")
tghg_pers <- batch_extract(c("TATM_GHG"),paste0("../Results_newdata/Persistency/",files ) )$TATM_GHG %>%
  mutate(file=str_remove_all(gdx,"../Results_newdata/Persistency/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble()

tsai_pers <- batch_extract(c("TATM_SAI"),paste0("../Results_newdata/Persistency/",files ) )$TATM_SAI %>%
  mutate(file=str_remove_all(gdx,"../Results_newdata/Persistency/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble()

wemi_pers <- batch_extract(c("W_EMI"),paste0("../Results_newdata/Persistency/",files ) )$W_EMI %>%
  mutate(file=str_remove_all(gdx,"../Results_newdata/Persistency/|.gdx"),
         t=as.numeric(t)) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble()

cbudget <- batch_extract(c("cbudget_2020_2100"),paste0("../Results_newdata/Persistency/",files ) )$cbudget_2020_2100 %>%
  mutate(file=str_remove_all(gdx,"../Results_newdata/Persistency/|.gdx")) %>%
  filter(!str_detect(file,"debug")) %>% as_tibble()

sanitized_names_pers <- tatm_pers %>% select(file) %>% unique() %>% sanitize()

tatm <- ggplot(tghg_pers %>% 
         inner_join(sanitized_names_pers) %>% 
         filter(ttoyear(t)==2100) %>% 
         mutate(pers_p=paste0("x",as.character( as.numeric(pers_p)/as.numeric(pers_t)) ) ) ) +
  geom_segment(data=. %>% filter(pers_t=="inf" & COOP=="coop" & nsrm=="no SRM"),
             aes(x=40,xend=45,y=value ),color="#00A36C",linewidth=1,linetype=2) +
  geom_segment(data=. %>% filter(pers_t=="inf" & COOP=="coop" & nsrm=="Cooperative"),
             aes(x=40,xend=45,y=value), color="#121B54",linewidth=1,linetype=2) +
  geom_segment(data=. %>% filter(pers_t=="inf" & COOP=="noncoop" & nsrm=="no SRM"),
             aes(x=40,xend=45,y=value), color="black",linewidth=1,linetype=2) +
  geom_segment(data=. %>% filter(pers_t!="inf" & COOP=="noncoop" & nsrm=="no SRM") %>%
                 group_by(pers_t) %>% 
                 summarise(value=mean(value)),
             aes(x=as.numeric(pers_t)-2,xend=as.numeric(pers_t)+2, y=value ), color="black",linewidth=1) +
  geom_line(data=. %>% filter(pers_t!="inf" & COOP=="coop" & nsrm=="Cooperative"),
            aes(x=as.numeric(pers_t),y=value,color=pers_p,group=pers_p),linewidth=1) +
  geom_point(data=. %>% filter(pers_t!="inf" & COOP=="coop" & nsrm=="Cooperative"),
             aes(x=as.numeric(pers_t),y=value,color=pers_p,group=pers_p),size=3) +
  scale_color_manual(name="P impacts persistency [x T]",values=c("x1"="#121B54",
                                                                 "x0"="#780000",
                                                                 "x0.5"="#C98C7E",
                                                                 "x2"="lightblue")) +
  ylab("GHG driven global warming  in 2100 [°C]") + xlab("T impacts persistency [years]") + ylim(c(1,3))


tsai <- ggplot(tsai_pers %>% 
                 inner_join(sanitized_names_pers) %>% 
                 filter(ttoyear(t)==2100) %>% 
                 mutate(pers_p=paste0("x",as.character( as.numeric(pers_p)/as.numeric(pers_t)) ) ) ) +
  geom_segment(data=. %>% filter(pers_t=="inf" & COOP=="coop" & nsrm=="no SRM"),
               aes(x=40,xend=45,y=value ),color="#00A36C",linewidth=1,linetype=2) +
  geom_segment(data=. %>% filter(pers_t=="inf" & COOP=="coop" & nsrm=="Cooperative"),
               aes(x=40,xend=45,y=value), color="#121B54",linewidth=1,linetype=2) +
  geom_segment(data=. %>% filter(pers_t=="inf" & COOP=="noncoop" & nsrm=="no SRM"),
               aes(x=40,xend=45,y=value), color="black",linewidth=1,linetype=2) +
  geom_segment(data=. %>% filter(pers_t!="inf" & COOP=="noncoop" & nsrm=="no SRM") %>%
                 group_by(pers_t) %>% 
                 summarise(value=mean(value)),
               aes(x=as.numeric(pers_t)-2,xend=as.numeric(pers_t)+2, y=value ), color="black",linewidth=1) +
  geom_line(data=. %>% filter(pers_t!="inf" & COOP=="coop" & nsrm=="Cooperative"),
            aes(x=as.numeric(pers_t),y=value,color=pers_p,group=pers_p),linewidth=1) +
  geom_point(data=. %>% filter(pers_t!="inf" & COOP=="coop" & nsrm=="Cooperative"),
             aes(x=as.numeric(pers_t),y=value,color=pers_p,group=pers_p),size=3) +
  scale_color_manual(name="P impacts persistency [x T]",values=c("x1"="#121B54",
                                                                 "x0"="#780000",
                                                                 "x0.5"="#C98C7E",
                                                                 "x2"="lightblue")) + ylab("SAI driven global cooling in 2100 [°C]") + 
  xlab("T impacts persistency [years]") + ylim(c(1,3))



cb <- ggplot(cbudget %>% 
         inner_join(sanitized_names_pers) %>% 
         mutate(pers_p=paste0("x",as.character( as.numeric(pers_p)/as.numeric(pers_t)) ) ) ) +
  geom_segment(data=. %>% filter(pers_t=="inf" & COOP=="coop" & nsrm=="no SRM"),
               aes(x=40,xend=45,y=value ),color="#00A36C",linewidth=1,linetype=2) +
  geom_segment(data=. %>% filter(pers_t=="inf" & COOP=="coop" & nsrm=="Cooperative"),
               aes(x=40,xend=45,y=value), color="#121B54",linewidth=1,linetype=2) +
  geom_segment(data=. %>% filter(pers_t=="inf" & COOP=="noncoop" & nsrm=="no SRM"),
               aes(x=40,xend=45,y=value), color="black",linewidth=1,linetype=2) +
  geom_segment(data=. %>% filter(pers_t!="inf" & COOP=="noncoop" & nsrm=="no SRM") %>%
                 group_by(pers_t) %>% 
                 summarise(value=mean(value)),
               aes(x=as.numeric(pers_t)-2,xend=as.numeric(pers_t)+2, y=value ), color="black",linewidth=1) +
  geom_line(data=. %>% filter(pers_t!="inf" & COOP=="coop" & nsrm=="Cooperative"),
            aes(x=as.numeric(pers_t),y=value,color=pers_p,group=pers_p),linewidth=1) +
  geom_point(data=. %>% filter(pers_t!="inf" & COOP=="coop" & nsrm=="Cooperative"),
             aes(x=as.numeric(pers_t),y=value,color=pers_p,group=pers_p),size=3) +
  scale_color_viridis_dw(name="P impacts persistency [x T]") + ylab("Carbon budget [GtCO2]") + xlab("T impacts persistency [years]") 

ggarrange(tatm,tsai,common.legend = TRUE)
