check_burkemod <- gdx('C:/Users/pietr/OneDrive - Politecnico di Milano/RICE50/RICE50x/3dgnew.gdx')
new <- check_burkemod["coeff_burke"] 
old <- check_burkemod["coeff_burke_original"] 
rescale <- check_burkemod["modulate_damages"] 
temps <- check_burkemod["climate_region_coef"]

newplot <- new %>% pivot_wider(names_from=V2) %>%
  full_join(rescale %>% rename(rescale=value)) %>%
  mutate_if(is.numeric,coalesce,1) %>%
  cross_join(data.frame(T=-10:35) ) %>%
  mutate(dg=(a+b*T+c*T^2) )

oldplot <- old %>% pivot_wider(names_from=V2) %>%
  mutate_if(is.numeric,coalesce,1) %>%
  cross_join(data.frame(T=-10:35) ) %>%
  mutate(dg=(a+b*T+c*T^2) )

ggplot(newplot %>% 
         inner_join(pivot_wider(temps,names_from=V1)) %>% 
         filter(n %in% c("tur","bra","ind","gbr","usa","ita","rus")) ) +
geom_line(data=newplot %>% 
           inner_join(pivot_wider(temps,names_from=V1)) %>% 
           filter(n %in% c("usa","bra","ind","rus","nga","chn","ita","can","gbr")),
          aes(x=T-(alpha_temp+beta_temp*tatm0),y=dg,color=n)) +
  geom_line(aes(x=T-(alpha_temp+beta_temp*tatm0),y=dg,color=n)) +
  geom_line(data=oldplot %>% 
                inner_join(pivot_wider(temps,names_from=V1)) %>% 
                filter(n %in% c("tur","bra","ind","gbr","usa","ita","rus")),
              aes(x=T-(alpha_temp+beta_temp*tatm0),y=dg,color=n), 
              linetype=2 ) +
  geom_vline(xintercept=0) +
  geom_hline(yintercept=0) +
  ylim(c(-0.1,0.05)) +
  xlim(c(-5,5)) +
  facet_wrap(n~.,) + theme(legend.position = "none") + 
  xlab("Difference with 2015 regional temperature") + 
  ylab("Difference in income growth per capita")

