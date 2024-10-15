srm_regional_data["coefficients_kotz"] %>%
  rename(coef=V2) %>%
  inner_join(base_prec) %>% 
  inner_join(sd_prec) %>%
  cross_join(data.frame(prec=seq(0.6,+1,by=0.05))) %>%
  group_by(n,prec) %>%
  summarise(value1=value[coef=="a"]*((prec-prec0)/(sd*prec0))^2+value[coef=="b"]*((prec-prec0)/(sd*prec0)), 
            value=value[coef=="a"]/sd^2-value[coef=="b"]/sd
             + (value[coef=="b"]/(sd*prec0)-2*prec0*value[coef=="a"]/(sd*prec0)^2)*(prec)
             + (value[coef=="a"]/(sd*prec0)^2)*(prec)^2,
             prec0=mean(prec0),
             sd=mean(prec0*(1+3*sd)) ) %>%
  filter(n=="rus") %>%
  ggplot() +
    geom_line(aes(x=prec,y=value1,color=n)) +
  geom_line(aes(x=prec,y=value,color=n)) +
   geom_hline(yintercept = 0) +
  # geom_hline(yintercept = 1) +
   geom_vline(aes(xintercept=prec0,color=n)) +
   geom_vline(aes(xintercept=sd,color=n)) +
  theme(legend.position="none")
  
srm_regional_data["coefficients_kotz"] %>%
  rename(coef=V2) %>%
  inner_join(base_prec) %>% 
  inner_join(sd_prec) %>%
  group_by(n) %>%
  summarise(a=value[coef=="a"]/sd^2-value[coef=="b"]/sd,
            b=(value[coef=="b"]/(sd*prec0)-2*prec0*value[coef=="a"]/(sd*prec0)^2),
            c=(value[coef=="a"]/(sd*prec0)^2),
            prec0=mean(prec0),
            sd=mean(prec0*(1+3*sd)) ) %>% unique()

base_prec %>% 
  inner_join(sd_prec) %>%
  cross_join(data.frame(prec=seq(-3,+3,by=0.1))) %>%
  group_by(n,prec) %>%
  summarise( value = -0.00251*(prec0+prec*sd)^2+0.01573*(prec0+prec*sd)-(-0.00251*prec0^2+0.01573*prec0) ) %>%
  ggplot() +
  geom_line(aes(x=prec,y=value,color=n)) +
  theme(legend.position="none")


base_prec %>% 
  inner_join(sd_prec) %>%
  cross_join(data.frame(prec=seq(-3,+3,by=0.1))) %>%
  group_by(n,prec) %>%
  summarise( value = -0.0168*(prec0+prec*sd)^2+0.0469*(prec0+prec*sd)-(-0.0168*prec0^2+0.0469*prec0) ) %>%
  ggplot() +
  geom_line(aes(x=prec,y=value,color=n)) +
  theme(legend.position="none")

