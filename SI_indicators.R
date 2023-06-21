##### test different inequality indexes:
index9010 <- Y_DIST %>%
  filter(file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest" & dist %in% c("D1","D10") ) %>%
  inner_join(ykali_dist) %>%
  group_by_at(c("t","n",file_group_columns)) %>%
  summarise(i = value[dist=="D10"]/value[dist=="D1"], i0 = ykali[dist=="D10"]/ykali[dist=="D1"]) %>% 
  ungroup() %>% mutate(irel = (i - i0) )
  
atk <- Y_DIST %>%
  filter(file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest" ) %>%
  inner_join(ykali_dist) %>%
  group_by_at(c("t","n",file_group_columns)) %>%
  summarise(atk = ineq::ineq(value,parameter=2,type="Atkinson"), atk0 = ineq::ineq(ykali,parameter=2,type="Atkinson") ) %>% 
  ungroup() %>% mutate(atkrel = (atk - atk0) )

theil <- Y_DIST %>%
  filter(file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest" ) %>%
  inner_join(ykali_dist) %>%
  group_by_at(c("t","n",file_group_columns)) %>%
  summarise(theil = dineq::theil.wtd(value), theil0 = dineq::theil.wtd(ykali) ) %>% 
  ungroup() %>% mutate(theilrel = (theil - theil0) )

mld <- Y_DIST %>%
  filter(file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest" ) %>%
  inner_join(ykali_dist) %>%
  group_by_at(c("t","n",file_group_columns)) %>%
  summarise(mld = dineq::mld.wtd(value), mld0 = dineq::mld.wtd(ykali) ) %>% 
  ungroup() %>% mutate(mldrel = (mld - mld0) )

##### compare indicators for Fig 3a
compare <- ginit %>%
  filter(file=="ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest" ) %>%
  inner_join(mld) %>% 
  inner_join(theil) %>% 
  inner_join(atk) %>% 
  inner_join(index9010) %>% 
  inner_join(share) 

indnames = c("atkrel"="Atkinson","ginirel"="Gini","irel"="10/10","theilrel"="Theil","mldrel"="MLD")
compare %>% 
  pivot_longer(c(ginirel,theilrel,mldrel,irel,atkrel)) %>%
  mutate(name=indnames[name]) %>% 
  group_by(t,breaksname,file,name) %>%
  summarise(med=median(value),min=quantile(value,probs=c(0.33)),max=quantile(value,probs=c(0.66))) %>%
  ggplot() +
  geom_line(aes(x=ttoyear(t),y=med,color=breaksname),size=1.5) +
  geom_ribbon(aes(x=ttoyear(t),ymin=min,ymax=max,fill=breaksname),size=1,alpha=0.2) +
  facet_wrap(name~.,scales="free") +
  labs(color="Carbon removed \n[% abated + removed]",fill="Carbon removed \n[% abated + removed]") +
  theme_pubr() + xlab("") + ylab("Change in inequality index")
ggsave("SIIND_fig1.png",width=12,height=10,dpi=320)

###### 
theilw <- Y_DIST %>%
  filter(file %in% c("ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest",
                     "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                     "ssp2_B700_DISTepc_COSThigh_TAXbest_NEGbest",
                     "ssp2_B700_DISTgeo_COSThigh_TAXbest_NEGbest") & 
           ttoyear(t) %in% c(2075,2100) ) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop) %>%
  group_by_at(c("t",file_group_columns)) %>%
  summarise(theil = dineq::theil.wtd(value,weights=pop/10), theil0 = dineq::theil.wtd(ykali,weights=pop/10) ) %>% 
  ungroup() %>% mutate(theilrel = (theil - theil0) )

mldw <- Y_DIST %>%
  filter(file %in% c("ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest",
                     "ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest",
                     "ssp2_B700_DISTepc_COSThigh_TAXbest_NEGbest",
                     "ssp2_B700_DISTgeo_COSThigh_TAXbest_NEGbest") & 
           ttoyear(t) %in% c(2075,2100) ) %>%
  inner_join(ykali_dist) %>%
  inner_join(pop) %>%
  group_by_at(c("t",file_group_columns)) %>%
  summarise(mld = dineq::mld.wtd(value,weights=pop/10), mld0 = dineq::mld.wtd(ykali,weights=pop/10) ) %>% 
  ungroup() %>% mutate(mldrel = (mld - mld0) )

### 
ynofin <- YGROSS_DIST %>%
  inner_join(COST_CDR) %>%
  inner_join(ABATECOST) %>%
  inner_join(Y_DIST %>% rename(gdp=value)) %>%
  inner_join(ineq_weights %>% filter(ineq_elast=="abatement") %>% rename(ctax=value) %>% select(-ineq_elast) ) %>%
  inner_join(quantiles_ref %>% rename(qref=value) ) %>%
  group_by(t,file) %>%
  mutate(ycosts = ygrossd - ctax*(abcost+cdrcost) ) %>% 
  select(t,n,file,dist,ycosts)

theilnofin <- ynofin %>% 
  inner_join(ykali_dist) %>%
  inner_join(pop) %>%
  group_by_at(c("t",file_group_columns)) %>%
  summarise(theil = dineq::theil.wtd(ycosts,weights=pop/10), theil0 = dineq::theil.wtd(ykali,weights=pop/10) ) %>% 
  ungroup() %>% mutate(theilrel = (theil - theil0) )

mldnofin <- ynofin %>% 
  inner_join(ykali_dist) %>%
  inner_join(pop) %>%
  group_by_at(c("t",file_group_columns)) %>%
  summarise(mld = dineq::mld.wtd(ycosts,weights=pop/10), mld0 = dineq::mld.wtd(ykali,weights=pop/10) ) %>% 
  ungroup() %>% mutate(mldrel = (mld - mld0) )

gininofin <- ynofin %>% 
  inner_join(ykali_dist) %>%
  inner_join(pop) %>%
  group_by_at(c("t",file_group_columns)) %>%
  summarise(gini = dineq::gini.wtd(ycosts,weights=pop/10), gini0 = dineq::gini.wtd(ykali,weights=pop/10) ) %>% 
  ungroup() %>% mutate(ginirel = (gini - gini0) )

##### compare indicators for Fig 3a
compare <- ginit %>% 
  filter(n=="World") %>%
  inner_join(mldw) %>% 
  inner_join(theilw) 

filetoscen <- c("ssp2_B700_DISTgeo_COSTbest_TAXbest_NEGbest" = "Low costs, north",
                "ssp2_B700_DISTepc_COSThigh_TAXbest_NEGbest" = "High costs, south",
                "ssp2_B700_DISTgeo_COSThigh_TAXbest_NEGbest" = "High costs, north",
                "ssp2_B700_DISTepc_COSTbest_TAXbest_NEGbest" = "Low costs, south")

compare %>% 
  pivot_longer(c(ginirel,theilrel,mldrel)) %>%
  inner_join(gininofin %>% 
  inner_join(mldnofin) %>% 
  inner_join(theilnofin) %>% 
  pivot_longer(c(ginirel,theilrel,mldrel),values_to="value2") %>% select(t,file,name,value2)) %>%
  rowwise() %>% mutate(name=indnames[name],file=filetoscen[as.character(file)]) %>% #mutate(file=ifelse(is.na(file),"High costs, north",file)) %>%
  ggplot() +
  geom_bar(aes(x=file,y=(value-value2)*100,fill=name),color="black",position="dodge",stat="identity") +
  labs(color="",fill="") +
  facet_grid(ttoyear(t)~.,) +
  theme_pubr() + xlab("") + ylab("Change in inequality index")
ggsave("SIIND_fig12.png",width=12,height=8,dpi=320)



