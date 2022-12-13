shapleyref_sensbd <- data.frame(t=numeric(),sigma=numeric(),name=character(),BD=character())

ssp <- 2
### extra dataframes
for (bd in c("epc","histresp","geo")) {

shapleyref_sensbd <- ALL_FLOWS %>% select(-yab,-ynet) %>%
    filter(ssp==ssp & ttoyear(t)>=2020  & ttoyear(t)<=2100 & ( (BD==bd & O=="yes" & B=="700") | (O=="no" & B=="ref") )  ) %>%
    inner_join(ykali_dist) %>% inner_join(Y_DIST) %>%
    mutate(c1=-abcost,c2=dacrev-gentax-dacost,c3=transfer-ctx,err=ygross-ykali+value-y) %>%
    select_at((c("t","n","dist","B","c1","c2","c3","err","ykali"))) %>%
    pivot_longer(c(c1,c2,c3,err)) %>%
    rename(state=B) %>% mutate(state=ifelse(state=="700","s1","s2")) %>%
    pivot_wider(names_from="state",values_from="value") %>%
    group_by(t) %>%
    do(make_shapley(.,global=TRUE)) %>%
    mutate(BD=as.character(bd)) %>%
    rbind(shapleyref)
} 
