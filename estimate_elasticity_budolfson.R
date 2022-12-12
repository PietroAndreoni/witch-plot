#get elasticity of omega from
###############################################################################
#obtain elasticity of carbon tax costs from Budolfson 2021 meta-analysis
#local for now, integrate to DVC
ela_data <- fread("./external_data/budolfson_elasticity_study_data_2021.csv")

#tidy the dataframe 
require(tidyverse)
datacols <- c("CountryA3", "studyyear", "Type", "Exp.Inc","Study", "Region", "Country",
              "pcGDP2014db", "pcGDP", "distyear")
cost_shares <- ela_data %>% select(datacols,"costS1","costS2","costS3","costS4","costS5") %>%
  pivot_longer(cols=!datacols,names_to="quant",values_to="cost") %>% mutate(quant = str_remove(quant,"costS"))
exp_shares <-  ela_data %>% select(datacols,"e1","e2","e3","e4","e5") %>%
  pivot_longer(cols=!datacols,names_to="quant",values_to="e") %>% mutate(quant = str_remove(quant,"e"))

regr <- full_join(cost_shares,exp_shares) %>% 
  group_by_at(vars(-cost,-e,-quant)) %>% 
  do(int = lm(log(cost)~log(e),data=.)[["coefficients"]][["(Intercept)"]],slope = lm(log(cost)~log(e),data=.)[["coefficients"]][["log(e)"]])

ggplot(regr%>% mutate(slope=unlist(slope))) +
  geom_point(aes(x=pcGDP,y=slope,color=Country ) ) 

meta_regression <- lm(slope~log(pcGDP) ,data=regr %>% mutate(slope=unlist(slope)))

################ plot omega in time, by country
ykali <- as_tibble(batch_extract("ssp_ykali","../RICE50x/data_ed57/data_baseline.gdx")[[1]])
pop <- as_tibble(batch_extract("ssp_l","../RICE50x/data_ed57/data_baseline.gdx")[[1]])

om <- full_join(ykali %>% rename(gdp=value),pop %>% rename(pop=value)) %>% rename(ssp=V1) %>%
  mutate(om = 3.3219 - 0.2334 * log(1e6 * gdp/pop))

regs <- c("usa","nde","chn","swe","uk","ita","fra","rsaf","can","bra","rus")
ggplot(om %>% filter(ttoyear(t) <= 2100 & n %in% regs)) +
  geom_line(aes(x=ttoyear(t),y=om,color=n)) +
  facet_wrap(ssp~.) +
  scale_color_manual(values=region_palette) +
  theme_pubr()
