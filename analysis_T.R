temp <- read.csv("monthly-temperature-anomalies.csv") %>%
  mutate(year=str_extract(Day,"1[0-9][0-9][0-9]-*"),
        month=str_extract(Day,"-[0-9][0-9]*"),
        month=str_remove(month,"-"),
        year=str_remove(year,"-")) %>%
  group_by(year,Code) %>%
  summarise(yearan=mean(Temperature.anomaly)) %>%
  group_by(Code) %>%
  summarise(sd=sd(yearan)) %>%
  mutate(sd3=3*sd)
