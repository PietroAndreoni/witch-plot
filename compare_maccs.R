require("witchtools")
pkgs<-c("tidyverse","data.table","ggplot2","gdxtools")
lapply(pkgs,require_package)
rm(list = ls())

ls <- list.files(path='.',pattern="\\.gdx")

#load data
old_fgases <- gdx("data_mod_fgases.gdx")
old_fgases_base <- data.table(extract(old_fgases,"ssp_fgases_baseline"))
old_fgases_base[V2=="hfc43-10",V2:="hfc4310"]
setnames(old_fgases_base,"value","old_value")
setnames(old_fgases_base,"V2","e")
old_fgases_base <- old_fgases_base[V1=="ssp2",]
old_fgases_base[,V1:=NULL]

old_ch4n20 <- gdx("data_landuse.gdx")
old_ch4_base <- data.table(extract(old_ch4n20,"ch4_agr_baseline"))
old_n2o_base <- data.table(extract(old_ch4n20,"n2o_agr_baseline"))
setnames(old_ch4_base,"value","old_value")
setnames(old_n2o_base,"value","old_value")

new_base <- gdx("data_mod_mac_harmsen.gdx")
fgases_base <- data.table(extract(new_base,"fgases_baseline"))
ch4n20_base <- data.table(extract(new_base,"ghg_baseline"))
setnames(fgases_base,"V1","e")
setnames(ch4n20_base,"V1","e")

# validation 

valid <-  gdx("data_validation.gdx")
valid_ghg <- data.table(extract(valid,"q_emi_valid_primap"))
setnames(valid_ghg,"V1","e")
setnames(valid_ghg,"value","valid")

#compare f-gases
fgases <- merge(old_fgases_base,fgases_base,by=c("e","t","n"))

ggplot(fgases) +
  geom_line(aes(x=as.numeric(t),y=value,color=n),linetype=1) +
  geom_line(aes(x=as.numeric(t),y=old_value,color=n),linetype=2) +
  facet_wrap(e~.,scales="free")

# compare agr ch4 and n20
ch4 <- merge(ch4n20_base,old_ch4_base[,e:="ch4_agr"],by=c("e","t","n"))
n20 <- merge(ch4n20_base,old_n2o_base[,e:="n2o_agr"],by=c("e","t","n"))

ch4 <- melt(ch4,id.vars=c("t","n","e"),measure.vars=c("value","old_value"))
ch4[variable=="value",value:=value/25*1000]

ggplot(ch4) +
  geom_line(aes(x=as.numeric(t),y=value,color=n,linetype=variable)) +
  facet_wrap(variable~.,scales="free")

n20 <- melt(n20,id.vars=c("t","n","e"),measure.vars=c("value","old_value"))
n20[variable=="value",value:=value/285]

ggplot(n20) +
  geom_line(aes(x=as.numeric(t),y=value,color=n,linetype=variable)) +
  facet_wrap(variable~.,scales="free")

# compare with validation
ch4n20_base[str_detect(e,"n2o"),.(value=sum(value),e="n2o"),by=c("t","n")]
ch4n20_base[str_detect(e,"ch4"),.(value=sum(value),e="ch4"),by=c("t","n")]
valid_ghg[str_detect(e,"n2o"),.(valid=sum(valid),e="n2o"),by=c("t","n")]
valid_ghg[str_detect(e,"ch4"),.(valid=sum(valid),e="ch4"),by=c("t","n")]

check_valid <- merge(ch4n20_base[as.numeric(t)<4,],valid_ghg,by=c("t","n","e"),all=TRUE)
check_valid <- merge(fgases_base[as.numeric(t)<4,],valid_ghg,by=c("t","n","e"),all=TRUE)
check_valid <- melt(check_valid,id.vars=c("t","n","e"),measure.vars=c("value","valid"))

check_valid[variable=="value" & str_detect(e,"n2o"),value:=value/285]
check_valid[variable=="value" & str_detect(e,"ch4"),value:=value/25]
check_valid <- rbind(check_valid,check_valid[,.(value=sum(value),n="World" ),by=.(t,e,variable)])

ggplot(check_valid[n=="World"]) +
  geom_line(aes(x=as.numeric(t),y=value,color=n,linetype=variable)) +
  facet_wrap(e~.,scales="free")
