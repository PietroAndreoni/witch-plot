#Specific functions for the RICE50+ model


plot_macc_fit <- function(yearcheck = 2040){
#check MACC curves
get_witch("ax_co2")
get_witch("bx_co2")
get_witch("emi_bau_co2")
get_witch("mx")

check_macc <- ax_co2 %>% select(t,n,value) %>% rename(ax_co2=value) %>% full_join(bx_co2 %>% select(t,n,value) %>% rename(bx_co2=value))  %>% full_join(emi_bau_co2 %>% select(t,n,value) %>% rename(emi_bau_co2=value))  %>% full_join(mx %>% select(t,n,value) %>% rename(mx=value)) 

plot_check_macc <- check_macc %>% filter(ttoyear(t) == yearcheck)
xmin<-0
xmax<-1.2
step<-0.05
xx<-seq(xmin,xmax,by=step)
miudf<-data.frame(matrix("", ncol = length(plot_check_macc$n), nrow = length(xx)))
names(miudf) <- plot_check_macc$n
miudf$xx<-xx
for(i in 1:length(plot_check_macc$n)){
  miudf[,i] <- plot_check_macc$ax_co2[i]*(miudf$xx^2)/2 + plot_check_macc$bx_co2[i]*(miudf$xx^5)/5
  miudf[,i] <- plot_check_macc$mx[i] * (plot_check_macc$ax_co2[i]*(miudf$xx^2)/2 + plot_check_macc$bx_co2[i]*(miudf$xx^5)/5)
  miudf[,i] <- plot_check_macc$mx[i] * (plot_check_macc$ax_co2[i]*(miudf$xx^2)/2 + plot_check_macc$bx_co2[i]*(miudf$xx^5)/5) * plot_check_macc$emi_bau_co2[i]
  miudf[,i] <- plot_check_macc$mx[i]*(plot_check_macc$ax_co2[i]*(miudf$xx^1) + plot_check_macc$bx_co2[i]*(miudf$xx^4))
  miudf[,i] <- (plot_check_macc$ax_co2[i]*(miudf$xx^1) + plot_check_macc$bx_co2[i]*(miudf$xx^4))
}
ggplot(miudf %>% pivot_longer(cols = !xx, names_to = "n"), aes(xx,value)) + geom_line(aes(colour = n))
plotly::ggplotly()
#add enerdata points
enerdata <- fread(file = file.path(witch_folder, "input", "data", "enerdata-enerdata_macc_full.csv"))
enerdata <- enerdata %>% filter(sector=="Total_CO2" & scenario=="Ener-Blue") %>% mutate(t=yeartot(Year), mju=abatement_perc, n=Code) %>% select(t,n,cost,mju)
ggplot(enerdata %>% filter(ttoyear(t)==yearcheck), aes(mju,cost)) + geom_point(aes(colour = n))

#plot enerdata and model together
ggplot(miudf %>% pivot_longer(cols = !xx, names_to = "n"), aes(xx,value))  + xlim(0,0.6) + ylim(0,1000) + geom_line(aes(colour = n)) + geom_point(data=enerdata %>% filter(ttoyear(t)==yearcheck), aes(mju,cost, colour = n)
) + xlab("mju") + ylab("Carbon price $/tCO2") + guides(color=FALSE)
saveplot("RICE50+ MACC Curves Fit")
}

#compare two scenarios, for RICE. Needs two scenarios and a bau
identikit_scenario_compare <- function(file_bau=c("ssp2_bau_noncoop"),files,labels=c("scen1","scen2")) {
  
  #load variables
  get_witch("Y")
  get_witch("pop")
  get_witch("EIND")
  get_witch("ELAND")
  get_witch("E_NEG")
  get_witch("Q_TRD")
  get_witch("COST_CDR")
  get_witch("ABATECOST")
  get_witch("ABATEDEMI")
  get_witch("MRC")
  get_witch("MAC")
  get_witch("ykali")
  get_witch("YGROSS")
  get_witch("DI_FUND")
  get_witch("I_FUND")
  get_witch("FUND")
  get_witch("C")
  get_witch("K")
  get_witch("I")
  
  scenarios <- paste0("scen",seq(1,length(files)))
  file_to_scenarios <- setNames(scenarios,files)
  labels <- setNames(labels,scenarios)
  
  palette_n <- c("r5asia"="#66c2a5", "r5maf"="#fc8d62", "r5lam"="#e78ac3","r5oecd"="#8da0cb", "r5ref"="#a6d854")
  names_regions <- c("r5asia"="ASIA","r5oecd"="OECD","r5lam"="LATIN AMERICA","r5maf"="AFRICA AND MENA","r5ref"="EX-USSR")
  palette_regions <- palette_n[match(names(names_regions), names(palette_n))]
  names(palette_regions) <- names_regions
  
  prices <- COST_CDR %>% rename(c_cdr = value) %>%
    filter(file %in% files) %>% 
    inner_join(ABATECOST %>% rename(c_ab = value)) %>% 
    full_join(MRC %>% rename(mrc = value)) %>% 
    inner_join(MAC %>% rename(mac = value)) %>%
    inner_join(ABATEDEMI %>% rename(e_ab = value)) %>%
    inner_join(E_NEG %>% rename(e_cdr = value) ) %>%
    mutate(cavg_ab = c_ab/e_ab*1e3, cavg_cdr = c_cdr/e_cdr*1e3) %>%
    select(-c_cdr,-c_ab,-e_cdr,-e_ab) %>%
    pivot_longer(c(mrc,mac,cavg_ab,cavg_cdr)) %>% complete()
  
  prices <- prices %>%
    filter(file %in% files) %>% 
    inner_join(Y %>% rename(gdp=value)) %>%
    group_by(t,file,name,pathdir) %>% 
    summarise(value=weighted.mean(value,gdp),n="World") %>%
    bind_rows(prices)
  prices$scen <- revalue(prices$file,file_to_scenarios)
  prices$label <- revalue(prices$scen,labels)
  
  e  <- EIND %>% rename(eind=value) %>% mutate(eind=eind*3.66) %>%
    filter(file %in% files) %>% 
    inner_join(ELAND %>% rename(afolu=value) %>% mutate(afolu=afolu*3.66)) %>% 
    inner_join(E_NEG %>% mutate(cdr=-value) %>% select(-value)) %>%
    pivot_longer(c(eind,afolu,cdr)) %>%
    mutate(value=ifelse(is.na(value),0,value)) %>%
    bind_rows(make_global_tr(data=.,group_cols=c("t","file","name")) %>% mutate(n="World")) %>% ungroup() 
  e$name <- revalue(e$name,c("afolu"="LAND USE CHANGE","cdr"="CDR","eind"="INDUSTRIAL EMISSIONS"))
  e$scen <- revalue(e$file,file_to_scenarios)
  e$label <- revalue(e$scen,labels)
  
  budget  <- e %>%
    make_cumulative(group_cols=c("n","file","name","scen","label")) %>% mutate(t="2020to2100*") %>%
    bind_rows(e %>%
                group_by(t,n,file) %>%
                filter(sum(value)>=0 & ttoyear(t)<=2100) %>%
                group_by(n,file,scen,label) %>%
                do(make_cumulative(data=.,group_cols=c("name"),yearend=max((ttoyear(.$t))) ) ) )
  
  costs <- COST_CDR %>% rename(c_cdr = value) %>%
    filter(file %in% files) %>% 
    inner_join(ABATECOST %>% rename(c_ab = value)) %>%
    inner_join(YGROSS %>% rename(yg=value)) %>%
    inner_join(Y %>% rename(y=value)) %>%
    inner_join(YGROSS %>% filter(file==file_bau) %>% rename(y0=value) %>% select(-file)) %>%
    mutate(c_g = y0-y - (c_cdr+c_ab) ) %>%
    select(-yg,-y) %>%
    pivot_longer(c(y0,c_cdr,c_ab,c_g)) %>%
    bind_rows(make_global_tr(data=.,group_cols=c("t","file","name")) ) %>%
    group_by(t,file) %>%
    mutate(valuerel = value/value[name=="y0" & n=="World"]) %>%
    filter(name!="y0") 
  costs$scen <- revalue(costs$file,file_to_scenarios)
  costs$name <- revalue(costs$name,c("c_cdr"="CDR COSTS","c_ab"="ABATEMENT COSTS","c_g"="GROWTH EFFECTS"))
  costs$label <- revalue(costs$scen,labels)
  
  closs <- C %>% 
    filter(file %in% files) %>%
    inner_join(C %>% filter(file==file_bau) %>% rename(c0=value) %>% select(-file)) %>%
    bind_rows(make_global_tr(data=.,group_cols=c("t","file"),sum_cols=c("value","c0") )) %>%
    mutate(valuerel = (c0-value)/c0) 
  closs$scen <- revalue(closs$file,file_to_scenarios)
  closs$label <- revalue(closs$scen,labels)
  
  econ <- FUND %>% rename(f=value) %>%
    filter(file %in% files) %>%
    inner_join(K %>% rename(k=value)) %>%
    inner_join(I_FUND %>% rename(i_f=value)) %>%
    inner_join(I %>% rename(i=value)) %>%
    inner_join(DI_FUND %>% rename(di_f=value)) %>%
    inner_join(YGROSS %>% filter(file==file_bau) %>% rename(y0=value) %>% select(-file)) %>%
    pivot_longer(c(y0,f,k,i,di_f,i_f)) %>%
    bind_rows(make_global_tr(data=.,group_cols=c("t","file","name")) ) %>%
    mutate(scen = recode(file, !!!file_to_scenarios)) %>%
    group_by(t,n) %>%
    mutate(valuerel = (value-value[scen=="scen1"])/value[name=="y0" & scen=="scen1"]) %>%
    filter(name!="y0") 
  econ$scen <- revalue(econ$file,file_to_scenarios)
  econ$label <- revalue(econ$scen,labels)
  
  group_cols <- c("t","n","file")
  effort <- ABATEDEMI %>% rename(abate=value) %>% 
    filter(file %in% files) %>%
    inner_join(E_NEG %>% rename(cdr=value))  %>%
    mutate(scen = recode(file, !!!file_to_scenarios)) %>%
    pivot_longer(c(abate,cdr)) %>%
    mutate(value=ifelse(is.na(value),0,value)) %>%
    group_by_at(setdiff(group_cols,c("file","scen") ) ) %>%
    mutate(valuerel=value-value[scen=="scen1"])
  effort$name <- revalue(effort$name,c("abate"="ABATED EMISSIONS","cdr"="CARBON REMOVED"))
  effort$label <- revalue(effort$scen,labels)
  
  map <- make_map_df("r5")
  map$Region <- plyr::revalue(map$n,names_regions)
  
  gini <- Y %>% rename(y=value) %>%
    filter(file %in% files) %>%
    inner_join(pop %>% rename(pop=value)) %>%
    mutate(scen = recode(file, !!!file_to_scenarios)) %>%
    group_by(t,file,scen) %>%
    summarise(gini = reldist::gini(y/pop,pop) ) %>% 
    group_by(t) %>%
    mutate(ginirel=gini-gini[scen=="scen1"]) %>%
    filter(scen=="scen2")
  
  #### plots
  emis <- ggplot(e %>% filter(n=="World" & ttoyear(t)<=2100 & ttoyear(t)>=2020),
                 aes(x=ttoyear(t),y=value)) +
    geom_area(aes(fill=name),color="black") + 
    geom_line(data=.%>% 
                group_by(t,n,file,scen) %>% 
                summarise(value=sum(value)) %>%
                mutate(scen=case_when(scen=="scen1"~"scen2",scen=="scen2"~"scen1")) %>% 
                mutate(label=recode(scen,!!!labels)),
              linewidth=1.2,linetype=2) + 
    geom_line(data=.%>% 
                group_by(t,n,file,scen,label) %>% 
                summarise(value=sum(value)),
              linewidth=1.2) + 
    geom_bar(data=budget %>% 
               filter(n=="World" & t=="2020to2100*") %>% unique(),
             aes(y=value/30,fill=name,x=2105),width=2,stat="identity",color="black",alpha=0.9) +
    geom_bar(data=budget %>% 
               filter(n=="World"& t!="2020to2100*" & str_detect(t,"2020")),
             aes(x=as.numeric(stringr::str_remove(t,"2020to")),y=value/30,fill=name),width=2,stat="identity",color="black",alpha=0.8,linetype=2) +
    geom_label(data=budget %>% 
                 filter(n=="World" & t=="2020to2100*") %>% unique(),
               aes(x=2105, y = value/30/2 + 1, label = round(value), fill=name ),color="white") +
    geom_label(data=budget %>% 
                 filter(n=="World" & t!="2020to2100*" & str_detect(t,"2020")),
               aes(x=as.numeric(stringr::str_remove(t,"2020to")), y = value/30/2 + 1, label = round(value), fill=name ),color="white") +
    geom_label(data=budget %>% 
                 filter(n=="World" & t=="2020to2100*") %>% unique() %>%
                 group_by(t,n,file,scen,label) %>% 
                 summarise(value=sum(value)),
               aes(x=2105, y = value/30/2 + 1, label = round(value) )) +
    geom_label(data=budget %>% 
                 filter(n=="World" & t!="2020to2100*" & str_detect(t,"2020")) %>%
                 group_by(t,n,file,scen,label) %>% 
                 summarise(value=sum(value)),
               aes(x=as.numeric(stringr::str_remove(t,"2020to")), y = value/30/2 + 1, label = round(value) )) +
    theme_pubr() + xlab('') + ylab('Emissions [GtCO2/yr]') + 
    scale_fill_manual(name="",values=c("CDR"="#00aad0","INDUSTRIAL EMISSIONS"="#c47900","LAND USE CHANGE"="#004f00")) +
    facet_grid(~label,)
  
  budget_scen <- ggplot(budget %>% filter(file %in% files & n!="World" & t=="2020to2100*") %>%
                          group_by(file,scen,n) %>%
                          summarise(value=sum(value)) %>%
                          group_by(file,scen) %>%
                          mutate(tot=sum(value)) %>% ungroup() %>% mutate(value=value/tot) ) +
    geom_bar(aes(x=tot/2, y=value, fill=n, width =tot),stat="identity", color="black") +
    geom_text(aes(x=tot/2, y=value, fill=n,label=paste0(round(value*100),"%")),color="black", position = position_stack(vjust = 0.5)) +
    geom_label(data=.%>%filter(n=="r5oecd"),aes(x=0,y=0,label = round(tot)) ) +
    coord_polar("y") + theme_void() + theme(legend.position="none") +
    scale_fill_manual(values=palette_n) +
    facet_grid(.~file,) + 
    theme(strip.text = element_blank(),
          plot.background = element_rect(fill = "white", colour = NA), 
          panel.background = element_rect(fill = "white", colour = NA),
          legend.background = element_rect(fill = "white", colour = NA))
  
  diff <- ggplot(effort %>% filter(scen=="scen2") %>% 
                   make_cumulative(group_cols=c("n","file","scen","name"),cols_sum = c("valuerel"),exclude_cols=c("value")) %>%
                   filter(n!="World")) +
    geom_hline(yintercept=0) +
    geom_bar(aes(x=name,y=valuerel,fill=n),position="stack",stat="identity",color="black") + 
    scale_fill_manual(values=palette_n) +
    theme_pubr() + ylab("Variation of [GtCO2]") + xlab("") +
    theme(legend.position="none",axis.ticks.x = element_blank(),  axis.line.x = element_blank())
  
  mapplot <- ggplot(map %>% filter(iso3!="ATA")) +  
    geom_polygon(aes(x=long,y=lat,group=group,fill=Region),color="black") + 
    theme_void() + theme(legend.position="top", legend.title = element_blank(),
                         plot.background = element_rect(fill = "white", colour = NA), 
                         panel.background = element_rect(fill = "white", colour = NA),
                         legend.background = element_rect(fill = "white", colour = NA)) +
    guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_fill_manual(values=palette_regions)
  
  f1 <- ggarrange(emis,ggarrange(mapplot,budget_scen,diff,ncol=1,heights=c(0.4,0.3,0.4),labels=c("b1","b2","b3")),widths=c(0.65,0.35),labels=c("a","")) + bgcolor("white")      
  
  ###### costs
  costw <- ggplot(costs %>% filter(n=="World" & ttoyear(t)<=2100 & ttoyear(t)>=2020) %>% mutate(name=as.factor(name))) +
    geom_area(aes(x=ttoyear(t),y=valuerel*100,fill=name),color="black") +
    geom_line(data=.%>% 
                group_by(t,n,file,scen) %>% 
                summarise(value=sum(valuerel)) %>%
                mutate(scen=case_when(scen=="scen1"~"scen2",scen=="scen2"~"scen1")) %>%
                mutate(label=recode(scen,!!!labels)),
              aes(x=ttoyear(t),y=value*100),linewidth=1.2,linetype=2) + 
    geom_line(data=.%>% 
                group_by(t,n,file,scen,label) %>% 
                summarise(value=sum(valuerel)),
              aes(x=ttoyear(t),y=value*100),linewidth=1.2) + 
    geom_line(data=closs %>% 
                filter(n=="World" & ttoyear(t)<=2100 & ttoyear(t)>=2020) %>% 
                group_by(t,n,file,scen) %>% 
                summarise(value=sum(valuerel)) %>%
                mutate(scen=case_when(scen=="scen1"~"scen2",scen=="scen2"~"scen1")) %>%
                mutate(label=recode(scen,!!!labels)),
              aes(x=ttoyear(t),y=value*100),linewidth=1.2,linetype=2,color="red") + 
    geom_line(data=closs %>% 
                filter(n=="World" & ttoyear(t)<=2100 & ttoyear(t)>=2020) %>% 
                group_by(t,n,file,scen,label) %>% 
                summarise(value=sum(valuerel)),
              aes(x=ttoyear(t),y=value*100),linewidth=1.2,color="red",vjust =0) + 
    geom_text(aes(x=2015,y=0.5,label="GDP loss")) +
    geom_text(aes(x=2015,y=-0.5,label="C loss"),color="red",vjust =0) +
    facet_grid(.~label,) + scale_fill_manual(name="",values=c("CDR COSTS"="#00aad0","ABATEMENT COSTS"="#c47900","GROWTH EFFECTS"="#808080","FUND EFFECTS"="#B2B2B2")) + 
    theme_pubr() + theme(legend.position = "bottom") + ylab("Costs [% global GDP]") + xlab("")
  
  costs$Region <- plyr::revalue(costs$n,names_regions)
  
  costreg <- ggplot( costs %>% group_by(t,Region,file,scen,label) %>% 
                       summarise(valuerel=sum(valuerel)) %>% 
                       filter(Region!="World" & ttoyear(t)>=2020 & ttoyear(t)<=2100) ) +
    geom_area( aes(x=ttoyear(t), y=valuerel, fill=Region), color="black", position="fill" ) +
    facet_grid( label ~ ., ) + theme_pubr() + ylab("Cost sharing [%]") + xlab("") + theme(legend.position="none") +
    scale_fill_manual(values=palette_regions)
  
  ggplot( costs %>% group_by(t,Region,file,scen,label) %>% 
            summarise(valuerel=sum(valuerel)) %>% 
            filter(Region!="World" & ttoyear(t)>=2020 & ttoyear(t)<=2100) ) +
    geom_line( aes(x=ttoyear(t), y=valuerel, color=Region), size=2) +
    facet_grid( label ~ ., ) + theme_pubr() + ylab("Cost sharing [%]") + xlab("") + 
    scale_color_manual(values=palette_regions)
  
  ggplot( Q_TRD %>% filter(file %in% files & ttoyear(t)<=2050) ) +
    geom_line( aes(x=ttoyear(t), y=value, color=n), size=2) +
    facet_grid( file ~ ., ) + theme_pubr() + ylab("Cost sharing [%]") + xlab("") + 
    scale_color_manual(values=palette_n)
  
  f2 <- ggarrange(costw,costreg,widths=c(0.65,0.35), heights=c(0.6,1), labels=c("c","d"))
  
  #### PRICES
  prices$name <- revalue(prices$name,c("cavg_cdr"="AVERAGE REMOVAL","cavg_ab"="AVERAGE ABATEMENT","mac"="MARGINAL ABATEMENT","mrc"="MARGINAL REMOVAL"))
  pricesplot <- ggplot(prices %>% filter(ttoyear(t)<=2100 & ttoyear(t)>=2020 & file %in% files)) +
    geom_line(data= . %>%filter(n=="World"),
              aes(x=ttoyear(t),y=value,color=name,linetype=label),linewidth=1.5) + 
    theme_pubr() + xlab('') + ylab('Prices [$/tonCO2]') + 
    theme(legend.position="bottom", legend.title = element_blank(), legend.box="vertical" ) +
    guides(color=guide_legend(nrow=2,byrow=TRUE),fill=guide_legend(nrow=2,byrow=TRUE)) +
    scale_fill_manual(name="",values=c("AVERAGE REMOVAL"="#00aad0","AVERAGE ABATEMENT"="#c47900",
                                       "MARGINAL ABATEMENT"="#CCAE71","MARGINAL REMOVAL"="#4393C3")) +
    scale_color_manual(name="",values=c("AVERAGE REMOVAL"="#00aad0","AVERAGE ABATEMENT"="#c47900",
                                        "MARGINAL ABATEMENT"="#CCAE71","MARGINAL REMOVAL"="#4393C3")) 
  
  giniplot <- ggplot(gini %>% filter(ttoyear(t)<=2100 & ttoyear(t)>=2020)) +
    geom_line(aes(x=ttoyear(t),y=ginirel),linewidth=1.5) + 
    geom_hline(yintercept=0) +
    theme_pubr() + xlab('') + ylab('Between country inequality') + 
    theme(legend.position="bottom", legend.title = element_blank()) +
    guides(color=guide_legend(nrow=2,byrow=TRUE),fill=guide_legend(nrow=2,byrow=TRUE)) +
    geom_segment(aes(x=2100, y=0, xend=2100, yend=max(ginirel)), arrow = arrow(length=unit(.5, 'cm')),color='red', lwd=3) +
    geom_text(aes(x=2095,y=max(ginirel)/2), label="+",color="red",size=10) +
    geom_segment(aes(x=2100, y=0, xend=2100, yend=min(ginirel)), arrow = arrow(length=unit(.5, 'cm')),color='green', lwd=3) +
    geom_text(aes(x=2095,y=min(ginirel)/2), label="-",color="green",size=10) 
  
  econ$type = revalue(econ$name,c("di_f"="FLOW","i"="FLOW","f"="STOCK","k"="STOCK","i_f"="FLOW"))
  econ$name = revalue(econ$name,c("di_f"="FUND DISINVESTMENTS","i"="FINAL GOOD INVESTMENTS","f"="FUND","k"="CAPITAL","i_f"="FUND INVESTMENTS"))
  fundplot <- ggplot(econ %>% filter(n=="World" & ttoyear(t)<=2100 & ttoyear(t)>=2020 & scen=="scen2")) +
    geom_line(aes(x=ttoyear(t),y=valuerel*100,color=name),linewidth=1.5) + 
    geom_line(data=. %>% mutate(valuerel=ifelse(name=="FUND DISINVESTMENTS",-valuerel,valuerel)) %>% group_by(t,n,file,scen,type) %>% summarise(valuerel=sum(valuerel)),aes(x=ttoyear(t),y=valuerel*100),linewidth=1.2,linetype=2,color="black") + 
    geom_hline(yintercept=0) +
    theme_pubr() + xlab('') + ylab('Variation of [% GDP]') + facet_wrap(.~type,scale="free") +
    theme(legend.position="bottom", legend.title = element_blank(), legend.box="vertical" ) +
    guides(color=guide_legend(nrow=2,byrow=TRUE),fill=guide_legend(nrow=2,byrow=TRUE)) 
  
  econ$Region <- plyr::revalue(econ$n,names_regions)
  
  fundreg <- ggplot(econ %>% filter(n!="World" & ttoyear(t)<=2100 & ttoyear(t)>=2020 & name=="FUND")) +
    geom_area(aes(x=ttoyear(t),y=value*100,fill=Region),color="black") + 
    geom_hline(yintercept=0) +
    theme_pubr() + xlab('') + ylab('') + 
    theme(legend.position="none", legend.title = element_blank()) +
    facet_grid( label ~ ., ) +
    scale_fill_manual(values=palette_regions)
  
  f3 <- ggarrange(pricesplot,fundplot,fundreg,widths=c(0.3,0.45,0.25),nrow=1,labels=c("e","f","g"))
  
  final_plot <- ggarrange(f1,f2,f3,nrow=3)
  ggsave(file.path(main_directory,paste0(paste(labels,collapse="_"),".png")),height=17,width=15)
  
  return(final_plot) }

