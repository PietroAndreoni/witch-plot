require(tidyverse)
require(ggpubr)

# flat macc
pot <- 0.2
flat <- tibble(q=seq(0,1.2,by=0.05),ca=2000,cr=200) %>%
  mutate(ca=ifelse(q<=1,1000*q^2,ca),calag = ifelse(q>=pot,1000*(q-pot)^2,ca)) %>%
  mutate(cinf=case_when( ca<cr ~  ca,
                         cr<=ca ~ cr ),
         clim= case_when( ca<cr ~  ca,
                          cr<=ca & q >= 0.45 & q <= 0.45 + pot ~ cr,
                          cr<=ca & q > 0.45 + pot ~ calag ) ) %>% 
  mutate(ca=ifelse(ca>=2000,NA,ca))


f1 <- ggplot(flat, aes(x=q)) +
  geom_line(aes(y=ca),color="black",size=2) +
  geom_vline(xintercept=1,color="grey",linetype=2,size=1) +
  geom_vline(xintercept=1.2,color="grey",linetype=2,size=1) +
  geom_hline(aes(yintercept=clim[q==1]),color="grey",linetype=2,size=1) +
  geom_hline(aes(yintercept=clim[q==1.2]),color="grey",linetype=2,size=1) +
  geom_text(x=0.2,aes(y=clim[q==1]+20),label="cprice at net-zero, \n limited CDR" ) +
  geom_text(x=0.2,aes(y=clim[q==1.2]+20),label="cprice at max overshoot, \n limited CDR" ) +
  geom_text(x=0.95,y=50,label="net-zero",angle=90 ) +
  geom_text(x=1.15,y=60,label="max net-neg",angle=90 ) +
  geom_text(x=1,aes(y=ca[q==1]+20),label="MACC, no CDR" ) +
  geom_line(aes(y=cinf),color="red",size=2) +
  geom_text(x=1,aes(y=cinf[q==1]+20),label="MACC, infinite CDR",color="red" ) +
  geom_line(aes(y=clim),color="blue",size=2) +  
  geom_text(x=1.2,aes(y=clim[q==1.2]-10),label="MACC, \nconstrained CDR",color="blue" ) +
  geom_rect(data=.%>%filter(q==1.2),aes(ymax=clim),xmin=0.45,xmax=0.65,ymin=200,fill="red",alpha=0.2,color="red",linetype=2) +
  geom_rect(data=.%>%filter(q==1),aes(ymax=clim),xmin=0.45,xmax=0.65,ymin=200,fill="blue",alpha=0.2,color="blue") +
  ylab('MACC [$/tonCO2]') + xlab('Abatement fraction [fraction of baseline emissions]') + 
  theme_pubr() +
  theme(text = element_text(size = 7))



#time evolution 
t1 <- tibble(q=seq(0,1.2,by=0.05),ca=2000,cr=300) %>%
  mutate(ca=ifelse(q<=1,1000*q^2,ca),calag = ifelse(q>= 0.5 +0.05,1000*(q-0.05)^2,ca)) %>%
  mutate(clim= case_when( ca<cr ~  ca,
                          cr<=ca & q >= 0.5 & q <= 0.5  + 0.05 ~ cr,
                          cr<=ca & q > 0.5  + 0.05 ~ calag ) ) %>% 
  mutate(clim=ifelse(q>1+0.05,NA,clim))

t2 <- tibble(q=seq(0,1.2,by=0.05),ca=2000,cr=100) %>%
  mutate(ca=ifelse(q<=1,600*q^2,ca),calag = ifelse(q>=0.4+0.2,600*(q-0.2)^2,ca)) %>%
  mutate(clim= case_when( ca<cr ~  ca,
                          cr<=ca & q >= 0.4 & q <= 0.4 + 0.2 ~ cr,
                          cr<=ca & q > 0.4 + 0.2 ~ calag ) ) %>% 
  mutate(clim=ifelse(q>1+0.2,NA,clim))

f2 <- ggplot() +
  geom_line(data=t1,aes(x=q,y=clim),color="red",size=2) +
  geom_line(data=t2,aes(x=q,y=clim),color="blue",size=2) + 
  geom_text(data=t1,x=1,aes(y=clim[q==1+0.05]+10),label="MACC, t0",color="red" ) +
  geom_text(data=t2,x=1.2,aes(y=clim[q==1+0.2]+10),label="MACC, t1",color="blue" ) +
  geom_segment(aes(y=300,yend=300,x=0,xend=0.5),color="red",linetype="dotted",size=1) +
  geom_segment(aes(y=100,yend=100,x=0,xend=0.5),color="blue",linetype="dotted",size=1) +
  geom_segment(aes(y=300,yend=100,x=0.15,xend=0.15),arrow=arrow(),size=1) +
  geom_text(aes(y=200,x=0.17),label="CDR gets \n cheaper...",hjust=0 ) +
  geom_segment(aes(y=500,yend=500,x=0.8,xend=1.1),arrow=arrow(),size=1) +
  geom_text(aes(y=550,x=0.95),label="mitigation gets \n cheaper" ) +
  geom_segment(aes(y=80,yend=80,x=0.4,xend=0.6),size=1) +
  geom_segment(aes(y=70,yend=90,x=0.4,xend=0.4),size=1) +
  geom_segment(aes(y=70,yend=90,x=0.6,xend=0.6),size=1) +
  geom_text(aes(y=50,x=0.5),label="... and more available") +
  ylab('MACC [$/tonCO2]') + xlab('Abatement fraction [fraction of baseline emissions]') + 
  theme_pubr() +
  theme(text = element_text(size = 7))

ggarrange(f1,f2,labels=c("a","b"))
ggsave("SI_macc.png",width=18,height=10,units="cm",dpi=300)
