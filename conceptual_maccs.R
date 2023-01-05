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
  

ggplot(flat, aes(x=q)) +
  geom_line(aes(y=ca),color="black") +
  geom_line(aes(y=cinf),color="red") +
  geom_line(aes(y=clim),color="blue") +  
  geom_vline(xintercept=1,color="grey",linetype=2) +
  geom_vline(xintercept=1.2,color="grey",linetype=2) +
  geom_hline(aes(yintercept=clim[q==1]),color="grey",linetype=2) +
  geom_hline(aes(yintercept=clim[q==1.2]),color="grey",linetype=2) +
  geom_text(x=0.2,aes(y=clim[q==1]+5),label="cprice at net-zero, \n limited CDR" ) +
  geom_text(x=0.2,aes(y=clim[q==1.2]+5),label="cprice at max overshoot, \n limited CDR" ) +
  geom_text(x=0.95,y=50,label="net-zero",angle=90 ) +
  geom_text(x=1.15,y=60,label="max net-neg",angle=90 ) +
  geom_rect(data=.%>%filter(q==1.2),aes(ymax=clim),xmin=0.45,xmax=0.65,ymin=200,fill="red",alpha=0.2,color="red",linetype=2) +
  geom_rect(data=.%>%filter(q==1),aes(ymax=clim),xmin=0.45,xmax=0.65,ymin=200,fill="blue",alpha=0.2,color="blue") +
  ylab('MACC [$/tonCO2]') + xlab('Abatement fraction [fraction of baseline emissions]') + theme_pubr()


#### 
quad <- tibble(q=seq(0,3.2,by=0.05),ca=NA,cr=200) %>%
  mutate(ca=ifelse(q<=1,200*q+600*q^4,ca),
         cr1=cr+((200*0.75+600*0.75^4-200)/(0.75)) *q,
         cr2=cr+((200*0.75+600*0.75^4-200)/(0.75)^2) *q^2)


ggplot(quad, aes(x=q)) +
  geom_line(aes(y=ca),color="black") +
  geom_line(data=.%>%filter(cr1<=ca[q==1]),aes(y=cr1),color="red") +
  geom_line(data=.%>%filter(cr2<=ca[q==1]),aes(y=cr2),color="blue") +  
  geom_vline(xintercept=1,color="grey",linetype=2) 
  