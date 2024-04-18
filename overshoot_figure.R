



a1 <- solve(t(matrix(c(1,1,1,
                 1,18,18^2,
                 0,1,2*11), 
               nrow=3,ncol=3)), 
      c(1.15,1.5,0)) 


a2 <- solve(t(matrix(c(1,1,1,
                       1,18,18^2,
                       0,1,2*11.5), 
                     nrow=3,ncol=3)), 
            c(1.15,1.5,0)) 

a3 <- solve(t(matrix(c(1,1,1,
                       1,18,18^2,
                       0,1,2*12), 
                     nrow=3,ncol=3)), 
            c(1.15,1.5,0)) 

a4 <- solve(t(matrix(c(1,1,1,
                       1,18,18^2,
                       0,1,2*15), 
                     nrow=3,ncol=3)), 
            c(1.15,1.5,0))

ggplot() + 
    geom_line(data=data.frame(t=1:18) %>%
                mutate(T=(a1[[1]]+a1[[2]]*t+a1[[3]]*t^2) ),
              aes(x=t*5+2015,y=T),
              color="darkblue",
              linewidth=1.2) +
    geom_line(data=data.frame(t=1:18) %>%
                mutate(T=(a2[[1]]+a2[[2]]*t+a2[[3]]*t^2) ),
              aes(x=t*5+2015,y=T),
              color="blue",
              linewidth=1.2) +
    geom_line(data=data.frame(t=1:18) %>%
                mutate(T=(a3[[1]]+a3[[2]]*t+a3[[3]]*t^2) ),
              aes(x=t*5+2015,y=T),
              color="lightblue",
              linewidth=1.2) +
    geom_line(data=data.frame(t=1:18) %>%
                mutate(T=(a4[[1]]+a4[[2]]*t+a4[[3]]*t^2) ),
              aes(x=t*5+2015,y=T),
              color="black",
              linetype=2,
              linewidth=1.2) + 
    theme_pubr() + 
    xlab("Year") +
    ylab("Global temperature increase [°C]")
  
ggplot() + 
  geom_line(data=data.frame(t=1:18) %>%
              mutate(T=(a1[[1]]+a1[[2]]*t+a1[[3]]*t^2) ),
            aes(x=t*5+2015,y=T),
            color="darkblue",
            linewidth=1.2) +
  geom_line(data=data.frame(t=1:18) %>%
              mutate(T=(a2[[1]]+a2[[2]]*t+a2[[3]]*t^2) ),
            aes(x=t*5+2015,y=T),
            color="blue",
            linewidth=1.2) +
  geom_line(data=data.frame(t=1:18) %>%
              mutate(T=(a3[[1]]+a3[[2]]*t+a3[[3]]*t^2) ),
            aes(x=t*5+2015,y=T),
            color="lightblue",
            linewidth=1.2) +
  geom_line(data=data.frame(t=1:18) %>%
              mutate(T=(a4[[1]]+a4[[2]]*t+a4[[3]]*t^2) ),
            aes(x=t*5+2015,y=T),
            color="black",
            linetype=2,
            linewidth=1.2) + 
  theme_pubr() + 
  xlab("Year") +
  ylab("Global temperature increase [°C]")



ggplot() + 
  geom_line(data=data.frame(t=1:18) %>%
              rowwise() %>% mutate(T=min(1.5,(a2[[1]]+a2[[2]]*t+a2[[3]]*t^2) ) ),
            aes(x=t*5+2015,y=T),
            color="blue",
            linewidth=1.2) +
  
  geom_line(data=data.frame(t=1:18) %>%
              mutate(T=(a2[[1]]+a2[[2]]*t+a2[[3]]*t^2) ),
            aes(x=t*5+2015,y=T),
            color="blue",
            linetype=2,
            linewidth=1.2) +
  geom_ribbon(data= data.frame(t=1:18) %>%
              rowwise() %>% mutate(T=(a2[[1]]+a2[[2]]*t+a2[[3]]*t^2),
                     T2=min(1.5,(a2[[1]]+a2[[2]]*t+a2[[3]]*t^2) )),
            aes(x=t*5+2015,ymin=T,ymax=T2),
            fill="blue",
            alpha=0.5,
            linewidth=1.2) +
  theme_pubr() + 
  xlab("Year") +
  ylab("Global temperature increase [°C]")

