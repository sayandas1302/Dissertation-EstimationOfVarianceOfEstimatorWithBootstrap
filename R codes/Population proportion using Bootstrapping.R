rm(list=ls())
library(ggplot2)
library(patchwork)
library(dplyr)
set.seed(seed=987654321)
P=0.3 #parameters
R=1000 #simulation number
B=100#boostrap number
n=20 #sample size
 
#simulation===============================================
data=data.frame(p=replicate(R,mean(rbinom(n,1,P))))
                          #generating sample proportions 
p.est.simu=mean(data[,1]);p.est.simu
se.p.simu=sqrt((R-1)/R)*sd(data[,1]);se.p.simu
                          #calculate SE of the sample proportion
g1=ggplot(data,aes(x=p))+
        geom_histogram(aes(y=..density..),bins=15,
                       fill="#313552",colour="black")+
        geom_vline(xintercept=P,colour="red")+
        labs(x="p.hat",y="Density",
             title="Histogram of estimates of P through simulation")
                          #histogram of p in simulation

#bootstrap===============================================
p.boot=array(0)
se.p.star=array(0)
for(i in 1:R) #simulation loop
{
        p.star=array(0) #bootstrap p initialization
        x=rbinom(n,1,P) #original sample
        for(j in 1:B) #bootstrap loop
        {
                x.star=sample(x,n,replace=T)
                               #bootstrap data
                p.star[j]=mean(x.star)
                               #bootstrap p
        }
        se.p.star[i]=sqrt((B-1)/B)*sd(p.star) 
                               #SE of the SE of p
        p.boot[i]=mean(p.star)
                               #estimates of p
}
p.est.boot=mean(p.boot);p.est.boot
se.p.boot=mean(se.p.star);se.p.boot
                        #estimate of SE of p
data=data%>%
        mutate(p.boot=p.boot)
g2=ggplot(data,aes(x=p))+
        geom_histogram(aes(y=..density..),fill="#B8405E",
                       bins=16,colour="black")+
        geom_vline(xintercept=0.3,colour="red")+
        labs(x="p.hat",y="Density",
             title="Histogram of estimates of P through bootstrap")
                        #histogram of x-bar in bootstrapping 
g1+g2
#theoretical SE of p==================================
se.p.orig=sqrt(P*(1-P)/n);se.p.orig
