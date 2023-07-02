rm(list=ls())
library(ggplot2)
set.seed(seed=123456789)
n=15
B=1000
alpha=3 #initial parameter value
alpha.hat=array(0)
x=rbeta(n,alpha,alpha)
for(i in 1:B){ #loops for bootstrapping
  x.star=sample(x,n,replace=T) #bootstrap data
  m2=((n-1)/n)*var(x.star) 
  alpha.hat[i]=(1-4*m2)/(8*m2)}

alpha.est=mean(alpha.hat);alpha.est #estimate of alpha
SE.alpha.hat=sqrt((B-1)/B)*sd(alpha.hat);SE.alpha.hat #se of alpha

#plots
data=data.frame(alpha.hat=alpha.hat)
ggplot(data,aes(x=alpha.hat))+
  geom_histogram(aes(y=..density..),fill="#019267",
                 colour="black",bins=20)+
  geom_vline(xintercept=alpha,colour="red")+
  labs(x="alpha.hat",
       y="Density",
       title="Histogram of the estimates of alpha obtained through Bootstrap")


rm(list=ls())
library(stats4)
library(ggplot2)
set.seed(seed=123456789)
n=15
B=1000
alpha=3 #initial parameter value
alpha.hat=array(0)
x=rbeta(n,alpha,alpha)
L=function(alpha){
  -sum(dbeta(x.star,alpha,alpha,log=T))
}
for(i in 1:B) #loops for bootstrapping
{
  x.star=sample(x,n,replace=T) #bootstrap data
  alpha.hat[i]=mle(L,start=0.1)@coef[1]
}
alpha.est=mean(alpha.hat);alpha.est #estimate of alpha
SE.alpha.hat=sqrt((B-1)/B)*sd(alpha.hat);SE.alpha.hat #se of alpha
