rm(list=ls())
library(nimble)
library(ggplot2)
library(patchwork)
set.seed(seed=123456789)
n=15 #sample size
B=1000 #bootstrap number
mu=10 #initial Value Of the parameter to generate the data
sigma=2
x=rdexp(n,location=mu,scale=sigma) #given data

#bootstrap
mu.hat=sigma.hat=array(0)
for(i in 1:B)
{
  x.star=sample(x,n,replace=T) #bootstrap data
  mu.hat[i]=median(x.star) #MLE for mu
  sigma.hat[i]=mean(abs(x.star-median(x.star))) #MLE for sigma 
}
mu.est=mean(mu.hat);mu.est #estimate of the mu by bootstrap
sigma.est=mean(sigma.hat);sigma.est #estimate of the sigma by bootstrap
SE.mu.hat=sqrt((B-1)/B)*sd(mu.hat);SE.mu.hat #se of mu
SE.sigma.hat=sqrt((B-1)/B)*sd(sigma.hat);SE.sigma.hat #se of sigma

#graphs
data=data.frame(mu.hat=mu.hat,sigma.hat=sigma.hat)
g1=ggplot(data,aes(x=mu.hat))+
    geom_histogram(aes(y=..density..),bins=15,
                   fill="#008E89",colour="black")+
    geom_vline(xintercept=mu,colour="red")+
    labs(x="mu.hat",y="Density",
         title="Histogram of estimates of Mu by bootstrap")

g2=ggplot(data,aes(x=sigma.hat))+
    geom_histogram(aes(y=..density..),bins=15,
                   fill="#085E7D",colour="black")+
    geom_vline(xintercept=sigma,colour="red")+
    labs(x="sigma.hat",y="Density",
         title="Histogram of estimates of Sigma by bootstrap")  
g1+g2
  
#theoretical se for mu.hat using large sample
se.actual=sigma/sqrt(n);se.actual
