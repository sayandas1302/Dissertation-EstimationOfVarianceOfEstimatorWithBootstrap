#Using Methods Of Moments Estimates
rm(list=ls())
library(VGAM)
library(ggplot2)
library(patchwork)
set.seed(seed=123456789)
n=15
B=1000
Pi = 0.5
lambda = 2  #initial parameter value
Pi.hat = array(0)
lambda.hat = array(0)
x=rzipois(n, lambda=lambda, pstr0=Pi)
for(i in 1:B) #loops for bootstrapping
{
  x.star=sample(x,n,replace=T) #bootstrap data
  m1.dash=mean(x.star)
  m2=((n-1)/n)*var(x.star)
  lambda.hat[i]=m1.dash+(m2/m1.dash)-1
  Pi.hat[i]=(m2-m1.dash)/(m1.dash^2+m2-m1.dash)
}
Pi.hat[!is.finite(Pi.hat)]=NA
lambda.hat[!is.finite(lambda.hat)]=NA

lambda.est=mean(lambda.hat,na.rm=T);lambda.est #estimate of lambda
Pi.est=mean(Pi.hat,na.rm=T);Pi.est  #estimate of Pi
SE.lambda.hat=sqrt((B-1)/B)*sd(lambda.hat, na.rm=T);SE.lambda.hat #se of lambda
SE.Pi.hat=sqrt((B-1)/B)*sd(Pi.hat, na.rm=T);SE.Pi.hat #se of Pi


#using Maximum Likelyhood Estimates
rm(list=ls())
library(VGAM)
library(stats4)
library(ggplot2)
library(patchwork)
L=function(Pi,lambda){
  -sum(dzipois(x.star,lambda=lambda, pstr0=Pi, log=T))
}
#set.seed(seed=123456789)
n=15
B=1000
Pi=0.3
lambda=4  #initial parameter value
Pi.hat=array(0)
lambda.hat=array(0)
x=rzipois(n, lambda=lambda, pstr0=Pi)
for(i in 1:B) #loops for bootstrapping
{
  x.star=sample(x,n,replace=T) #bootstrap data
  lambda.hat[i]=mle(L,start=c(0.1,0.1))@coef[2]
  Pi.hat[i]=mle(L,start=c(0.1,0.1))@coef[1]
}
  
lambda.est=mean(lambda.hat,na.rm=T);lambda.est #estimate of lambda
Pi.est=mean(Pi.hat,na.rm=T);Pi.est  #estimate of Pi
SE.lambda.hat=sd(lambda.hat, na.rm=T);SE.lambda.hat #se of lambda
SE.Pi.hat=sd(Pi.hat, na.rm=T);SE.Pi.hat #se of Pi

#Plots
data=data.frame(Pi.hat=Pi.hat,lambda.hat=lambda.hat)
g1=ggplot(data,aes(x=Pi.hat))+
      geom_histogram(aes(y=..density..),fill="#D8D2CB",
                     colour="black",bins=10)+
      geom_vline(xintercept=Pi,colour="red")+
      labs(x="Pi.hat",
           y="Density",
           title="Histogram of the estimates of Pi through Bootstrap")
g2=ggplot(data,aes(x=lambda.hat))+
      geom_histogram(aes(y=..density..),fill="#398AB9",
                     colour="black",bins=10)+
      geom_vline(xintercept=lambda,colour="red")+
      labs(x="lambda.hat",
           y="Density",
           title="Histogram of the estimates of lambda through Bootstrap")
g1|g2