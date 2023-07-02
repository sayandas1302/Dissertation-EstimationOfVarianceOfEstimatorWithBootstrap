rm(list=ls())
library(ggplot2)
library(patchwork)
set.seed(seed=123456789)
n=15 #sample size
B=1000 #bootstrap number
alpha=4 #initial Value Of the parameter to generate the data
beta=5
x=rbeta(n,alpha,beta) #given data

alpha.hat=beta.hat=array(0)
for(i in 1:B)
{
  x.star=sample(x,n,replace=T) #bootstrap data
  m1.dash=mean(x.star)
  m2=((n-1)/n)*var(x.star)
  alpha.hat[i]=m1.dash*((m1.dash*(1-m1.dash)/m2)-1) #MME for alpha
  beta.hat[i]=(1-m1.dash)*((m1.dash*(1-m1.dash)/m2)-1) #MME for beta
}
alpha.est=mean(alpha.hat);alpha.est #estimate of the alpha by bootstrap
beta.est=mean(beta.hat);beta.est #estimate of the beta by bootstrap
SE.alpha.hat=sqrt((B-1)/B)*sd(alpha.hat);SE.alpha.hat #se of alpha
SE.beta.hat=sqrt((B-1)/B)*sd(beta.hat);SE.beta.hat #se of alpha



rm(list=ls())
library(stats4)
library(ggplot2)
library(patchwork)
set.seed(seed=123456789)
n=15
B=1000
alpha=3
beta=5  #initial parameter value
alpha.hat=array(0)
beta.hat=array(0)
x=rbeta(n,alpha,beta)
L=function(alpha,beta){
  -sum(dbeta(x.star,alpha,beta,log=T))
}
for(i in 1:B) #loops for bootstrapping
{
  x.star=sample(x,n,replace=T) #bootstrap data
  alpha.hat[i]=mle(L,start=c(0.1,0.1))@coef[1]
  beta.hat[i]=mle(L,start=c(0.1,0.1))@coef[2]
}
alpha.est=mean(alpha.hat);alpha.est #estimate of alpha
beta.est=mean(beta.hat);beta.est #estimate of beta
SE.alpha.hat=sqrt((B-1)/B)*sd(alpha.hat);SE.alpha.hat #se of alpha
SE.beta.hat=sqrt((B-1)/B)*sd(beta.hat);SE.beta.hat #se of beta

#plots
data=data.frame(alpha.hat=alpha.hat,beta.hat=beta.hat)
g1=ggplot(data,aes(x=alpha.hat))+
     geom_histogram(aes(y=..density..),fill="#39AEA9",
                    colour="black",bins=15)+
     geom_vline(xintercept=alpha,colour="red")+
     labs(x="alpha.hat",
          y="Density",
          title="Histogram of the estimates of alpha through Bootstrap")
g2=ggplot(data,aes(x=beta.hat))+
      geom_histogram(aes(y=..density..),fill="#A2D5AB",
                     colour="black",bins=15)+
      geom_vline(xintercept=beta,colour="red")+
      labs(x="beta.hat",
           y="Density",
           title="Histogram of the estimates of beta through Bootstrap")
g1|g2