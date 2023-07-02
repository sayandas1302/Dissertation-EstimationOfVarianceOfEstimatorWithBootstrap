#==============================================
#Simulation and Bootstrapping for Mu in N(Mu,1) 
#==============================================
rm(list=ls())
library(ggplot2)
library(patchwork)
library(dplyr)
set.seed(seed=987654321)

#declaration====================================
mu=50 #parameters
R=1000 #simulation number
B=100 #bootstrap number
n=20 #sample size

#simulation=====================================
data=data.frame(x.bar.simu=replicate(R,mean(rnorm(n,mu,1)))) 
                  #genearting sample means
mu.est=mean(data[,1]);mu.est
se.x.bar.simu=sqrt((R-1)/R)*sd(data[,1]);se.x.bar.simu 
                  #calculating the SE of sample mean
g1=ggplot(data,aes(x=x.bar.simu))+
        geom_histogram(aes(y=..density..),fill="#D9534F",
                 colour="black",bins=15)+
        geom_vline(xintercept=50,color="red")+
        labs(x="mu.hat",y="Density",
                 title="Histogram of estimates of mu through simulation")
                  #histogram of x-bar in simulation

#bootstrap=======================================
x.bar.boot=array(0)
se.x.bar.star=array(0)
for(i in 1:R) #simulation loop
{
  x.bar.star=array(0) #bootstrap mean initialization
  x=rnorm(n,mu,1) #original sample
  for(j in 1:B) #bootstrap loop
  {
    x.star=sample(x,n,replace=T) #bootstrap data
    x.bar.star[j]=mean(x.star) #bootstrap mean generation
  }
  se.x.bar.star[i]=sqrt((B-1)/B)*sd(x.bar.star) 
                       #SE of sample bootstrap means 
  x.bar.boot[i]=mean(x.bar.star)
                       #estimate of sample mean
}
mu.est=mean(x.bar.boot);mu.est
se.x.bar.boot=mean(se.x.bar.star);se.x.bar.boot
                      #estimate of SE of sample mean
data=data%>%
  mutate(x.bar.boot=x.bar.boot)

g2=ggplot(data,aes(x=x.bar.boot))+
        geom_histogram(aes(y=..density..),fill="#96CEB4",
                 colour="black",bins=15)+
        geom_vline(xintercept=50,color="red")+
        labs(x="mu.hat",y="Density",
                 title="Histogram of estimates of mu through Bootstrap")
                      #histogram of x-bar in bootstrapping  
g1+g2

#theoretical SE of x-bar===========================
se.x.bar.orig=1/sqrt(n);se.x.bar.orig
