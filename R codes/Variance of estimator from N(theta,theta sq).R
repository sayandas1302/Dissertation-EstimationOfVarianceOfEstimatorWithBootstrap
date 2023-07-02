rm(list=ls())
library(ggplot2)
set.seed(seed=123456789)
n=15
B=1000
theta=3 #initial parameter value
theta.hat=array(0)
x=rnorm(n,theta,theta)
for(i in 1:B) #loops for bootstrapping
{
  x.star=sample(x,n,replace=T) #bootstrap data
  m1.dash=mean(x.star)
  m2.dash=mean(x.star^2) 
  k=sqrt(m1.dash^2+4*m2.dash)
  theta.hat[i]=(k-m1.dash)/2
}
theta.est=mean(theta.hat);theta.est #estimate of theta
SE.theta.hat=sqrt((B-1)/B)*sd(theta.hat);SE.theta.hat #se of theta

#plots
data=data.frame(theta.hat=theta.hat)
ggplot(data,aes(x=theta.hat))+
  geom_histogram(aes(y=..density..),bins=15,
                 fill="#E45826",colour="black")+
  geom_vline(xintercept=theta,colour="red")+
  labs(x="theta.hat",
       y="Density",
       title="Histogram of the estimates of theta through Bootstrap")

#original variance of theta(approximated for large sample)
se.theta.orig=sqrt(3/(4*n))*theta;se.theta.orig
