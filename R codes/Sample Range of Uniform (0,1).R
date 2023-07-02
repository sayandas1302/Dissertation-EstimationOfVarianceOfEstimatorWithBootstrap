rm(list=ls())
sample_range=function(x)
  return(max(x)-min(x))
B=1000
n=15

x=runif(n,0,1);x
y=replicate(B,sample_range(sample(x,n,replace=T)))
exp_samp_range=mean(y);exp_samp_range
var_samp_range=var(y);var_samp_range