# R driver for solving Bayesian state space logistic model
library(R2WinBUGS)
library(emdbook)
library(mcmcplots)
setwd("~/MU Courses/Graduate statistical modeling class/Nov 27")
logdat <- read.table("logistic_data.txt",header=T)
obs <- logdat$N                        
N = length(obs)
statespace.data <- list("N", "obs")
inits <- list(list(r = 0.2, K=12, tau.obs = 1, tau.proc = 1),
  list(r = 0.3, K=7, tau.obs = 0.5, tau.proc = 1.5),
  list(r = 0.4, K=9, tau.obs = 1.5, tau.proc = 0.5))
parameters <- c("r", "K", "sigma.obs", "sigma.proc", "true")
statespace.bugs <- bugs(data=statespace.data,inits,param=parameters,
  model="statespace.txt",n.chains=3,n.iter=10000,
  working.directory="~/MU Courses/Graduate statistical modeling class/Nov 27")
print(statespace.bugs,digits=4)
s1 = as.mcmc.bugs(statespace.bugs)
denplot(s1,parms="r")
denplot(s1,parms="K")
denplot(s1,parms="sigma.obs")
denplot(s1,parms="sigma.proc")