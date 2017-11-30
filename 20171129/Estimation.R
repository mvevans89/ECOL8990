# function to fit an observation error-only logistic model (trajectory matching)
obserror = function(x){
  Nsim <- as.numeric(tmax)
  Nsim[1] <- 1
  for (j in 2:tmax){
    Nsim[j] <- Nsim[j-1]+x[1]*Nsim[j-1]*(1-Nsim[j-1]/x[2])
  }
  sum((Nsim-N.obs)^2,log=T) # sum of squared deviations
}

# function to fit a process error-only logistic model (one-step-ahead prediction)
procerror = function(x){
  Nsim <- as.numeric(tmax)
  Nsim[1] <- 1
  for (j in 2:tmax){
    Nsim[j] <- N.obs[j-1]+x[1]*N.obs[j-1]*(1-N.obs[j-1]/x[2])
  }
  sum((Nsim-N.obs)^2,log=T) # sum of squared deviations
}

set.seed(1003)
tmax <- 50 # number of years in time series
r <- 0.25 # pop. growth rate
K <- 10 # carrying capacity
sigma.obs <- 1 # observation error
sigma.proc <- 1 # process error
N.det <- as.numeric(50) # deterministic population size (no error)
N.true <- as.numeric(50) # true population values
N.obs <- as.numeric(50) # observed values
# Initial values
N.det[1] <- 1
N.true[1] <- 1
N.obs[1] <- N.true[1]+rnorm(1,0,sigma.obs)
for (i in 2:50){
  N.det[i] <- N.det[i-1]+N.det[i-1]*r*(1-N.det[i-1]/K)
  N.true[i] <- N.true[i-1]+N.true[i-1]*r*(1-N.true[i-1]/K)+rnorm(1,0,sigma.proc)
  N.obs[i] <- N.true[i]+rnorm(1,0,sigma.obs)
}
t <- 1:50
plot(t,N.det, type="l",col="red",ylim=c(0,13))
lines(t,N.true,col="green")
lines(t,N.obs,col="blue")
# Recover the parameter values assuming...
# ... observation error only...
fit.obs <- optim(c(0.2,10),fn=obserror,method="Nelder-Mead")
# ... process error only
fit.proc <- optim(c(0.2,10),fn=procerror,method="Nelder-Mead")
