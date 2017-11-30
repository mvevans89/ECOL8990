# Simple linear population model with observation error only
set.seed(1001)
nt <- 20 # Number of time steps
a <- 100
b <- 10
sd.obs <- 5
N <- numeric(nt)
Nobs <- numeric(nt)
N[1] <- a # Initial conditions
for (t in 1:(nt-1)){
  N[t+1] <- N[t] + b
  Nobs[t] <- rnorm(1, mean = N[t], sd = sd.obs)
}
Nobs[nt] <- rnorm(1, mean = N[nt], sd = sd.obs)
plot(Nobs, type="l")