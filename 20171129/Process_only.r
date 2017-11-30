# Simple linear population model with process error only
set.seed(1001)
nt <- 20 # Number of time steps
a <- 100
b <- 10
sd.proc <- 5
N <- numeric(nt)
Nobs <- numeric(nt)
N[1] <- a # Initial conditions

for (t in 1:(nt-1)){
  N[t+1] <- rnorm(1, mean = N[t] + b, sd=sd.proc)
  Nobs[t] <- N[t]
}

Nobs[nt] <- N[nt]
plot(Nobs,type="l")