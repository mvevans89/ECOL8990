ser <- read.csv("Serengeti_wildebeest.csv", header=T)
tmax <- nrow(ser) # number of years in time series
N.obs <- ser$Wildebeest

# Function to obtain the fit of simulated values to data (r and K)
logis.fit1 = function(par){
  N.sim <- as.numeric(tmax)
  N.sim[1] <- 250000
  D = 0
  for (j in 2:tmax){
    N.sim[j] <- N.sim[j-1] + par[1] * N.sim[j-1] * (1 - N.sim[j-1] / par[2])
    # par[1] is r and par[2] is K
    if (!is.na(N.obs[j])) D = D + ((N.sim[j] - N.obs[j])^2)
  }
  D # sum of squared deviations
}

# Function 2: make K a function of dry-season rainfall
logis.fit2=function(par){
  N.sim <- as.numeric(tmax)
  N.sim[1] <- 250000
  D = 0
  for (j in 2:tmax){
    N.sim[j] <- N.sim[j-1] + par[1] * N.sim[j-1] * (1 - N.sim[j-1] / (par[2] * ser$Rdry[j-1]))
    # par[1] is r and par[2] is K
    if (!is.na(N.obs[j])) D = D + ((N.sim[j] - N.obs[j])^2)
  }
  D # sum of squared deviations
}

# Find the best-fit parameter values
mod.fit1 <- optim(c(0.25,1.1E6),fn=logis.fit1,method="Nelder-Mead")
mod.fit2 <- optim(c(0.25, 10000),fn=logis.fit2,method="Nelder-Mead")

# Generate simulated data based on these best-fit parameter values
par1 <- mod.fit1$par
r.best1 <- par1[1]
K.best1 <- par1[2]
par2 <- mod.fit2$par
r.best2 <- par2[1]
Omega.best2 <- par2[2]
t <- 1:tmax
Year <- 1959 + t
N.sim1 <- as.numeric(tmax)
N.sim2 <- as.numeric(tmax)
N.sim1[1] <- 250000
N.sim2[1] <- 250000
for (j in 2:tmax){
  N.sim1[j] <- N.sim1[j-1] + r.best1 * N.sim1[j-1] * (1 - N.sim1[j-1] / K.best1)
  N.sim2[j] <- N.sim2[j-1] + r.best2 * N.sim2[j-1] * (1 - N.sim2[j-1] / (Omega.best2* ser$Rdry[j-1]))
}
sub <- ser[complete.cases(ser$Wildebeest), ]
plot(sub$Year, sub$Wildebeest, cex=2, xlab="Year", ylab="Wildebeest", pch=17)
lines(Year,N.sim1, col="blue")
lines(Year,N.sim2, col="red")

