wild <- read.csv("Serengeti_wildebeest_interp.csv")
M <- nrow(wild)
wild$DeltaN <- wild$Wild_interp[2:(M+1)] - wild$Wild_interp
wild$Y <- wild$DeltaN / wild$Wild_interp
wild <- wild[1:nrow(wild)-1,]

plot(wild$Wild_interp,wild$Y,xlab="Nt",ylab="DeltaN / Nt")
reg <- lm(Y ~ Wild_interp, data=wild)
coefs <- coef(reg)
r <- coefs[1]
K <- -r/coefs[2]

# Plot modeled wildebeest numbers against data
plot(wild$Year,wild$Wildebeest,pch=17,cex=2,xlab="Year",ylab="Wildebeest")
Tmax <- nrow(wild)
Nsim <- numeric(Tmax)
Nsim[1] <- 250000
for (t in 2:Tmax){
  Nsim[t] <- Nsim[t-1] + r * Nsim[t-1] * (1 - Nsim[t-1] / K)
}
time <- seq(1960,2002)
lines(time,Nsim,col="blue")