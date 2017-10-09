N <- 10
x <- runif(N, min = 0, max = 20)
a <- 2
b <- 1
sd <- 7
nsim <- 400
pval <- numeric(nsim)
for (i in 1:nsim){
  y_det <- a + b * x
  y <- rnorm(N, mean = y_det, sd = sd)
  m <- lm(y ~ x)
  pval[i] <- coef(summary(m))["x", "Pr(>|t|)"]
}

hist(pval)
mean(pval<0.05) #this is giving us the proportion of time that pvalue is less than 0.05


#now systematically cycle over coefficients, sd, and simulations
N <- 6
x <- runif(N, min = 0, max = 20)
a <- 2
bVec <- seq(-2,2,by=0.1)
sd <- 7
nsim <- 400
pval <- numeric(nsim)
power.b <- length(bVec)

for (j in 1:length(bVec)){
  b <- bVec[j]
  for (i in 1:nsim){
    y_det <- a + b * x
    y <- rnorm(N, mean = y_det, sd = sd)
    m <- lm(y ~ x)
    pval[i] <- coef(summary(m))["x", "Pr(>|t|)"]
  }
  power.b[j]<- mean(pval<0.05)
}

plot(bVec, power.b)
plot(bVec, power.b, col="red", add=T)
