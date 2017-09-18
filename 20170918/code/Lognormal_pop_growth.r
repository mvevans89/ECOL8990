Time <- 100
t <- seq(from=1,to=Time)
N <- numeric(Time) # vector to hold 50 years of population data
set.seed(101) # Seed random number generator
r <- rnorm(Time,mean=1.05,sd=0.1) # population growth rate
sigma <- 75 # process error
N[1] <- 100 # year 1 population
for (i in 2:Time){
  N[i] <- N[i-1]*r[i-1] # population dynamics loop
}
par(mfrow=c(2,2))
hist(r)
plot(t,N,type="l",col="red") # population dynamics plot
hist(N) # histogram of untransformed data
hist(log(N)) # histogram of log-transformed data
par(mfrow=c(1,1))