rm(list=(ls()))
library(emdbook)
library(bbmle)
# Get Myxomatosis data
data(MyxoTiter_sum)
myxdat <- subset(MyxoTiter_sum, grade==1)
# Some data visualization
hist(myxdat$titer)
plot(myxdat$day, myxdat$titer)
# Non linear, non-normal
# We can fit titer to day using a Ricker model, assuming a Gamma error

# Reminder: what does a Gamma distribution look like?
x <- seq(0, 10)
y1 <- dgamma(x, shape = 1, scale = 1)
y2 <- dgamma(x, shape = 5, scale = 1)
plot(x, y1, type = 'l')
lines(x, y2, col = 'red')

mle2(titer ~ dgamma(shape, scale = a * day * exp(-b * day) / shape),
     start = list(a = 1, b = 0.2, shape = 50),
     data = list(day = myxdat$day, titer = myxdat$titer),
     method = "Nelder-Mead")

# Now a Bayesian solution
library(rjags)

# Data
data <- 

# Initial values
inits <- 

n.adapt=10000
n.update=50000
n.iter=10000

# Run model in JAGS
