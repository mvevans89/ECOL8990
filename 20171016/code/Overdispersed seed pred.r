# Overdispersed seed predation events
set.seed(1001)
# First, all 1000 sites are equally likely to be visited
x <- rbinom(1000,prob=0.5,size=5)
f <- factor(x,levels=0:5)
plot(f)
# Now, what if some sites are more likely to be visited than others?
x <- rbinom(1000,prob=runif(1000,0.2,0.8),size=5)
f <- factor(x,levels=0:5)
plot(f)

mean(x)
var(x) #doesn't fit binomial