library(rjags)

# Data
data <- list(Y = c(42, 43, 58, 70, 47, 51, 85, 63, 58, 46))

# Initial value value
inits <- list(list(m = 55, tau = 0.01),
              list(m = 50, tau = 0.004),
              list(m = 60, tau = 0.005)) # Semi-arbitrary initial values

n.adapt=1000 #burn-in
n.update=5000 #number of times you'll update
n.iter=1000 #iterations you'll sample 

# Run model in JAGS
jmod <- jags.model("Tree_model.R", data = data, inits, n.chains = length(inits), n.adapt=n.adapt)

# Generate samples of the posterior distribution
post <- coda.samples(jmod, variable.names = c('m', 'tau'), 
                     n.iter = n.iter, n.thin = 10) #thinning is basically stratified sampling
summary(post)
plot(post)
