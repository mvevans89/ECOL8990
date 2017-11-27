# model for Frost JAGS model

# Model3: ALIVE2013 ~ EMEAN

model{
  #priors
  beta0 ~ dnorm(0, 0.001)
  beta1 ~ dnorm(0, 0.001)
  tau.plot ~ dgamma(0.5, 0.5)
  sigma.plot <- 1/sqrt(tau.plot)
  for (s in 1:16){
    # Plotprior
    b.plot[s] ~ dnorm(0.0,tau.plot)
  }
  #likelihood
  for (i in 1:N){
    Y[i] ~ dbern(p[i])
    p[i] <- exp(z[i]) / (1 + exp(z[i]))
    z[i] <- beta0 + beta1 * DIAM[i] + b.plot[PLOTNUM[i]]
  }
}