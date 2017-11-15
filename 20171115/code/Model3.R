# Model3: ALIVE2013 ~ EMEAN

model{
  #priors
  beta0 ~ dnorm(0, 0.001)
  beta1 ~ dnorm(0, 0.001)
  tau.site ~ dgamma(0.5, 0.5)
  tau.plot ~ dgamma(0.5, 0.5)
  tau.species ~ dgamma(0.5, 0.5)
  sigma.site <- 1/sqrt(tau.site)
  sigma.plot <- 1/sqrt(tau.plot)
  sigma.species <- 1/sqrt(tau.species)
  for (s in 1:8){
    # Site prior
    b.site[s] ~ dnorm(0.0,tau.site)
    # Priors for plot effects within site
    for (k in 1:4){
      b.plot[s,k] ~ dnorm(0.0,tau.plot)
    }
  }
  for (j in 1:17){
    b.species[j] ~ dnorm(0.0,tau.species)
  }
  #likelihood
  for (i in 1:N){
    Y[i] ~ dbern(p[i])
    p[i] <- exp(z[i]) / (1 + exp(z[i]))
    z[i] <- beta0 + beta1 * EMEAN[i] + b.site[SITE[i]] + b.plot[SITE[i],PLOT[i]] + b.species[SPECIES[i]]
  }
}