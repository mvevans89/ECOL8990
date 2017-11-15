# Tree growth JAGS model

model
{
  m ~ dnorm(53, 0.04) # informative prior for mean
  #m ~ dnorm(0, 0.000001) # flat prior for mean
  tau ~ dgamma(0.001, 0.001)

  # Likelihood
  for (i in 1:10)
  {
    Y[i] ~ dnorm(m, tau) # diameter drawn from a normal distribution
    # note that tau is precision (1/sd)
  }
}