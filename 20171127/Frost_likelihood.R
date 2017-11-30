library(lme4)
frost <- read.csv("Frost1.csv")
model1 <- glmer(cbind(FROST, 1 - FROST) ~ DIAM + (1|PLOT), family=binomial, data=frost)
# Solve this problem in a Bayesian framework using JAGS, using the ML estimates as initial values

# Convert PLOT to numeric variable first
frost$PLOTNUM <- as.numeric(frost$PLOT)
# Use a Bernoulli distribution for FROST -> dbern
# Use a logit link function
# Tip, this is built into JAGS: e.g., logit(p) <- beta0 + beta1 * X[i]
# Search online for tips on how to fit a logistic regression in JAGS

library(rjags)

n.adapt=10000
n.update=50000
n.iter=10000

inits <- list(list(beta0 = 2.06, beta1 = -0.6, tau.plot = 1))

data <- list(N = nrow(frost), Y = frost$FROST, DIAM = frost$DIAM, PLOTNUM = frost$PLOTNUM)

jm <- jags.model("Frost_model.R", data = data, inits, n.chains = length(inits), 
                 n.adapt=n.adapt)
update(jm, n.iter=n.update)
zm <- coda.samples(jm, variable.names = c("beta0", "beta1", "sigma.plot"),
                    n.iter=n.iter, n.thin=10)
summary(zm)
plot(zm)
