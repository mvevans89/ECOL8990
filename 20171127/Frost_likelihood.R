library(lme4)
frost <- read.csv("Frost.csv")
model1 <- glmer(cbind(FROST, 1 - FROST) ~ DIAM + (1|PLOT), family=binomial, data=frost)
# Solve this problem in a Bayesian framework using JAGS, using the ML estimates as initial values

# Convert PLOT to numeric variable first
frost$PLOTNUM <- as.numeric(frost$PLOT)
# Use a Bernoulli distribution for FROST -> dbern
# Use a logit link function
# Tip, this is built into JAGS: e.g., logit(p) <- beta0 + beta1 * X[i]
# Search online for tips on how to fit a logistic regression in JAGS