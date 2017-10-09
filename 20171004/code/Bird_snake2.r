# Load data and solve the model
set.seed(101) # So we all get the same results
birds <- read.csv("Nest_predation.csv")
# Solve the model
mod.binom <- glm(cbind(pred,nests-pred)~snakes,data=birds,family=binomial)
# Extract the coefficients
coef <- mod.binom$coefficients
beta0 <- coef[1]
beta1 <- coef[2]
# As far as you know, these best describe the relationship between snakes and nest predation
# Generate simulated data with these
# Choose a range of sampling efforts you might try
N.sites <- c(5,10,15,20,50)
# Make a place to store delat AIC values
delta.aic <- numeric(5)
for (i in 1:5){
  snakes <- runif(N.sites[i], 0, 6) # Simulate snake data
  nests <- rep(8, N.sites[i]) # Simulate nest number data
  # Make a linear predictor from the coeffcients you calculated
  eta <- beta0 + beta1 * snakes
  prop.pred <- exp(eta)/(1+exp(eta))
  # Make binomially distributed deviates
  pred <- rbinom(N.sites[i], prob=prop.pred, size=nests)
  # Fit an intercept model
  mod.binom.null <- glm(cbind(pred, nests-pred)~1, family=binomial)
  # Fit a model with snakes as a covariate
  mod.binom <- glm(cbind(pred, nests-pred) ~ snakes, family=binomial)
  # Compute the deltaAIC
  aic.null <- mod.binom.null$aic
  aic <- mod.binom$aic
  delta.aic[i] <- aic.null - aic
}
# Plot the results
plot(N.sites, delta.aic, type='l', xlab="Sampling effort", ylab="delta AIC")