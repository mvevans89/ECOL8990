# Load data and solve the model
set.seed(1001) # So we all get the same results
birds <- read.csv("Nest_predation.csv")
# Solve the model
mod.binom <- glm(cbind(pred,nests-pred)~snakes,data=birds,family=binomial)
# Extract the coefficients
coef <- mod.binom$coefficients
beta0 <- coef[1]
beta1 <- coef[2]
# As far as you know, these best describe the relationship between snakes and nest predation
# Generate simulated data with these
N.sites <- 50 # Assume you have a max of 50 sites
snakes <- runif(N.sites, 0, 6) # Simulate snake data
nests <- rep(8, N.sites) # Simulate nest number data
# Make a linear predictor from the coefficients you calculated
eta <- beta0 + beta1 * snakes
prop.pred <- exp(eta) / (1 + exp(eta))
pred <- rbinom(N.sites, prob = prop.pred, size = nests)

# Plot the results for the 50 sites and do a regression