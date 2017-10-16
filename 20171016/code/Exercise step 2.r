# Load data and solve the model
set.seed(1001) # So we all get the same results)
seed <- read.csv("Brsp_seedl.csv",header=T)
# In the original data set, these are the coefficients of the Recruits ~ Basal regression
# Intercept -> 1.1
# Slope -> 0.35
# Remember that these are on a log link function scale
# How does the power of a likelihood ratio test vary with effect size of 0.1, 0.2, and 0.3?
beta0 <- 1.1
# Number of reps
Nreps <- 100
beta1 <- c(0.1,0.15,0.2,0.25,0.3)
M <- length(beta1)
# Make a place to store LRT values
lrt <- numeric(Nreps)
lrt.prop <- numeric(M)
for (i in 1:M){
  for (j in 1:Nreps){
    # Simulate basal area data
    bas <- runif(18,0,6)
    # Make a linear predictor from your coefficients
    eta <- beta0 + beta1[i]*bas
    Rec <- rpois(18, lambda=exp(eta))
    pois.null <- glm(Rec ~ 1, family=poisson)
    # Get the log Likelihood of the model
    ll.null <- logLik(pois.null)
    pois.bas <- glm(Rec ~ bas, family=poisson)
    ll.bas <- logLik(pois.bas)
    lrt[j] <- ll.bas - ll.null
  }
  lrt.prop[i] <- sum(lrt>1.92)/Nreps
}
plot(beta1,lrt.prop,type="l",col="red")