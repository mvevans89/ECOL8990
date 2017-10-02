library(emdbook)
library(bbmle)
seed <- read.csv("Brsp_seedl.csv")

# define likelihood functions
poisNLL1 <- function(c, Recruits){-sum(dpois(Recruits,
                lambda = exp(c),log = TRUE))}
poisNLL2 <- function(a, Recruits, Basal){-sum(dpois(Recruits,
                lambda = exp(a * Basal), log = TRUE))}
poisNLL3 <- function(a, c, Recruits, Basal){-sum(dpois(Recruits,
                lambda = exp(a * Basal + c), log = TRUE))}
poisNLL4 <- function(a, b, Recruits, Basal){-sum(dpois(Recruits,
                lambda = exp(a * Basal ^ b), log = TRUE))}
# Part 1: Make a new nonlinear nll function with a Michaelis Menten relationship


# solve likelihood functions
p1 <- mle2(poisNLL1, start = list(c = 10), data = seed)
p2 <- mle2(poisNLL2, start = list(a = 1), data = seed)
p3 <- mle2(poisNLL3, start = list(a = 1, c = 10), data = seed)
p4 <- mle2(poisNLL4, start = list(a = 5, b = 0.5), data = seed)
# Part 2: Solve for the MLEs
p5 <- 

# get delta AIC values and AIC weights
aic <- AICtab(p1, p2 , p3, p4, p5, weights = TRUE)
print(aic)

# Part 3: Recalculate Akaike weights for the top models
w4 <- 
w5 <- 

# New data for which we need predictions
Newbas <- c(1.3,5.2,4.3,6.7,7.2,0.9,2.3,13) # New data

# Part 4: Predict Recruits from a new set of plots
pred4 <- 
pred5 <-
# Part 5: Obtain weighted predictions 
pred.weight <- 
# Plot them all together to compare
plot(Newbas, pred4, col="red", pch=16, cex=1.5)
points(Newbas, pred5, col="blue", pch=16, cex=1.5)
points(Newbas, pred.weight, col="green", pch=16, cex=1.5)