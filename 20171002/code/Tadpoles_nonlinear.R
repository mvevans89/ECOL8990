library(emdbook)
tad <- read.csv('Tadpoles.csv')

# A null model
binomNLL1 <- function(c){
  -sum(dbinom(tad$Killed, prob = c, size = tad$Initial, log=TRUE))
}
m1 <- mle2(binomNLL1, start = list(c = 0.5))

# Type II functional response
binomNLL2 <- function(a, h){-sum(dbinom(tad$Killed, prob = a / (1 + a * h * tad$Initial), 
                                       size = tad$Initial, log=TRUE))}

m2 <- mle2(binomNLL2, start = list(a = 0.5,h = 0.0125))

# Linear dependence of p on Initial
binomNLL3 <- function(a, c){-sum(dbinom(tad$Killed, prob = a * tad$Initial + c, 
                                       size = tad$Initial, log=TRUE))}

m3 <- mle2(binomNLL3, start = list(a = -0.002, c = 0.48))

AICtab(m1, m2, m3, weights = TRUE)

# Compare m3 with glm solution
m3.glm <- glm(cbind(Killed, Initial - Killed) ~ Initial, tad, family=binomial)
summary(m3.glm)

# Model 3 mle2 solved with link function
binomNLL3.logit <- function(a, c){-sum(dbinom(tad$Killed,
                    prob = exp(a * tad$Initial + c) / (1 + exp(a * tad$Initial + c)), 
                    size = tad$Initial, log=TRUE))}

m3.logit <- mle2(binomNLL3.logit, start = list(a = -0.009, c = -0.1))