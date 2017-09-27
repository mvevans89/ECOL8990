library(emdbook)
tad <- read.csv('../data/Tadpoles1.csv')

#solve with glm
glm1 <- glm(cbind(Killed, Initial-Killed)~1, data=tad, family=binomial)

# A null model
binomNLL1 <- function(c){
  -sum(dbinom(tad$Killed, prob =c, size = tad$Initial, log=TRUE))
}

# Solve it
m1 <- mle2(binomNLL1, start = list(c = 0.5)) #this works, but we need to constrain c between 0 and 1

#logit function
binomNLL1 <- function(c){
  -sum(dbinom(tad$Killed, prob = exp(c)/(1+exp(c)), size = tad$Initial, log=TRUE))
}

# Type II functional response

binomNLL2 <- function(a,h){
  -sum(dbinom(tad$Killed, prob = a / (1 + a * h * tad$Initial), 
                                       size = tad$Initial, log=TRUE))
  }

m2 <- mle2(binomNLL2, start = list(a = 0.5,h = 0.0125))

#technically this needs a link function (we are just lucky that the prob is remaining between 0 and 1)

binomNLL2 <- function(a,h){
  -sum(dbinom(tad$Killed, prob = exp(a / (1 + a * h * tad$Initial))/(1+exp(a / (1 + a * h * tad$Initial))), 
              size = tad$Initial, log=TRUE))
}

m2 <- mle2(binomNLL2, start = list(a = 0.5,h = 0.0125))
m2 <- mle2(binomNLL2, start = list(a = -12,h = 200))
m2
