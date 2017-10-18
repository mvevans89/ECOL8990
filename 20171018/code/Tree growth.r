tree <- read.csv("../data/Trees.csv") # This file contains the species and plot data
set.seed(51)
Y <- rep(0,32)
alpha.A <- 8 # species A effect
alpha.B <- 6 # species B effect
alpha.C <- 4.5 # species C effect
sigma.sq <- 2 # standard deviation of epsilon error term
sigma.sq.b <- 6
b <- rnorm(5,mean=0,sd=sigma.sq.b)
epsilon <- rnorm(32, mean=0, sd=sigma.sq) # generate error terms
for (k in 1:32){
  if (tree$Species[k]=="A") alpha <- alpha.A
  else if (tree$Species[k]=="B") alpha <- alpha.B
  else alpha <- alpha.C
  tree$Y[k] <- alpha + b[tree$Plot[k]] + epsilon[k] # Growth model
}
Plotfact <- factor(tree$Plot) # Tell R that Plot is a categorical variable
plot(tree$Species,tree$Y)
plot(Plotfact,tree$Y)
m1 <- lm(Y ~ Species, data = tree)
anova(m1)

#Now try a mixed model
library(nlme)
m2 <- lme(fixed = Y ~ Species, random = ~1|Plot, data = tree)
anova(m2)

qqnorm(m2, ~resid(.)|Plot, id = 0.05)
