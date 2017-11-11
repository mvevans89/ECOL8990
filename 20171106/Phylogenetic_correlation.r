library(nlme)
library(ape)
library(MASS)
set.seed(1234)
tr <- rtree(16) # Generate a random phylogenetic tree with 16 tips
tr <- as.phylo(tr) # Make it of class phylo
plot(tr) # Plot it
b0 <- 0  ## intercept
b1 <- 0  ## slope
s <- 2  ## standard deviation
V <- vcv(tr) # Calculate the variance-covariance matrix of the tree
x <- mvrnorm(1, mu = rep(0,16), Sigma = V)  ## x data
y <- mvrnorm(1, mu = b0 + b1 * x, Sigma = s*V)  ## simulate autocorrelated y data
plot(x, y)
## Ols regression
ols.reg <- lm(y ~ x)
summary(ols.reg)
## Incorporate phylogenetic structure
gls.reg <- gls(y ~ x, correlation = corBrownian(phy=tr))
summary(gls.reg)
