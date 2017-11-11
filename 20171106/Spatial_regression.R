set.seed(1001)
a <- 1  ## intercept
b <- 1  ## slope
s <- 0.2  ## variance
L <- 3    ## scale factor for spatial autocorrelation
x <- runif(100)  ## x-locations
y <- runif(100)  ## y-locations
c <- runif(100)  ## covariate values
dist <- sqrt((outer(x, x , "-")) ^ 2 + (outer(y, y, "-") ^ 2))  ## distance matrix between observations
library(MASS)
corr <- exp(-L*dist)  ## correlation matrix from distance matrix
V <- s*corr           ## variance-covariance matrix
d <- mvrnorm(1, mu = a + b * c, Sigma = V)  ## simulate autocorrelated values
plot(c, d)    ## covariate vs. response
ols <- lm(d ~ c)   ## fit OLS model
fr <- cut(resid(ols), breaks=100)  ## categories of magnitude of residuals
plot(x, y, pch = 21, bg=gray((1:100)/100)[fr])  ## plot residuals across space

## we can also do an OLS rgeression with Generalized Least Squares, assuming no spatial interdependence
## this allows us to plot a variogram based on the residuals
library(nlme)
gls1 <- gls(d ~ c) 
plot(Variogram(gls1, form = ~ x + y))

## We imposed the autocorrelation - what should it look like?
distance <- seq(from=0, to=1, by=0.1)
correl <- exp(-L * distance)
plot(distance, correl, type="l")
                
## Is the autocorrelation significant?
library(pgirmess)
res <- residuals(gls1)
I <- correlog(coords = cbind(x, y), z = res, method = "Moran")
Idf <- data.frame(I)
plot(Idf$dist.class, Idf$coef, type='l', xlab='Lag distance', ylab='Moran I')
lines(c(0,1.2), c(0,0), lty='dashed')

## Now let's repeat the regression, but assume that there is autocorrelation
gls2 <- gls(d~c,correlation=corExp(1,form=~x+y))
plot(Variogram(gls2, resType = "n", form=~x+y))
