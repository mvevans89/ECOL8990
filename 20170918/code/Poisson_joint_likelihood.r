# Poisson likelihood for three values
# Make a vector lambda
lambda <- seq(0.1,10,0.1)
like.2 <- dpois(3,lambda)
like.4 <- dpois(4,lambda)
like.5 <- dpois(4,lambda)
like.all <- like.2*like.4*like.5
# Plot joint Likelihood for the three values
plot(lambda,like.all,type="l",col="blue",ylab="Likelihood")
# Plot log Likelihood for the three values
logL <- log(like.all)
plot(lambda,logL,type="l",col="blue",ylab="log L")
# Plot neg log Likelihood for the three values
plot(lambda,-logL,type="l",col="blue",ylab="Neg log L")

# Let's plot the relevant part of the figure...
plot(lambda,-logL,type="l",col="blue",ylab="Neg log L",xlim=c(0,10),ylim=c(5,10))

# What value of lambda minimizes the negative logL?
# We can use this function to explore this

nll.pois <- function(lambda){
  -log(dpois(2,lambda)*dpois(4,lambda)*dpois(5,lambda))
}

