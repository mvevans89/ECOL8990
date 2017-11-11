A <- matrix(c(2,1,3,1), nrow=, ncol=2)
B <- matrix(c(0.5,5,2,1), nrow=2, ncol=2)
C <- A%*%B
I <- matrix(c(1,0,0,1), nrow=2, ncol=2)

x<- c(0.1,0.3,0.42,0.6,0.9)
set.seed(1001)

y <- rnorm(5, mean=2*x+1, sd=0.2)
plot(x,y)

#create X vector
X <- rep(1,10)
dim(X) <- c(5,2)
X[,2] <- x

#create Y vector
Y <- y
dim(Y) <- c(5,1)
Y

#Solve OLS of  lm(y~x) using matrices

bHat <- solve(t(X) %*% X) %*% (t(X) %*% Y)
summary(lm(y~x))

#coefficients should match those in bHat
