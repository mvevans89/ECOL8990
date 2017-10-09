x <- rep(1:20,5)
x<- 1:100
a <- 2
b <- 1
yDet <- a + b * x
# y <- rnorm(20, mean=yDet,  sd= 2) 
#or create error term
epsilon <- rnorm(100, mean=0, sd=5)
y <- yDet + epsilon
plot(x,y)
reg <- lm(y~x)
summary(reg)
abline(reg,col="blue")
abline(a=a, b=b, col="red")
