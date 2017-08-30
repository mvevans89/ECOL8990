fr <- read.csv("data/TypeII_fr.csv")
plot(fr$Density, fr$Consumed) #matches type II response

## Linearize

y <- 1/fr$Consumed[fr$Density!=0] #wierd zeros, so forget them
x <- 1/fr$Density[fr$Density!=0]

mod <- lm(y~x)
summary(mod)
coef(mod)

a <- 1/coef(mod)[1] #calculate intercept
b <- a * coef(mod)[2] #calculate slope
