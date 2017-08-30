# Nonlinear least-squares regression with nls function
# Assumes that TypeII_FR.txt data is loaded and attached already
# a and b coefficients from linearization fit
a.lin <- 
b.lin <- 
fr <- read.csv("TypeII_FR.csv")
nl.fit <- nls(Consumed ~ a.nls * Density / (b.nls  +Density), data = fr, 
              start = list(a.nls = 14, b.nls = 9))
summary(nl.fit)
# a and b coefficients from nls fit
a.nls <- coef(nl.fit)[1]
b.nls <- coef(nl.fit)[2]
# Plot the fit against the data
plot(fr$Density, fr$Consumed)
# Generate pseudo density data
D <- seq(from=0, to=250) 
C.lin <- a.lin * D / (b.lin + D) # Consumed fit from linearization
C.nls <- a.nls * D / (b.nls + D) # Consumed fit from nls
lines(D, C.lin, col="blue")
lines(D, C.nls, col="red")