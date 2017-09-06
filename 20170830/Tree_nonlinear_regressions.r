# Comparing nonlinear regression models for tree height data

# Input tree height data into dataframe H
H <- read.csv("data/height1.csv")
# Extract species "buaf"
buaf <- subset(H, Sp=="BUAF")
# Plot height vs. diameter
plot(buaf$Diameter, buaf$Height, xlab="Diam (cm)", ylab="Ht (m)", col="red")
# Model 1: a Power law
mod1 <- nls(Height ~ c * Diameter^d, data = buaf, start = list(c=0.36, d=0.8))
# Model 2: a linear relationship
mod2 <- nls(Height ~ c * Diameter, buaf, start = list(c=0.36)) #use nls for easy comparisons
# Which model fits better?
anova(mod1,mod2)
# Do we need an intercept?
mod3 <- nls(Height ~ c * Diameter^d + e, buaf, start = list(c=0.36, d=0.8, e=0.1))
anova(mod1,mod3)
# Model 4: a Michaelis Menton
mod4 <- nls(Height ~ a * Diameter / (b + Diameter), buaf, start = list(a=14, b=15))
# Does it fit better than model 3?
anova(mod3, mod4)
# can't use anova to test because they're not nested. can't use a least-squares framework

# What do the MM parameters tell us?
# Exercise: fit a Ricker model to the data
mod5 <- nls(Height~a*Diameter*exp(-b*Diameter), buaf, start=list(a=0.4, b=0.02))
summary(mod5)
# can you compare this to models above?
anova(mod5, mod1) #nope
