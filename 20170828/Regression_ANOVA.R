# Simple regression example
library(tidyr)
library(broom)

# Input tree height data into dataframe H
H <- read.csv("height.csv")
# Extract species "buaf"
buaf <- subset(H,Sp=="BUAF")
# Plot height vs. diameter
plot(buaf$Diameter,buaf$Height,xlab="Diam (cm)",ylab="Ht (m)",col="red")
# Perform linear regression
reg <- lm(Height ~ Diameter, data=buaf)
# Shoe results of regression
summary(reg)
# Produce ANOVA table
anova(reg)
# Draw best-fit line
abline(reg)
# Get the residuals
d <- resid(reg)
# Look at their distribution
hist(d)
#Let's do some model diagnostics
plot(reg)
# A simple ANOVA
# Let's compare the height of species in dataframe H
# First, let's look at a plot
plot(H$Sp,H$Height)
# Now let's do an ANOVA
model <- lm(Height ~ Sp, data=H)
# Let's look at the results
summary(model)
# Maybe this is what you really want...
anova(model)

# EXERCISE: REDO THE REGRESSION AND ANOVA WITH LOG-TRANSFORMED VARIABLES

# Plot height vs. diameter
plot(log(buaf$Diameter),log(buaf$Height),xlab="Diam (cm)",ylab="Ht (m)",col="red")

logReg <- lm(log(Height) ~ log(Diameter), data=buaf)
summary(logReg)
plot(logReg)
rbind(glance(reg), glance(logReg))

## Get predictions
htPreds <- predict(reg, data=buaf)
plot(htPreds, buaf$Height)
abline(a=0, b=1, lty=2)

newData <- data.frame(Diameter=c(9,12,16,21,33,40))
newData$predHt<- predict(reg, newData)
plot(x=newData$Diameter, y=newData$predHt)
