# Simple regression example

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