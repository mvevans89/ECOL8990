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
#note that this doesn't make biological sense, even though it fits well
# can you compare this to models above?
anova(mod5, mod1) #nope

## PREDICTIONS
# compare predictions for models 1,2, and 4
predictions <- rbind(buaf[,2:3], 
                     cbind(Diameter=0:35, Height=predict(mod1, data.frame(Diameter=0:35))), 
                     cbind(Diameter=0:35, Height=predict(mod2, data.frame(Diameter=0:35))), 
                     cbind(Diameter=0:35, Height=predict(mod4, data.frame(Diameter=0:35))))
predictions$model <- rep(c("Original", "mod1", "mod2" ,"mod4"), c(nrow(buaf), 36,36,36))


ggplot(data=predictions, aes(group=model))+
  geom_point(data=predictions[predictions$model=="Original",], aes(x=Diameter, y=Height), color="gray40")+
  theme_bw()+
  geom_line(data=predictions[predictions$model!="Original",], aes(x=Diameter, y=Height, color=model))
