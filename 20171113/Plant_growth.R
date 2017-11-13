library(nlme)
library(lattice)
plants <- read.csv("Plant_growth.csv", stringsAsFactors = F)
plants <- plants[order(plants$Bin, plants$Day), ]
xyplot(Crown ~ Day | Bin, data = plants, type='l')
xyplot(Crown ~ Day | Species, groups = TTT, data = plants)
# OLS model
mod1 <- lm(Crown ~ TTT * Day * Species, data = plants)
# Mixed model with random effect for individual (bin)
mod2 <- lme(Crown ~ TTT * Day * Species, random = ~ 1 | Bin, data = plants)
# Some model diagnostics
plot(mod2, resid(., type = "n") ~ Day | Bin)
plot(ACF(mod2, form = ~ Day, maxLag = 10, resType = 'n'), alpha = 0.01)
# Could bin (container) affect growth rate differently in each case?
# Mixed model with random slope for each Bin
mod3 <- lme(Crown ~ TTT * Day * Species, random = ~ Day | Bin, data = plants)
plot(ACF(mod3, form = ~ Day | Bin, maxLag = 10, resType = 'n'), alpha = 0.01)
# Is model 3 an improvement?
anova(mod2, mod3)
# There is residual temporal autocorrelation remaining
mod4 <- update(mod3, correlation = corCAR1(form = ~ Day | Bin))
plot(ACF(mod4, form = ~ Day | Bin, maxLag = 10, resType = 'n'), alpha = 0.01)
# Does the autoregressive model improve fit?
anova(mod3, mod4)
# How do the residuals look now?
plot(mod4, resid(., type = "n") ~ Day | Bin)