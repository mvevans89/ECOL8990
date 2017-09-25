library(dplyr)
library(broom)
library(tidyr)
library(MuMIn)
library(boot)

seeds <- read.csv("../data/Brsp_seedl.csv")
#plot data
plot(seeds$Recruits, seeds$D40)
plot(seeds$Recruits, seeds$Basal)

mod1 <- glm(Recruits~1, data=seeds, family="poisson")
mod2 <- glm(Recruits~Basal, data=seeds, family="poisson")
mod3 <- glm(Recruits~Basal + D40, data=seeds, family="poisson")

AIC(mod1, mod2, mod3)
AICc(mod1, mod2, mod3) #for n/k < 40
Weights(AICc(mod1,mod2,mod3))


confint(mod1)
confint(mod2)
confint(mod3)

modCompare <- data.frame(do.call(rbind,(lapply(list(mod1, mod2,mod3),glance))))

#Model diagnostics
## Can't just use plot for glm objects
plot(mod2) #you can, but not interpretable
#better to use bootstrapping
glm.diag.plots(mod2) #basically the same as the DHARMA package

# Selection vs hypothesis testing
anova(mod1,mod2, mod3, test="Chisq")
# Confidence Intervals
confint(mod3) #note that this is only univariate

#when using link functions, make sure to specify response in predict function
exp(1.088 + 0.35*seeds$Basal)
predict(mod2)
predict(mod2, type="response")
exp(predict(mod2))
#compare raw with predictions
plot(predict(mod2, type="response"), seeds$Recruits)
abline(a=0, b=1, lty=2)
