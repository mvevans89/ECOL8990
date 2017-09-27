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

###---Writing our own likelihood functions

library(emdbook)
library(bbmle)

#define functions
mod1 <- function(c){-sum(dpois(seeds$Recruits, lambda=c, log=T))}
mod2 <- function(a){-sum(dpois(seeds$Recruits, lambda=a*seeds$Basal, log=T))}
mod3 <- function(a,c){-sum(dpois(seeds$Recruits, lambda=(c+a*seeds$Basal), log=T))}
mod4 <- function(a,b){-sum(dpois(seeds$Recruits, lambda=(a*seeds$Basal^b), log=T))}

p1 <- mle2(mod1, start=list(c=10))
p2 <- mle2(mod2, start=list(a=1))
p3 <- mle2(mod3, start=list(a=1,c=10)) #throws an error becuase gives negative values for lambda
p4 <- mle2(mod4, start=list(a=1,b=1))

print(AICtab(p1,p2,p3,p4, weights=T))

#fix p3 error by using a log link function so that we can take the log of the lambda
#ie an exponential of a negative number is positive, so we can always take the log
mod1 <- function(c){-sum(dpois(seeds$Recruits, lambda=exp(c), log=T))}
mod2 <- function(a){-sum(dpois(seeds$Recruits, lambda=exp(a*seeds$Basal), log=T))}
mod3 <- function(a,c){-sum(dpois(seeds$Recruits, lambda=exp(c+a*seeds$Basal), log=T))}
mod4 <- function(a,b){-sum(dpois(seeds$Recruits, lambda=exp(a*seeds$Basal^b), log=T))}

p1 <- mle2(mod1, start=list(c=10))
p2 <- mle2(mod2, start=list(a=1))
p3 <- mle2(mod3, start=list(a=1,c=10)) 
p4 <- mle2(mod4, start=list(a=1,b=0.5))

print(AICtab(p1,p2,p3,p4, weights=T)) #model 4 fits even better than before

summary(p4)

#try a new nonlinear model
mod5 <- function(a,b){-sum(dpois(seeds$Recruits, lambda=exp(a*seeds$Basal+(b*seeds$Basal^2)), log=T))}
p5 <- mle2(mod5, start=list(a=1,b=-0.2))

print(AICtab(p1,p2,p3,p4,p5, weights=T))

