library(lme4)
library(lattice)
library(MuMIn)

# Loda infestation data
infes <- read.csv("../data/Infestation.csv")

# Plot data
xyplot(infes$INF/(infes$INF+infes$NINF) ~ infes$MAP | infes$SEASON,
       group=infes$SPECIES,auto.key=list(space="right"))

# GLM Models
mod0 <- glm(cbind(INF,NINF) ~ 1, family=binomial, data=infes)
mod1 <- glm(cbind(INF,NINF) ~ MAP, family=binomial, data=infes)
mod2 <- glm(cbind(INF,NINF) ~ MAP + SPECIES, family=binomial, data=infes)
mod3 <- glm(cbind(INF,NINF) ~ MAP + SPECIES + SEASON, family=binomial, data=infes)
mod4 <- glm(cbind(INF,NINF) ~ MAP * SPECIES, family=binomial, data=infes)
mod5 <- glm(cbind(INF,NINF) ~ SPECIES * SEASON, family=binomial, data=infes)
mod6 <- glm(cbind(INF,NINF) ~ MAP + SPECIES + SEASON + MAP * SPECIES + SEASON * SPECIES,
            family=binomial, data=infes)
AIC(mod0,mod1,mod2,mod3,mod4,mod5,mod6)
# Models are highly overdispersed!

mod6q <- glm(cbind(INF,NINF) ~ MAP + SPECIES + SEASON + MAP * SPECIES + SEASON * SPECIES,
            family=quasibinomial, data=infes)

# Function to extract overdispersion parameter from glm (from Ben Bolker):
# https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf
# This function gets the c^hat (VIF) parameter needed for a QAIC (quasi-AIC)
dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}

# Example
dfun(mod1)
# We can get a QAIC value from a glm model as follows
mod0.qaic <- QAIC(object=mod0, chat=dfun(mod0))
mod1.qaic <- QAIC(object=mod1, chat=dfun(mod1))
mod2.qaic <- QAIC(object=mod2, chat=dfun(mod2))
mod3.qaic <- QAIC(object=mod3, chat=dfun(mod3)) #best qaic
mod4.qaic <- QAIC(object=mod4, chat=dfun(mod4))
mod5.qaic <- QAIC(object=mod5, chat=dfun(mod5))
mod6.qaic <- QAIC(object=mod6, chat=dfun(mod6))
View(c(mod0.qaic, mod1.qaic, mod2.qaic, mod3.qaic, mod4.qaic, mod5.qaic, mod6.qaic))
