library(lme4)
library(lattice)
library(MuMIn)

# Loda infestation data
infes <- read.csv("Infestation.csv")

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

# Function to extract overdispersion parameter from glm (from Ben Bolker):
# https://cran.r-project.org/web/packages/bbmle/vignettes/quasi.pdf
dfun <- function(object) {
  with(object,sum((weights * residuals^2)[weights > 0])/df.residual)
}

# Example
dfun(mod1)
# We can get a QAIC value from a glm model as follows
mod1.qaic <- QAIC(object=mod1, chat=dfun(mod1))

# Calculate QAIC values using dispersion parameters
Model <- c("mod0","mod1","mod2","mod3","mod4","mod5","mod6")
df <- data.frame(Model)
df$QAIC <- NA
df$QAIC[1] <- QAIC(object=mod0, chat=dfun(mod0))
df$QAIC[2] <- QAIC(object=mod1, chat=dfun(mod1))
df$QAIC[3] <- QAIC(object=mod2, chat=dfun(mod2))
df$QAIC[4] <- QAIC(object=mod3, chat=dfun(mod3))
df$QAIC[5] <- QAIC(object=mod4, chat=dfun(mod4))
df$QAIC[6] <- QAIC(object=mod5, chat=dfun(mod5))
df$QAIC[7] <- QAIC(object=mod6, chat=dfun(mod6))
print(df)

library(nlme)
# The problem: the bionomial is WAY overdispersed
infes$p <- infes$INF/(infes$INF + infes$NINF)
infes$logit.p <- log((infes$p+0.01)/(1-(infes$p+0.01)))
# GLM Models
mod0 <- lme(logit.p ~ 1, data=infes, random = ~ 1 | SITE, method = "ML")
mod1 <- lme(logit.p ~ MAP, data=infes, random = ~ 1 | SITE, method = "ML")
mod2 <- lme(logit.p ~ MAP + SPECIES, data=infes, random = ~ 1 | SITE, method = "ML")
mod3 <- lme(logit.p ~ MAP + SPECIES + SEASON, data=infes, random = ~ 1 | SITE, method = "ML")
mod4 <- lme(logit.p ~ MAP * SPECIES, data=infes, random = ~ 1 | SITE, method = "ML")
mod5 <- lme(logit.p ~ SPECIES * SEASON, data=infes, random = ~ 1 | SITE, method = "ML")
mod6 <- lme(logit.p ~ MAP + SPECIES + SEASON + MAP * SPECIES + SEASON * SPECIES, data=infes, 
            random = ~ 1 | SITE, method = "ML")
AIC(mod0,mod1,mod2,mod3,mod4,mod5,mod6)
