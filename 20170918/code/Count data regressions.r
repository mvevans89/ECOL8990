seed <- read.csv("../data/Brsp_seedl.csv")
plot(seed$Basal, seed$Recruits)
# Is there a relationship between basal area and new seedlings (Recruits)?
# Normal regression with no transformation
norm.mod <- lm(Recruits ~ Basal, seed)
summary(norm.mod)
plot(norm.mod)
# Normal regression with arcsin sqrt and logit transformations
sqrt.mod <- lm(sqrt(Recruits) ~ Basal, seed)
summary(sqrt.mod)
plot(sqrt.mod)

log.mod <- lm(log(Recruits + 1) ~ Basal, seed)
summary(log.mod)
plot(sqrt.mod)

# transforming didn't help. It may be that we need to try a new error distribution\

# Now let's try a Poisson regression
