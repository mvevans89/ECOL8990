tad <- read.csv("../data/Tadpoles.csv")
tad$pkill <- tad$Killed / tad$Initial
plot(tad$Initial, tad$Killed)
plot(tad$Initial, tad$pkill)
# Normal regression with no transformation
norm.mod <- lm(pkill ~ Initial, data = tad)
summary(norm.mod)
plot(norm.mod) #qq plot is off
# Let's make some new variables
tad$pkill.asin <- asin(sqrt(tad$pkill))
tad$pkill.logit <- log(tad$pkill/(1-tad$pkill))
# Let's plot this
plot(tad$Initial, tad$pkill.logit)
plot(tad$Initial, tad$pkill.asin)
# Normal regression with arcsin sqrt and logit transformations
asin.mod <- lm(pkill.asin ~ Initial, data=tad)
summary(asin.mod)
plot(asin.mod)
logit.mod <- lm(pkill.logit ~ Initial, data=tad)
summary(logit.mod)
plot(logit.mod)
# Neither transformation really helps

# Now let's try a binomial regression
binom.mod1 <- glm(cbind(Killed, Initial - Killed) ~ Initial, data = tad, family = binomial)
# cbind to get the successes and failures of the experiment, this gives it more power than just a proportion
# not that default link function for binomial is logit
summary(binom.mod1)
plot(binom.mod1)


