set.seed(135)
N <- 100
x <- runif(10,0,10)
df <- data.frame(x)
eta <- -2 + x * 0.2 + rnorm(10, mean = 0, sd = 0) # Linear predictor
p.det <- exp(eta) / (1 + exp(eta))
df$y <- rbinom(10, prob = p.det, size = N)
df$Site <- factor(1:10)
df$N <- N
with(df, plot(x, y/N))

# Analyze with glm
mod.binom <- glm(cbind(y, N - y) ~ x, family = binomial, data = df)
print(summary(mod.binom))

library(lme4)
glmm <- glmer(cbind(y, N - y) ~ x + (1|Site), df, family=binomial)
summary(glmm)