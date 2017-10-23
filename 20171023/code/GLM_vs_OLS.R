set.seed(135)
N <- 100
x <- runif(10,0,10)
eta <- -2 + x * 0.2 + rnorm(10, mean = 0, sd = 1) # Linear predictor
p.det <- exp(eta) / (1 + exp(eta))
plot(x, p.det)
y <- rbinom(10, prob = p.det, size = N)
plot(x, y/N)
# Analyze with glm
mod.binom <- glm(cbind(y, N - y) ~ x, family = binomial)
print(summary(mod.binom))
# Use lm instead
prop <- y / N
mod.ols <- lm(prop ~ x)
print(summary(mod.ols))
