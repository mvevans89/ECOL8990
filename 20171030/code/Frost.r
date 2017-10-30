library(lme4)
library(dplyr)
frost <- read.csv("../data/Frost.csv")
plot(frost$DIAM, frost$FROST)
plot(frost$PLOT, frost$FROST)
frost.mean <- tapply(frost$FROST, frost$PLOT,mean)
barplot(frost.mean)
model1 <- glmer(FROST ~ DIAM + (1|PLOT), family=binomial, data=frost)

#adjust scaling of crown
frost$CROWN.resc <- (frost$CROWN - mean(frost$CROWN))/ sd(frost$CROWN)

#compare some models
model2 <- glmer(FROST ~ DIAM + CROWN.resc + (1|PLOT),
                family = binomial,
                data = frost)
model0 <- glmer(FROST ~ 1 + (1|PLOT),
                family = binomial,
                data = frost)
model3 <- glmer(FROST ~ DIAM + BASAL + (1|PLOT),
                family = binomial,
                data = frost)
model4 <- glmer(FROST ~ DIAM + BASAL + CROWN.resc + (1|PLOT),
                family = binomial,
                data = frost)

model5 <- glmer(FROST ~ DIAM * BASAL * CROWN.resc + (1|PLOT),
                family = binomial,
                data = frost)

MuMIn::Weights(AIC(model0, model1, model2, model3, model4, model5))

mod.glm <- glm(FROST ~ DIAM, data = frost, family = binomial)

uniqueTest <- frost %>%
  select(PLOT, CROWN) %>%
  distinct()
