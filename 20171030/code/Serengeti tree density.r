library(lme4)
library(dplyr)
library(ggplot2)
# Analysis of Serengeti tree density across 36 0.1-ha plots
dens <- read.csv("../data/Density.csv")
dens09 <- subset(dens, Year>2009) # Keep only 2010-2012
# Response variable: N = number of trees in a 0.1-ha plot
# Fixed effects: Year = year from 2009 t0 2012; MAP = Mean Annual Precip in mm;
# Fire = burned (B) or not (C) in a given year;
# Class = tree distribution is bimodal (D) or not (S), ignore for now
dens09$t <- dens09$Year-2010 # Convert Year (2010-2012) into time (0-2)

# Model selection to find model that explains number of trees (N)

##### viz ####
ggplot(data=dens09, aes(x=MAP, y=N))+
  geom_point(aes(color=Site))+
  facet_wrap(~t)

ggplot(data=dens09, aes(x=MAP, y=N))+
  geom_point(aes(color=Site))+
  facet_wrap(~Fire)

ggplot(data=dens09, aes(x=Fire, y=N))+
  geom_boxplot()+
  facet_wrap(~Site)
# fire seems to be dependent on site
ggplot(data=dens09, aes(x=t, y=N))+
  geom_point()+
  facet_wrap(~Site)

ggplot(data=dens09, aes(x=Fire, y=N))+
  geom_boxplot()+
  facet_wrap(~t)
#fire does not change over year (no interaction)

##### variable transformation ####

dens09$MAP.resc <- (dens09$MAP - mean(dens09$MAP))/sd(dens09$MAP)

#### models ####
#null models w and w/o random effects (use poisson dist.)
mod0 <- glm(N~1, data = dens09, family = poisson)
mod0 <- glmer(N~1 + (1|Site/Plot), 
                 data = dens09,
                 family = poisson) #lots of support for random effects

mod1 <- glmer(N ~ Fire + (1|Site/Plot),
              family = poisson, 
              data = dens09)

mod2 <- glmer(N ~ t + (1|Site/Plot),
              family = poisson, 
              data = dens09)

mod3 <- glmer(N ~ t + Fire + (1|Site/Plot),
              family = poisson, 
              data = dens09)

mod4 <- glmer(N ~ MAP.resc + (1|Site/Plot),
              family = poisson,
              data = dens09)

mod5 <- glmer(N ~ MAP.resc + t + (1|Site/Plot),
              family = poisson,
              data = dens09)

mod5b <- glmer(N ~ t + MAP.resc + (t|Site/Plot),
               family = poisson,
               data = dens09)

AIC(mod0, mod1, mod2, mod3, mod4, mod5)
anova(mod0, mod1, mod2, mod3, mod4, mod5)

