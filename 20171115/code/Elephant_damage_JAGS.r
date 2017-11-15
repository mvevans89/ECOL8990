# Elephant tree mortality analysis - JAGS version
# PLOT, SITE and SPECIES are random effects
library(rjags)
rm(list=(ls()))

ele.dat <- read.csv("../data/ELE_DAM_09-14_final_RMH.csv")  
ele.dat <- ele.dat[is.na(ele.dat$ALIVE2009)==FALSE,] #INCLUDE ONLY TREES ALIVE IN 2009
N <- nrow(ele.dat)

# Models originally fit with GLM

# Model1 ALIVE2013 ~ MAP + EMEAN + FFREQ + MAP*EMEAN + MAP*FFREQ + EMEAN*FFREQ
# Model2: ALIVE2013 ~ MAP
# Model3: ALIVE2013 ~ EMEAN
# Model4: ALIVE2013 ~ EMAX
# Model5: ALIVE2013 ~ FFREQ
# Model6: ALIVE2013 ~ FFREQ + EMEAN + FFREQ*EMEAN
# Model7: ALIVE2013 ~ FFREQ + EMAX + FFREQ*EMAX
# Model8: ALIVE2013 ~ FFREQ + MAP + EMAX + FFREQ*EMAX + MAP*EMAX
# Model9: ALIVE2013 ~ FFREQ + EMAX + FFREQ*EMAX + HT09
# Model10: ALIVE2013 ~ EMEAN + HT09 - Note: added post-hoc

n.adapt=10000
n.update=50000
n.iter=10000

# Model 3
inits3 <- list(list(beta0 = 3.5, beta1 = -0.4, tau.site = 5.6, tau.plot = 3.2, tau.species = 0.8),
               list(beta0 = 3.9, beta1 = -0.6, tau.site = 4.8, tau.plot = 3.8, tau.species = 1.2),
               list(beta0 = 3.0, beta1 = -0.2, tau.site = 6.0, tau.plot = 2.8, tau.species = 1))

data3 <- list(N = nrow(ele.dat), Y = ele.dat$ALIVE2014, EMEAN = ele.dat$EMEAN, SITE = as.numeric(ele.dat$SITE),
              PLOT = ele.dat$PLOTID, SPECIES = as.numeric(ele.dat$SPECIES))

jm3 <- jags.model("Model3.R", data=data3, inits3, n.chains=length(inits3), n.adapt=n.adapt)
update(jm3, n.iter=n.update)
zm3 <- coda.samples(jm3, variable.names = c("beta0", "beta1", "sigma.site", "sigma.plot", "sigma.species"),
                     n.iter=n.iter, n.thin=10)
summary(zm3)
plot(zm3)