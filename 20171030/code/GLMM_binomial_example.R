# CBPP example from the built-in dataset in lme4

library(lme4)
# Get proportional incidence data for visualization
cbpp$prop <- cbpp$incidence / cbpp$size
# Plot
library(ggplot2)
ggplot(cbpp,aes(period,prop))+
  geom_point(alpha=0.5, aes(size=size)) +
  theme_classic()

# Fit models
cbpp.per <- glmer(cbind(incidence, size - incidence) ~ period + (1|herd),
             family = binomial, data = cbpp)
cbpp.null <- glmer(cbind(incidence, size - incidence) ~ 1 + (1|herd),
                   family = binomial, data = cbpp)
AIC(cbpp.per,cbpp.null)

glm.per <- glm(cbind(incidence, size - incidence) ~ period ,
               family = binomial, data = cbpp)

summary(cbpp.per)
summary(glm.per)

# Testing for overdispersion
# Approximate method: https://bbolker.github.io/mixedmodels-misc/glmmFAQ.html#testing-for-overdispersioncomputing-overdispersion-factor
overdisp_fun <- function(model) {
  ## number of variance parameters in 
  ##   an n-by-n variance-covariance matrix
  vpars <- function(m) {
    nrow(m)*(nrow(m)+1)/2
  }
  model.df <- sum(sapply(VarCorr(model),vpars))+length(fixef(model))
  rdf <- nrow(model.frame(model))-model.df
  rp <- residuals(model,type="pearson")
  Pearson.chisq <- sum(rp^2)
  prat <- Pearson.chisq/rdf
  pval <- pchisq(Pearson.chisq, df=rdf, lower.tail=FALSE)
  c(chisq=Pearson.chisq,ratio=prat,rdf=rdf,p=pval)
}
overdisp_fun(cbpp.per) #p-value suggests it is not overdispersed

## GLMM with individual-level variability (accounting for overdispersion)
## For this data set the model is the same as one allowing for a period:herd
## interaction, which the plot indicates could be needed.
cbpp$obs <- 1:nrow(cbpp) #create variable representing observation
cbpp.per.obs <- glmer(cbind(incidence, size - incidence) ~ period +
                (1 | herd) +  (1|obs),
              family = binomial, data = cbpp)
AIC(cbpp.null, cbpp.per, cbpp.per.obs)
