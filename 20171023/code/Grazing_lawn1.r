# Garrett Arnold Kruger grazing lawn data

library(nlme)
gl <- read.csv("../data/grazing_lawn2.csv")
# Variables were transformed prior to analysis
# Bulk mass, LS ratio, N, and P were log-transformed
# N
N.lme <- lme(fixed = N ~ In_out, random =  ~ 1 | Lawn/Plot, data=gl)
anova(N.lme)
plot(N.lme)
qqnorm(N.lme,~ resid(.)|Lawn)
summary(N.lme)
# Grouped data
gl.grouped <- groupedData(N ~ In_out | Lawn/Plot, data = gl)
plot(gl.grouped)

# A simpler approach, using a paired T-test
gl.paired <- read.csv("grazing_lawn_paired.csv")
t.test(gl.paired$N.in, gl.paired$N.out, paired = TRUE)