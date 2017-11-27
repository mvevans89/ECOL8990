# Analysis of Serengeti tree cover change
snp <- read.csv("SNP_TC_change.csv")
# Test for autocorrelation
library(nlme)
gls.MAP <- gls(MAP ~ 1, data=snp) 
gls.FIRE <- gls(FIRE ~ 1, data=snp)  
plot(Variogram(gls.MAP, form=~X+Y))
plot(Variogram(gls.FIRE, form=~X+Y))
## Is the autocorrelation significant?
library(pgirmess)
res.MAP <- residuals(gls.MAP)
I.MAP <- correlog(coords = snp[,2:3], z = res.MAP, method = "Moran")
res.FIRE <- residuals(gls.FIRE)
I.FIRE <- correlog(coords = snp[,2:3], z = res.FIRE, method = "Moran")

# Fit models for WCI
gls1.ols <- gls(WCI ~ MAP, data=snp, method="ML")
gls1.ar <- gls(WCI ~ MAP, correlation=corSpher(5000, form=~X+Y), data=snp, method="ML")
anova(gls1.ar, gls1.ols)
gls2.ols <- gls(WCI ~ FIRE, data=snp, method="ML")
gls2.ar <- gls(WCI ~ FIRE, correlation=corSpher(5000, form=~X+Y), data=snp, method="ML")
anova(gls2.ar, gls2.ols)
# Autoregressive models are warranted

# Alternative correlation structures
gls1.ar1 <- gls(WCI ~ MAP, correlation=corSpher(5000, form=~X+Y), data=snp, method="ML")
gls1.ar2 <- gls(WCI ~ MAP, correlation=corExp(5000, form=~X+Y), data=snp, method="ML")
gls1.ar3 <- gls(WCI ~ MAP, correlation=corGaus(5000, form=~X+Y), data=snp, method="ML")
AIC(gls1.ar1, gls1.ar2, gls1.ar3)
# Exponential correlation structure fits best

gls0 <- gls(WCI ~ 1, correlation=corExp(5000, form=~X+Y), data=snp, method="ML")
gls1 <- gls(WCI ~ MAP, correlation=corExp(5000, form=~X+Y), data=snp, method="ML")
gls2 <- gls(WCI ~ MAP * SLOPE, correlation=corExp(5000, form=~X+Y), data=snp, method="ML")
gls3 <- gls(WCI ~ MAP + ELEV, correlation=corExp(5000, form=~X+Y), data=snp, method="ML")
gls4 <- gls(WCI ~ MAP * CURVAT, correlation=corExp(5000, form=~X+Y), data=snp, method="ML")
gls5 <- gls(WCI ~ MAP + ELEV.SD, correlation=corExp(5000, form=~X+Y), data=snp, method="ML")
gls6 <- gls(WCI ~ MAP + FIRE, correlation=corExp(5000, form=~X+Y), data=snp, method="ML")
gls7 <- gls(WCI ~ MAP * FIRE, correlation=corExp(5000, form=~X+Y), data=snp, method="ML")
AIC(gls0, gls1, gls2, gls3, gls4, gls5, gls6, gls7)
# Model 3 is best

plot(Variogram(gls3, resType = "n", form=~X+Y))

# What is the approximate range of the autocorrelation for the best model?
plot(Variogram(gls3, form=~X+Y))
# Both graphically and in terms of parameter estimation it is on the order of 2000 m
# Option 1, assuming points are randomly distributed
Area <- (max(snp$X) - min(snp$X)) * (max(snp$Y) - min(snp$Y))
# If points are at least 2000 m apart, we could sample Area / 2000 ^ 2 points
Area / 2000 ^ 2
# We only have 134 points! So this won't work...
# Option 2
# Overlay a 2000 x 2000 m grid and assign each point to a position in the grid
# This makes a 2000 x 2000 grid
library(raster)
cs <- c(2000, 2000) # Cell size in m
cc <- c(min(snp$X), min(snp$Y)) # cell offset of lower left cell
cd <- c(round((max(snp$X) - min(snp$X)) / 2000), round((max(snp$Y) - min(snp$Y)) / 2000)) # number of cells
grd <- GridTopology(cellcentre.offset=cc, cellsize=cs, cells.dim=cd)
# This converts snp into a shapefile (GIS)
coordinates(snp) <- ~X+Y
proj4string(snp) <- CRS("+proj=utm +datum=WGS84")
# This converts our grid to a spatial grid object
sp_grd <- SpatialGridDataFrame(grd,
                               data=data.frame(id=1:prod(cd)),
                               proj4string=CRS("+proj=utm +datum=WGS84"))
# Now overlay snp points on the grid
ov <- over(snp, sp_grd)
# How many different tiles are used?
length(unique(ov$id))
snp$loc <- ov$id
# We can get rid of duplicate locations (those on the same grid location)
snp2 <- snp[!duplicated(snp$loc), ]
ols0 <- lm(WCI ~ 1, data=snp2)
ols1 <- lm(WCI ~ MAP, data=snp2)
ols2 <- lm(WCI ~ MAP * SLOPE, data=snp2)
ols3 <- lm(WCI ~ MAP + ELEV, data=snp2)
ols4 <- lm(WCI ~ MAP * CURVAT, data=snp2)
ols5 <- lm(WCI ~ MAP + ELEV.SD, data=snp2)
ols6 <- lm(WCI ~ MAP + FIRE, data=snp2)
ols7 <- lm(WCI ~ MAP * FIRE, data=snp2)
AIC(ols0, ols1, ols2, ols3, ols4, ols5, ols6, ols7)
# Now, model 4 is best!