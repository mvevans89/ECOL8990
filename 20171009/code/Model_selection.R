library(MuMIn)
snp  <- read.csv("../data/Snp_tree_community.csv")
# Variable reduction
# Soil variables are poribably highly correlated
cor(snp[,11:14])
# One option: variable reduction through principal components
pca <- prcomp(snp[,11:14], scale=TRUE)
snp$PC1 <- pca$x[,1]
snp$PC2 <- pca$x[,2]
# Candidate model approach
mod0 <- glm(MDS1 ~ 1, data=snp) # Null model
mod1 <- glm(MDS1 ~ MAP, data=snp)
mod2 <- glm(MDS1 ~ MAP + SLOPE, data=snp)
mod3 <- glm(MDS1 ~ MAP + PC1, data=snp)
mod4 <- glm(MDS1 ~ MAP + FIRE, data=snp)
mod5 <- glm(MDS1 ~ MAP + ELE10, data=snp)
mod6 <- glm(MDS1 ~ MAP + ELE10 + PC1, data=snp)
mod7 <- glm(MDS1 ~ MAP + ELE10 + PC1 + PC2, data=snp)
mod8 <- glm(MDS1 ~ SLOPE + MAP + FIRE, data=snp)
mod9 <- glm(MDS1 ~ SLOPE * MAP, data=snp)
mod10 <- glm(MDS1 ~ MAP + RIVDIST + ELE10, data=snp)
mod11 <- glm(MDS1 ~ MAP + RIVDIST + ELE10 + PC1, data=snp)
mod12 <- glm(MDS1 ~ MAP + RIVDIST, data=snp)
mod13 <- glm(MDS1 ~ MAP + RIVDIST + FIRE + ELE10, data=snp)
aic <- AICc(mod0,mod1,mod2,mod3,mod4,mod5,mod6,mod7,mod8, mod9, mod10, mod11, mod12, mod13)
aic$w <- Weights(aic)

# 'Dredging' using MuMIn
options(na.action = "na.fail")
mod.full <- glm(MDS1 ~ MAP + SLOPE + ELEV + RIVDIST + ELE10 + FIRE + TOPO + PC1 + PC2,
                data = snp)
dr <- dredge(mod.full)
sub <- subset(dr, delta < 4)
par(mar = c(3,5,6,4))
plot(dr, labAsExpr = TRUE, border=NA)
# Model averaging using cumulative weight up to 95%
ma <- model.avg(dr, subset = cumsum(weight) <= .95) # get averaged coefficients
ma$coefficients
