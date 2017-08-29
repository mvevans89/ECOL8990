library(reshape2)
setwd("Z:/Holdo/UGA courses/Statistical Modeling Fall 2017/Aug 16")
nyl <- read.csv("CR1000_SM_5_min.dat", skip = 3)
names(nyl) <- c("Timestamp", "RN", "VWC1_5", "VWC1_10", "VWC1_20", "VWC1_40", "VWC1_70", 
                          "VWC2_10", "VWC3_10", "VWC4_10", "Precip")
# Obtain mean daily VWC
# Extract time data from sapflow
time <- strptime(nyl$Timestamp, "%Y-%m-%d %H:%M:%S")
nyl$Day <- time$yday # Day of year
nyl.sm <- aggregate(cbind(VWC1_5, VWC1_10, VWC1_20, VWC1_40, VWC1_70, VWC2_10, VWC3_10, VWC4_10) ~
                      Day, data = nyl, mean) # Mean daily soil moisture values
nyl.Precip <- aggregate(Precip ~ Day, data = nyl, sum) # Mean daily precipitation values
nyl.ag <- merge(nyl.Precip, nyl.sm, by = "Day") # Merge datasets
# Break up into different dataframes (by location and by depth)
nyl.sm.l <- melt(nyl.sm, id.vars = "Day", variable.name = "ProbeID", value.name = "VWC")
s <- strsplit(as.character(nyl.sm.l$ProbeID), '_') # Split ProbeID name into two columns
nyl.sm.l$Loc <- sapply(s, "[", 1)
nyl.sm.l$Depth <- sapply(s, "[", 2)
gps <- read.csv("Nylsvlei_Probe_GPS.csv")
nyl.sm.10 <- nyl.sm.l[nyl.sm.l$Depth == 10, ]
nyl.sm.10 <- aggregate(VWC ~ Loc, data = nyl.sm.10, mean)
nyl.sm.10 <- merge(gps, nyl.sm.10, by="Loc")
radius <- sqrt(nyl.sm.10$VWC/pi)
symbols(nyl.sm.10$X,nyl.sm.10$Y, circles=radius, inches=0.25, xlab='Eastings (m)', ylab='Northings (m)')