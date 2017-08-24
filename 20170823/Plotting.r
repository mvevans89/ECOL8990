# Plotting examples

# Basic plotting
snp1 <- read.csv("Serengeti1.csv",header=T) # First Serengeti data set
plot(snp1$Site, snp1$Ht) # X-Y plot

# EXERCISE 1: ADD AXIS LABELS -------------------------------------------

hist(snp1$Ht) # Histogram

library(lattice) # Plotting package lattice for trellis plots
# Some examples
xyplot(Ht ~ Dam | Sp, data = snp1) # Lattice X-Y plot
xyplot(Ht ~ Dam | Site, snp1)
densityplot(~ Ht, snp1)
histogram(~ Ht | Sp, snp1) # Lattice histogram by grouping factor species
histogram(~ Ht | Site, snp1) # Lattice histogram by grouping factor site
bwplot(Ht ~ Site | Sp, snp1) # Box-and-whisker plot by species

library(ggplot2) # ggplot2 graphics model
ggplot(data = snp1) + geom_point(mapping = aes(x = Dam, y = Ht))

ggplot(data = snp1) +
  geom_point(mapping = aes(x = Dam, y = Ht), color = "blue", size = 2) +
  xlab("Damage") + ylab("Height (m)") + theme_classic()

ggplot(data = snp1) +
  geom_point(mapping = aes(x = Dam, y = Ht), color = "blue", size = 2) +
  xlab("Damage") + ylab("Height (m)") + theme_classic() +
  theme(axis.text = element_text(size=18)) +
  theme(axis.title = element_text(size=18)) +
  theme(axis.line = element_line(size = 1))

# ggplot facets (lattuce trellis plot analog)
ggplot(data = snp1) +
  geom_point(mapping = aes(x = Dam, y = Ht), color = "blue", size = 2) +
  xlab("Damage") + ylab("Height (m)") + theme_classic() +
  facet_wrap(~ Site, nrow=3)

# EXERCISE 2: FIND NUMBER OF TREES PER SITE AND PLOT

# Plot as boxplot
ggplot(snp.den, aes(Site, Count)) + geom_boxplot()

# EXERCISE 3: REPEAT FOR SPECIES acator AND acarob

# EXERCISE 4: EXTRACT DATA FOR PLOT 1 OF SITE KIT

# Make bubble plots mapping trees by diameter and height
symbols(kit1$Y,kit1$X,circles=kit1$Ht,inches=0.3,bg="green")
symbols(kit1$Y,kit1$X,circles=kit1$Dam,inches=0.3,bg="red",add=T)

# EXERCISE 5: PLOT HEIGHTS ACROSS SPECIES, DAMAGE ACROSS SPECIES, A HEIGHT HISTOGRAM AND
# HEIGHT VS. DAMAGE IN A 2 X 2 PLOT IN BASE R
# REMINDER: USE PAR(MFROW=C(2,2)) TO SET UP 2X2 PANELS

# Second Serengeti data set
snp2 <- read.csv("Serengeti2.csv")
# Plot heights by site and year
xyplot(Ht ~ Year | Site, snp2) # Box-and-whisker plot by site and year
bwplot(Ht ~ Year | Site, snp2)
snp2$Year <- factor(snp2$Year)
bwplot(Ht ~ Year | Site, snp2)
ggplot(snp2, aes(Year, Ht)) + geom_boxplot() +
  facet_wrap(~ Site, nrow=3)

# EXERCISE 6: CALCULATE THE MEDIAN HEIGHT ACROSS SITES AND PLOT THEM ACROSS YEARS

# Back to the datalogger data
# A simple plot of mean daily VWC and precipitation in two vertical panels
par(mfrow=c(2,1))
plot(nyl.ag$Day, nyl.ag$VWC1_10, type="l", col='red', xlab='Day', ylab='VWC')
barplot(nyl.ag$Precip, col='blue', ylab='Precip (mm)')
par(mfrow=c(1,1))