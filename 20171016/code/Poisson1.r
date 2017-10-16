set.seed(1001)
# Make a 1-ha 'habitat' and randomly distribute T seedlings in it
T <- 500
x <- runif(T,0,100) # x coordinate
y <- runif(T,0,100) # y coordinate
plot(x,y)
# Now take samples from 50 sq.m circular plots
r <- sqrt(50/pi) # The plots are of radius r
# Sample N plots and put seedling counts into vector Counts
N <- 500
Counts <- numeric(N)
px <- runif(N,4,96) # Account for edges
py <- runif(N,4,96) # Account for edges
# Count number of seedlings in each plot
for (i in 1:N){
  # Calculate the distance between each seedling and the center of plot i
  D <- sqrt((px[i]-x)^2+(py[i]-y)^2)
  # How many are within a distance r?
  In <- 1*(D<r)
  # Count them!
  Counts[i] <- sum(In)
}
f <- factor(Counts,levels=0:10)
plot(f)