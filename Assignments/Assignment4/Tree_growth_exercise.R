N <- 32
splist <- c('A', 'B', 'C')
Plot <- sample(1:5, N, replace=TRUE)
tree <- data.frame(Plot)
tree$Species <- factor(sample(splist, N, replace = TRUE))
tree$Y <- 0 # Placeholder for Y (growth rate)
alpha.A <- 8 # species A effect
alpha.B <- 6 # species B effect
alpha.C <- 4.5 # species C effect
sigma.sq <- 2 # standard deviation of epsilon error term
sigma.sq.b <- 0.5
b <- rnorm(5, mean=0, sd=sigma.sq.b)
epsilon <- rnorm(N, mean=0, sd=sigma.sq) # generate error terms
for (k in 1:N){
  if (tree$Species[k]=="A") alpha <- alpha.A
  else if (tree$Species[k]=="B") alpha <- alpha.B
  else alpha <- alpha.C
  tree$Y[k] <- alpha + b[tree$Plot[k]] + epsilon[k] # Growth model
}
Plotfact <- factor(tree$Plot) # Tell R that Plot is a categorical variable