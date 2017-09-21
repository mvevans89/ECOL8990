set.seed(101)
# Generate random x and y data
x <- runif(30, min = 0, max = 10)
y <- rnorm(30, mean = 2 * x, sd = 2)
# Make a vector of potential slope values
slopes <- seq(from = 0.1, to = 4, by = 0.1)
# Make a vector to store the residual sums of squares
ss <- numeric(length(slopes))
# Loop through the slope values
for (i in 1:length(slopes)){
  # EXERCISE: calculate the residual sum of squares for a given slope (slopes[i])
  # and store it in ss[i]
}
# plot slopes on the x-axis and ss on the y-axis