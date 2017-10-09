# Factorial loop example
N <- 7
f <- 1
for (i in 1:N){
  f <- f*i
}
print(f)

# A conditional example
# Find values at least one sd larger than the mean
R <- rnorm(10, mean=5, sd=2)
for (i in 1:10){
  if (R[i] > 7){
    print(c(R[i],": Greater"))
  }
  else{
    print(c(R[i],": Not greater"))
  }
}

# Count even numbers in a sequence
x <- c(2,5,3,9,8,11,6)
count <- 0
for (val in x) {
  if(val %% 2 == 0) count = count+1 # %% is the modulus operator
}
print(count)

# The problem above can easily be vectorized
sum(x %% 2==0)

# Example using ifelse
# Find prime numbers in a sequence
y <- 1:20
prime <- numeric(length(y))
for (i in 1:length(y)){
  a <- 1:y[i] # Sequence of values from 1 to y[i]
  b <- y[i] %% a # Find modulus of y[i] divided by every value from 1 to y[i]
  prime[i] <- ifelse(sum(b == 0) > 2, 0, y[i]) # If prime, there should be only two zeros
}
print(prime[prime != 0])
