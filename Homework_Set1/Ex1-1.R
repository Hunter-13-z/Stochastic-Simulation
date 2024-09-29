# Function to draw n samples from the bivariate random variable U = (U1, U2)
generate_samples <- function(n) {
  U1 = runif(n, min = -1, max = 1)  # U1 ~ Uniform(-1, 1)
  U2 = runif(n, min = 0, max = 1)   # U2 ~ Uniform(0, 1)
  
  samples = cbind(U1, U2)
  
  return(samples)
}


calculate_sum = function(samples,n) {
  result = 0
  for(i in 1:n){
    indicator = ((samples[i,1]^2 +samples[i,2]^2)<=1)
    result = result + indicator
  }
  
  result = result * 4/n
  return(result)
  
}

# Example: Generate 500 samples
n = 50000
samples = generate_samples(n)

# Print the samples
#print(samples)

result = calculate_sum(samples,n)
print(result)

#We got the approximation of pi
#As the sample grows larger, our sum get more close to the true value of pi.
#I will use the picture under to illustrate

# Determine if each sample is inside or outside the quarter circle
inside_circle = (samples[, 1]^2 + samples[, 2]^2 <= 1)

# Plot the points
plot(samples, col = ifelse(inside_circle, "blue", "red"), 
     pch = 2, xlab = "U1", ylab = "U2", main = "Monte Carlo Simulation to Estimate Pi")

# Add a quarter circle boundary to the plot
curve(sqrt(1 - x^2), from = -1, to = 1, add = TRUE, col = "black", lwd = 2)

#As we can see x^2 + y^2<= 1 form a half circle. And here we are consider the square,
#so we will get a quater circle and the probabilty to fall in which will be pi/4.


