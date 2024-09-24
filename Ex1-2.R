# Set the value of beta
beta <- 1/2

# Function to generate n Weibull random variables using inversion
generate_weibull <- function(n, beta) {
  U <- runif(n)  # Generate n uniform random variables U ~ Uniform(0, 1)
  X <- (-log(1 - U))^(1/beta)  # Apply the inversion formula
  return(X)
}

# Generate Weibull random variables
n <- 10000  # Number of samples
weibull_samples <- generate_weibull(n, beta)

# Plot histogram of simulated values
hist(weibull_samples, breaks = 50, probability = TRUE, 
     main = "Histogram of Simulated Weibull Random Variables",
     xlab = "X", col = "lightblue", border = "black")

# Add the theoretical density curve
x_vals <- seq(0, max(weibull_samples), length.out = 1000)
theoretical_density <- beta * x_vals^(beta - 1) * exp(-x_vals^beta)
lines(x_vals, theoretical_density, col = "red", lwd = 2)

# Add a legend
legend("topright", legend = c("Simulated Values", "Theoretical Density"),
       col = c("lightblue", "red"), lwd = 2)


