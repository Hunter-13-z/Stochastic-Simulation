c = 1
xi = 1
mu = c/xi
lambda = c / xi^3

generate_geo = function(n){
  U = runif(n)
  samples = -log(U)
  
  return(samples)
}

generate_normal = function(n){
  Y1 = generate_geo(n)
  Y2 = generate_geo(n)
  
  
  Z = Y1[Y2 >= (Y1 - 1)^2 / 2]
  
  # Step 4: Ensure we have at least n normal samples (in case some are rejected)
  while (length(Z) < n) {
    # Generate more Y1 and Y2 if necessary
    Y1 = generate_geo(n)
    Y2 = generate_geo(n)
    
    Z = c(Z, Y1[Y2 >= (Y1 - 1)^2 / 2])
  }
  
 
  Z = Z[1:n]
  
  U = runif(n)
  Z[U <= 0.5] = -Z[U <= 0.5]
  
  return(Z)
}

generate_inverse_gaussian = function(n, mu, lambda) {
  normal_samples = generate_noraml(n)  # Generate n normal random variables
  inverse_gaussian_samples = numeric(n)  
  
  for (i in 1:n) {
    Z = normal_samples[i]
    
    # Generate the inverse Gaussian candidate
    X1 = mu + (mu^2 * Z^2) / (2 * lambda) - (mu / (2 * lambda)) * sqrt(4 * mu * lambda * Z^2 + mu^2 * Z^4)
    
    # Apply A-R
    U = runif(1)
    if (U <= mu / (mu + X1)) {
      inverse_gaussian_samples[i] = X1
    } else {
      inverse_gaussian_samples[i] = mu^2 / X1
    }
  }
  
  return(inverse_gaussian_samples)
}

n = 10000
#normal = generate_noraml(n)
inv_gaussian = generate_inverse_gaussian(n, mu, lambda)

#hist(normal, breaks = 50, probability = TRUE, 
    # main = "Histogram of Simulated Normal Random Variables",
    # xlab = "X", col = "lightblue", border = "black")

hist(inv_gaussian, breaks = 50, probability = TRUE, 
     main = "Histogram of Simulated inverse_gaussian Random Variables",
     xlab = "X", col = "lightgreen", border = "black")



mean_X = mean(inv_gaussian)
var_X = var(inv_gaussian)
mean_X2 = mean(inv_gaussian^2)
std_X2 = sd(inv_gaussian^2)

# Theoretical values
theoretical_mean = c / xi
theoretical_var = c / (xi^3)
theoretical_X2 = theoretical_var + theoretical_mean^2

# Confidence interval E[X]
alpha = 0.05
t_critical = qt(1 - alpha/2, df = n - 1)
std_error = sd(inv_gaussian) / sqrt(n)
mean_ci_lower = mean_X - t_critical * std_error
mean_ci_upper = mean_X + t_critical * std_error

# Confidence Interval E[X^2]
alpha = 0.05
t_critical = qt(1 - alpha/2, df = n - 1)
std_error_X2 = std_X2 / sqrt(n)
X2_ci_lower = mean_X2 - t_critical * std_error_X2
X2_ci_upper = mean_X2 + t_critical * std_error_X2

# Confidence Interval Var(X)
var_ci_lower = (n - 1) * var_X / qchisq(1 - alpha/2, df = n - 1)
var_ci_upper = (n - 1) * var_X / qchisq(alpha/2, df = n - 1)

# Print results
cat("Sample Mean (E[X]):", mean_X, "\n")
cat("95% Confidence Interval for E[X]: [", mean_ci_lower, ",", mean_ci_upper, "]\n")
cat("Theoretical E[X]:", theoretical_mean, "\n\n")

cat("Sample E[X^2]:", mean_X2, "\n")
cat("95% Confidence Interval for E[X^2]: [", X2_ci_lower, ",", X2_ci_upper, "]\n")
cat("Theoretical E[X^2]:", theoretical_X2, "\n\n")

cat("Sample Variance (Var[X]):", var_X, "\n")
cat("95% Confidence Interval for Var[X]: [", var_ci_lower, ",", var_ci_upper, "]\n")
cat("Theoretical Var[X]:", theoretical_var, "\n")
