# Function to compute the true policy value difference
# Inputs:
#   dgp_D: A function to generate treatment assignment D given X
#   dgp_Y: A function to generate potential outcome Y given D and X for a single individual
#   X: Covariate matrix or data frame with each row representing an individual's covariates
#   learned_policy: Vector representing the learned policy assignments
#   baseline_policy: Vector representing the baseline policy assignments

true_delta <- function(dgp_Y, X, learned_policy, baseline_policy) {
  
  n <- nrow(X)  # Number of individuals
  
  # Step 1: Generate potential outcomes Y(1) and Y(0) for each individual
  Y_1 <- numeric(n)  # Vector to store potential outcomes if treated
  Y_0 <- numeric(n)  # Vector to store potential outcomes if not treated
  
  for (i in 1:n) {
    # Generate Y(1) and Y(0) for each individual
    Y_1[i] <- dgp_Y(X[i, , drop = FALSE], D = 1)  # Potential outcome if treated
    Y_0[i] <- dgp_Y(X[i, , drop = FALSE], D = 0)  # Potential outcome if not treated
  }
  
  # Step 2: Compute the difference in policy assignments for each individual
  policy_diff <- learned_policy - baseline_policy
  
  # Step 3: Calculate the true policy value difference
  delta_pi <- mean((Y_1 - Y_0) * policy_diff)
  
  return(delta_pi)
}


# Example user-defined functions for potential outcomes

# Potential outcome model based on linear regression model
dgp_Y <- function(X, D) {
  coef_D <- 2.0  # Treatment effect
  coef_X <- c(0.3, -0.5)  # Coefficients for each covariate in X
  
  # Calculate the outcome for a single individual as a function of D and X
  Y <- coef_D * D + as.numeric(as.matrix(X) %*% coef_X) + rnorm(1, mean = 0, sd = 1)
  return(Y)
}

# Generate example covariate data X
set.seed(123)
n <- 100
X <- data.frame(X1 = rnorm(n), X2 = rnorm(n))

# Assuming policies_matrix is the output from the previous step, with the final learned policy in the last column
# Extract the baseline and final learned policy from policies_matrix
baseline_policy <- policies_matrix[, 1]          # First column: baseline policy
learned_policy <- policies_matrix[, ncol(policies_matrix)]  # Last column: final learned policy

# Compute the true policy value difference
delta_pi_T_pi_0 <- true_delta(dgp_Y = dgp_Y, X = X, 
                              learned_policy = learned_policy, 
                              baseline_policy = baseline_policy)

# Print the result
print(delta_pi_T_pi_0)
