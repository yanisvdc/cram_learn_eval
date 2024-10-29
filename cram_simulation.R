path <- "C:/Users/yanis/OneDrive/Documents/"

# Load functions
source(file.path(path, "cram_generate_data.R"))
source(file.path(path, "cram_learning.R"))
source(file.path(path, "cram_estimate.R"))
source(file.path(path, "cram_variance_estimator.R"))


# Combined simulation function
cram_simulation <- function(X, dgp_D = function(Xi) rbinom(1, 1, 0.5), dgp_Y, nb_batch, nb_simulations) {
  results <- list()
  
  for (i in 1:nb_simulations) {
    # Step 1: Row-wise bootstrap of X
    X_boot <- X[sample(1:nrow(X), nrow(X), replace = TRUE), ]
    
    # Step 2: Generate D for each individual using dgp_D function
    D <- sapply(1:nrow(X_boot), function(j) dgp_D(X_boot[j, ]))
    
    # Step 3: Generate Y for each individual using dgp_Y function
    Y <- sapply(1:nrow(X_boot), function(j) dgp_Y(D[j], X_boot[j, ]))
    
    # Step 4: Run the cram learning process to get policies and batch indices
    learning_result <- cram_learning(X_boot, D, Y, nb_batch)
    policies <- learning_result$policies
    batch_indices <- learning_result$batch_indices
    final_policy_model <- learning_result$final_policy_model
    
    # Step 5: Calculate the proportion of treated individuals under the final policy
    final_policy <- policies[, nb_batch + 1]
    proportion_treated <- mean(final_policy)
    
    # Step 6: Run the cram estimator using the policies and batch indices
    delta_estimate <- cram_estimator(Y, D, policies, batch_indices)
    
    # Step 7: Estimate the standard error of delta_estimate using cram_variance_estimator
    asymptotic_variance <- cram_variance_estimator(Y, D, policies, batch_indices)
    asymptotic_sd <- sqrt(asymptotic_variance)
    standard_error <- asymptotic_sd / sqrt(nb_batch)
    
    # Step 8: Compute the 95% confidence interval for delta_estimate
    ci_lower <- delta_estimate - 1.96 * standard_error
    ci_upper <- delta_estimate + 1.96 * standard_error
    confidence_interval <- c(ci_lower, ci_upper)
    
    # Store results for this simulation
    results[[i]] <- list(
      final_policy_model = final_policy_model,
      proportion_treated = proportion_treated,
      delta_estimate = delta_estimate,
      standard_error = standard_error,
      confidence_interval = confidence_interval
    )
  }
  
  return(results)
}

# Example usage of cram_simulation

set.seed(123)  # For reproducibility

# Define individualized data-generating processes

# dgp_D: Assign treatment with a Bernoulli(0.5) probability based on individual's covariates Xi
dgp_D <- function(Xi) {
  return(rbinom(1, 1, 0.5))  # Each individual has a 50% chance of treatment
}

# dgp_Y: outcome generation as a function of individual D and X with noise
# Define dgp_Y with heterogeneous treatment effects
dgp_Y <- function(d, x) {
  # Define theta based on individual's covariates
  theta <- ifelse(
    x["binary"] == 1 & x["discrete"] <= 2,  # Group 1: High benefit
    1,
    ifelse(x["binary"] == 0 & x["discrete"] >= 4,  # Group 3: High adverse effect
           -1,
           0.1)  # Group 2: Neutral effect (small positive or negative)
  )
  
  # Define outcome Y with treatment effect and noise
  y <- d * (theta + rnorm(1, mean = 0, sd = 1)) + 
    (1 - d) * rnorm(1)  # Outcome influenced by treatment and noise for untreated
  
  # Ensure Y has no names by converting it to an unnamed numeric value
  return(unname(y))
}



# Load the input data (replace generate_data function with actual X matrix)
n <- 1000  # Number of samples
X <- generate_data(n)$X  # Load or generate X

# Number of simulations and batches
nb_simulations <- 2
nb_batch <- 20  

# Run cram_simulation
simulation_results <- cram_simulation(X, dgp_D, dgp_Y, nb_batch, nb_simulations)

# Print results of the first simulation
print(simulation_results[[1]])
