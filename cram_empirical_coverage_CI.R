path <- "C:/Users/yanis/OneDrive/Documents/"

# Load functions
source(file.path(path, "cram_estimate.R"))
source(file.path(path, "cram_variance_estimator.R"))


#' Calculate Empirical Coverage of Confidence Interval for Cram Estimator
#'
#' This function calculates the empirical coverage of the confidence interval for the cram estimator
#' by checking how many bootstrap confidence intervals contain the true policy difference.
#'
#' @param delta_estimates A vector of \(\hat{\Delta}\) estimates generated from resampling.
#' @param variance_estimates A vector of variance estimates corresponding to each \(\hat{\Delta}\) estimate.
#' @param true_delta The true policy value difference \(\Delta(\hat{\pi}_T; \pi_0)\).
#' @param T The number of batches.
#' @param alpha Significance level for the confidence interval (default is 0.05 for a 95% CI).
#' @return The empirical coverage proportion.
cram_empirical_coverage_CI <- function(delta_estimates, variance_estimates, true_delta, T, alpha = 0.05) {
  z_alpha <- qnorm(1 - alpha / 2)  # Z-score for the confidence interval
  coverage_count <- 0
  n <- length(delta_estimates)
  
  # Loop through each delta estimate and check if the true delta is within the CI
  for (i in 1:n) {
    margin_error <- z_alpha * (sqrt(variance_estimates[i]) / sqrt(T))
    lower_bound <- delta_estimates[i] - margin_error
    upper_bound <- delta_estimates[i] + margin_error
    if (true_delta >= lower_bound && true_delta <= upper_bound) {
      coverage_count <- coverage_count + 1
    }
  }
  
  # Calculate the empirical coverage as the proportion of intervals that cover the true delta
  coverage_proportion <- coverage_count / n
  return(coverage_proportion)
}


# Example usage of cram_empirical_coverage_CI with bootstrap-generated \(\hat{\Delta}\) estimates

# Placeholder values for the true policy difference
true_delta <- 0.5  # This should be the known or assumed true value of \(\Delta(\hat{\pi}_T; \pi_0)\)

# Parameters
n_bootstrap <- 1000  # Number of bootstrap samples
nb_batch <- 10  # Number of batches
batch_size <- 10  # Size of each batch
T <- nb_batch  # Number of batches

# Initialize vectors to store the bootstrap estimates of delta and variance
delta_estimates <- numeric(n_bootstrap)
variance_estimates <- numeric(n_bootstrap)

# Generate original data
set.seed(123)
Y <- rnorm(100)  # Simulated outcome variable
D <- sample(0:1, 100, replace = TRUE)  # Binary treatment variable
pi <- matrix(sample(0:1, 100 * 11, replace = TRUE), nrow = 100, ncol = 11)  # Policy matrix
batch_indices <- split(1:100, rep(1:nb_batch, each = batch_size))  # Split into batches

# Perform bootstrap sampling to generate multiple delta and variance estimates
for (b in 1:n_bootstrap) {
  # Generate bootstrap sample
  bootstrap_indices <- sample(1:length(Y), replace = TRUE)
  Y_boot <- Y[bootstrap_indices]
  D_boot <- D[bootstrap_indices]
  pi_boot <- pi[bootstrap_indices, ]
  
  # Compute delta and variance for the bootstrap sample
  delta_estimates[b] <- cram_estimator(Y_boot, D_boot, pi_boot, nb_batch, batch_indices)
  variance_estimates[b] <- cram_variance_estimator(Y_boot, D_boot, pi_boot, nb_batch, batch_indices, batch_size)
}

# Calculate empirical coverage of the confidence intervals
coverage <- cram_empirical_coverage_CI(delta_estimates, variance_estimates, true_delta, T)
print(paste("Empirical Coverage of 95% CI:", round(coverage * 100, 2), "%"))

