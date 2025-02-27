#' Crammed Variance Estimator for Policy Evaluation
#'
#' This function estimates the variance of the cram estimator to measure the asymptotic
#' variance of policy value differences.
#'
#' @param Y A vector of outcomes for the n individuals.
#' @param D A vector of binary treatments for the n individuals.
#' @param pi A matrix of n rows and (nb_batch + 1) columns, containing the policy assignment for each individual
#'           for each policy. The first column represents the baseline policy.
#' @param batch_indices A list where each element is a vector of indices corresponding to the individuals in each batch.
#' @return The estimated variance \(\hat{v}^2_T\).
#' @examples
#' # Example usage:
#' Y <- sample(0:1, 100, replace = TRUE)
#' D <- sample(0:1, 100, replace = TRUE)
#' pi <- matrix(sample(0:1, 100 * 11, replace = TRUE), nrow = 100, ncol = 11)
#' batch_indices <- split(1:100, rep(1:nb_batch, each = 10))
#' variance_estimate <- cram_variance_estimator(Y, D, pi, batch_indices)
cram_variance_estimator <- function(Y, D, pi, batch_indices) {
  # Determine number of batches
  nb_batch <- length(batch_indices)
  # Batch size (assuming all batches are the same size)
  batch_size <- length(batch_indices[[1]])
  
  if (nrow(pi) != length(Y) || length(Y) != length(D)) {
    stop("Y, D, and pi must have matching lengths")
  }
  
  # Initialize the total variance estimator
  variance_hat <- 0
  
  # Loop through each batch (from j = 2 to T)
  for (j in 2:nb_batch) {
    # Collect all g_hat_Tj values across batches k = j to T 
    g_hat_Tj_values <- c()
    
    for (k in j:nb_batch) {
      # Compute g_hat_Tj for each individual in batch k
      for (i in batch_indices[[k]]) {
        # IPW estimator component
        ipw_component <- Y[i] * D[i] / 0.5 - Y[i] * (1 - D[i]) / 0.5
        # Summing the weighted policy differences for each t from 1 to j - 1
        policy_diff_sum <- sum((pi[i, 2:j] - pi[i, 1:(j - 1)]) / (nb_batch - (1:(j - 1))))
        # Compute g_hat_Tj for individual i in batch k
        g_hat_Tj <- ipw_component * policy_diff_sum
        # Collect g_hat_Tj values for batch j
        g_hat_Tj_values <- c(g_hat_Tj_values, g_hat_Tj)
      }
    }
    
    # Mean of g_hat_Tj over the batches from j to T
    g_bar_Tj <- mean(g_hat_Tj_values)
    
    # Compute V_hat(g_hat_Tj) for batch j
    if (batch_size == 1 && j == nb_batch) {
      V_hat_g_Tj <- 0  # Set variance to zero if batch size is one and j = T
    } else {
      V_hat_g_Tj <- sum((g_hat_Tj_values - g_bar_Tj)^2) / (batch_size * (nb_batch - j + 1) - 1)
    }
    
    # Add contribution of this batch to the total variance estimator
    variance_hat <- variance_hat + V_hat_g_Tj
  }
  
  # Final variance estimator, scaled by T / B
  variance_hat <- (nb_batch / batch_size) * variance_hat
  return(variance_hat)
}


# Y <- sample(0:1, 100, replace = TRUE)
# D <- sample(0:1, 100, replace = TRUE)
# pi <- matrix(sample(0:1, 100 * 11, replace = TRUE), nrow = 100, ncol = 11)
# variance_estimate <- cram_variance_estimator(Y, D, pi, batch_indices)
# print(variance_estimate)