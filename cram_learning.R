library(grf)

# Function to run the learning process of the cram method
cram_learning <- function(X, D, Y, nb_batch) {
  
  # Step 1: Set up batches
  n <- nrow(X)
  indices <- sample(1:n)  # Randomly shuffle the indices without replacement
  group_labels <- rep(1:nb_batch, length.out = n)  # Repeat labels 1 to nb_batch, filling up to n elements
  batches <- split(indices, group_labels)  # Split indices into batches
  
  # Step 2: Initialize a matrix to store policies
  policies <- matrix(0, nrow = n, ncol = nb_batch + 1)  # Initialize with zeros
  # First column is already set to the baseline policy (all zeros)
  
  # Step 3: Iteratively learn policies on cumulative batches
  for (t in 1:nb_batch) {
    
    # Accumulate indices for batches 1 through t
    cumulative_indices <- unlist(batches[1:t])  # Combine batches 1 through t
    cumulative_X <- X[cumulative_indices, ]
    cumulative_D <- D[cumulative_indices]
    cumulative_Y <- Y[cumulative_indices]
    
    # Train causal forest on accumulated data
    causal_forest_fit <- causal_forest(cumulative_X, cumulative_Y, cumulative_D, num.trees = 2000)
    cate_estimates <- predict(causal_forest_fit, X)$predictions  # Predict CATE on full dataset X
    
    # Define the learned policy based on CATE: Assign 1 if CATE is positive, 0 otherwise
    learned_policy <- ifelse(cate_estimates > 0, 1, 0)
    
    # Store the learned policy in the (t+1)th column of the policies matrix
    policies[, t + 1] <- learned_policy
    
  }
  
  final_policy_model <- causal_forest_fit
  
  return(list(
    final_policy_model = final_policy_model,
    policies = policies,
    batch_indices = batches 
  ))
}

# Example usage:
# Assuming X, D, Y are already defined and nb_batch is set
# result <- cram_learning(X = X_data, D = D_data, Y = Y_data, nb_batch = 3)
# This will return a list `result` where:
# - result$policies is the policies matrix,
# - result$batch_indices contains the batch indices in the format of split(1:100, rep(1:nb_batch, each = 10)).

# Example usage:
# Assuming X, D, Y are already defined and nb_batch is set
# policies_matrix <- cram_learning(X = X_data, D = D_data, Y = Y_data, nb_batch = 3)
# This will return a matrix `policies_matrix` where:
# - policies_matrix[, 1] is the baseline policy (all 0s),
# - policies_matrix[, 2] to policies_matrix[, nb_batch + 1] are the learned policies on cumulative batches.
