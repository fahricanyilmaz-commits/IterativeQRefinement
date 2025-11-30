# -------------------------------------------------------------------------
# Title:        GDINA Q-Matrix Refinement (Maximum Speed Optimization)
# Description:  Automated Q-matrix validation and refinement using parallel processing.
# License:      Copyright (c) 2025 [Fahri Yilmaz]. All Rights Reserved.
# -------------------------------------------------------------------------

if(!require(GDINA)) install.packages("GDINA")
if(!require(parallel)) install.packages("parallel")

library(GDINA)
library(parallel)

display_item_probabilities <- function(response_data, q_matrix) {
  final_model <- GDINA(response_data, q_matrix, model = "GDINA", verbose = 0)
  item_probs <- coef(final_model, what = "itemprob")
  
  cat("\nItem Success Probabilities:\n")
  cat("==========================\n")
  
  for(item in 1:length(item_probs)) {
    cat(sprintf("\nItem %d:\n", item))
    n_item_attrs <- sum(q_matrix[item,])
    probs <- item_probs[[item]]
    names <- names(probs)
    
    if(is.null(names)) {
      patterns <- expand.grid(replicate(n_item_attrs, c(0,1), simplify = FALSE))
      names <- apply(patterns, 1, paste, collapse="")
    }
    
    for(i in 1:length(probs)) {
      cat(sprintf("P(%s) = %.4f\n", names[i], probs[i]))
    }
    cat(sprintf("Non-mastery (P(0..0)) = %.4f\n", probs[1]))
    cat(sprintf("Full mastery (P(1..1)) = %.4f\n", probs[length(probs)]))
    cat("------------------------\n")
  }
}

get_attribute_combinations <- function(n_attrs, total_attrs) {
  if(n_attrs > total_attrs) return(NULL)
  combn(total_attrs, n_attrs)
}

evaluate_item_fit <- function(item_idx, test_q, response_data, success_threshold, nonmastery_threshold) {
  if(sum(test_q[item_idx,]) == 0) {
    return(list(meets_criteria = FALSE, max_prob = 0, nonmastery_prob = 1, score = -Inf))
  }
  
  tryCatch({
    test_model <- GDINA(response_data, test_q, model = "GDINA",
                        control = list(conv.crit = 0.30, max.iter = 30), verbose = 0)
    probs <- coef(test_model, what = "itemprob")[[item_idx]]
    
    list(
      meets_criteria = probs[length(probs)] >= success_threshold && probs[1] <= nonmastery_threshold,
      max_prob = probs[length(probs)],
      nonmastery_prob = probs[1],
      score = probs[length(probs)] - probs[1]
    )
  }, error = function(e) {
    list(meets_criteria = FALSE, max_prob = 0, nonmastery_prob = 1, score = -Inf)
  })
}

process_item <- function(item, q_matrix, n_attributes, response_data, 
                         success_threshold, nonmastery_threshold,
                         max_attributes_per_item) {
  best_config <- q_matrix[item,]
  best_result <- list(meets_criteria = FALSE, max_prob = 0, nonmastery_prob = 1, score = -Inf)
  
  for(n_attrs in 1:min(max_attributes_per_item, n_attributes)) {
    attr_combs <- get_attribute_combinations(n_attrs, n_attributes)
    if(!is.null(attr_combs)) {
      for(i in 1:ncol(attr_combs)) {
        test_q <- q_matrix
        test_q[item,] <- 0
        test_q[item, attr_combs[,i]] <- 1
        
        result <- evaluate_item_fit(item, test_q, response_data, 
                                    success_threshold, nonmastery_threshold)
        
        if(result$score > best_result$score) {
          best_result <- result
          best_config <- test_q[item,]
          if(best_result$meets_criteria) break
        }
      }
    }
    if(best_result$meets_criteria) break
  }
  
  list(config = best_config, result = best_result)
}

refine_q_matrix <- function(response_data, initial_q_matrix, 
                            success_threshold = 0.80,
                            nonmastery_threshold = 0.35,
                            max_total_attributes = NULL,
                            max_attributes_per_item = 10,
                            n_cores = NULL) {
  
  n_items <- nrow(initial_q_matrix)
  
  if(is.null(max_total_attributes)) {
    max_total_attributes <- floor(n_items/4)
  }
  
  q_matrix <- initial_q_matrix
  n_attributes <- ncol(q_matrix)
  
  if(is.null(n_cores)) {
    n_cores <- min(parallel::detectCores() - 1, 7)
  }
  
  cl <- makeCluster(n_cores)
  clusterEvalQ(cl, { library(GDINA); NULL })
  clusterExport(cl, c("get_attribute_combinations", "evaluate_item_fit", "process_item"))
  on.exit(stopCluster(cl))
  
  cat("\nEvaluating initial Q-matrix configuration...\n")
  cat(sprintf("Cores: %d | Max attrs/item: %d | Max total attrs: %d\n", n_cores, max_attributes_per_item, max_total_attributes))
  
  pb <- txtProgressBar(min = 0, max = n_items, style = 3)
  
  initial_model <- GDINA(response_data, q_matrix, model = "GDINA",
                         control = list(conv.crit = 0.30, max.iter = 30), verbose = 0)
  initial_probs <- coef(initial_model, what = "itemprob")
  
  problematic_items <- integer(0)
  for(item in 1:n_items) {
    setTxtProgressBar(pb, item)
    probs <- initial_probs[[item]]
    if(probs[length(probs)] < success_threshold || probs[1] > nonmastery_threshold) {
      problematic_items <- c(problematic_items, item)
    }
  }
  close(pb)
  
  if(length(problematic_items) > 0) {
    cat(sprintf("\nFound %d problematic items. Starting refinement...\n", length(problematic_items)))
    
    still_problematic <- problematic_items
    
    while(length(still_problematic) > 0 && n_attributes < max_total_attributes) {
      n_attributes <- n_attributes + 1
      q_matrix <- cbind(q_matrix, rep(0, n_items))
      colnames(q_matrix)[n_attributes] <- paste0("A", n_attributes)
      
      cat(sprintf("\nTrying configurations with %d attributes...\n", n_attributes))
      pb <- txtProgressBar(min = 0, max = length(still_problematic), style = 3)
      
      batch_size <- ceiling(length(still_problematic) / n_cores)
      batches <- split(still_problematic, ceiling(seq_along(still_problematic) / batch_size))
      
      for(batch_idx in seq_along(batches)) {
        batch_items <- batches[[batch_idx]]
        
        results <- parLapply(cl, batch_items, process_item,
                             q_matrix = q_matrix,
                             n_attributes = n_attributes,
                             response_data = response_data,
                             success_threshold = success_threshold,
                             nonmastery_threshold = nonmastery_threshold,
                             max_attributes_per_item = max_attributes_per_item)
        
        for(idx in seq_along(batch_items)) {
          item <- batch_items[idx]
          q_matrix[item,] <- results[[idx]]$config
          
          # Uncomment next line if you want verbose item-by-item updates in console
          # if(results[[idx]]$result$meets_criteria) cat(sprintf("\nItem %d: Success!", item))
          
          setTxtProgressBar(pb, (batch_idx - 1) * batch_size + idx)
        }
      }
      close(pb)
      
      model <- GDINA(response_data, q_matrix, model = "GDINA",
                     control = list(conv.crit = 0.30, max.iter = 30), verbose = 0)
      probs <- coef(model, what = "itemprob")
      still_problematic <- which(sapply(probs, function(p) {
        p[length(p)] < success_threshold || p[1] > nonmastery_threshold
      }))
      
      cat(sprintf("\nItems still problematic: %d\n", length(still_problematic)))
    }
  }
  
  attr_usage <- colSums(q_matrix)
  if(any(attr_usage == 0)) {
    used_attrs <- which(attr_usage > 0)
    q_matrix <- q_matrix[, used_attrs, drop = FALSE]
    colnames(q_matrix) <- paste0("A", 1:ncol(q_matrix))
  }
  
  return(q_matrix)
}

# --- Example Usage (Uncomment to run) ---

# refined_q <- refine_q_matrix(
#   response_data = response_data,
#   initial_q_matrix = initial_q_matrix,
#   success_threshold = 0.80,
#   nonmastery_threshold = 0.35
# )

# display_item_probabilities(response_data, refined_q)