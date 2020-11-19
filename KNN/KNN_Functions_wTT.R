##################################################################################################
# Copyright (c) 2020, Ana Karen Reyna Rivas
# All rights reserved.

# This source code is licensed under the BSD-style license found in the LICENSE file in the root directory of this source tree.
##################################################################################################

# Save to Data Frame
saveFinalSummaryTable_knn_TT = function(dataframeValues, x, TRB, model,
                                       accuracy, kappa_model1,
                                       sensitivity_model1, specificity_model1) {
  i <- 0
  dataframeValues[x, i <- i + 1] = TRB
  dataframeValues[x, i <- i + 1] = model
  dataframeValues[x, i <- i + 1] = paste(accuracy, '%') 
  dataframeValues[x, i <- i + 1] = kappa_model1
  dataframeValues[x, i <- i + 1] = sensitivity_model1
  dataframeValues[x, i <- i + 1] = specificity_model1
  
  return (dataframeValues)
}

# Save to Data Frame
saveFinalSummaryTable_knn_TT_2 = function(dataframeValues, x, TRB,
                                         accuracy_model1, kappa_model1,
                                         accuracy_model2, kappa_model2,
                                         accuracy_model3, kappa_model3,
                                         accuracy_model4, kappa_model4) {
  i <- 0
  dataframeValues[x, i <- i + 1] = TRB
  dataframeValues[x, i <- i + 1] = paste(accuracy_model1, '%') 
  dataframeValues[x, i <- i + 1] = kappa_model1
  dataframeValues[x, i <- i + 1] = paste(accuracy_model2, '%') 
  dataframeValues[x, i <- i + 1] = kappa_model2
  dataframeValues[x, i <- i + 1] = paste(accuracy_model3, '%') 
  dataframeValues[x, i <- i + 1] = kappa_model3
  dataframeValues[x, i <- i + 1] = paste(accuracy_model4, '%') 
  dataframeValues[x, i <- i + 1] = kappa_model4
  
  return (dataframeValues)
}

# Get Model 1
get_model1_knn_TT <- function(version, seed, data_train, trControl){
  # Set Random Seed to Insure You Can Reproduce Results
  set.seed(seed)
  
  # Train Model
  fit <- train(SplitEntrepreneurIntention ~ . - Typerulebreaking - SplitEntrepreneurIntention,
               data = data_train,
               method = "knn",
               metric = "Accuracy",
               trControl = trControl)
  
  # Print Results
  print(fit)
  
  # Get and Format K, Accuracy, Kappa Values
  best_k <- fit$bestTune$k
  bestFit <- filter(fit$results, k == best_k)
  best_accuracy <- bestFit$Accuracy * 100
  best_accuracy <- format(round(best_accuracy, 2), nsmall = 2)
  best_kappa <- bestFit$Kappa
  best_kappa <- format(round(best_kappa, 2), nsmall = 2)
  
  # Return Object with Model, Accuracy, Kappa, K
  model1 <- list(fit = fit,
                 accuracyTest = best_accuracy,
                 kappaTest = best_kappa,
                 bestK = best_k)
  
  return (model1)
}

# Get Model 2
get_model2_knn_TT <- function(version, seed, fit, data_train, trControl){
  # Set Random Seed to Insure You Can Reproduce Results
  set.seed(seed)
  
  # Find the Best K
  tuneGrid <- expand.grid(.k = c(1:15))
  knn_fit <- train(SplitEntrepreneurIntention ~ . - Typerulebreaking - SplitEntrepreneurIntention,
                   data = data_train,
                   method = "knn",
                   metric = "Accuracy",
                   tuneGrid = tuneGrid,
                   trControl = trControl)
  
  # Print Results
  print(knn_fit)
  
  # Get and Format K, Accuracy, Kappa Values
  best_k <- knn_fit$bestTune$k
  bestFit <- filter(knn_fit$results, k == best_k)
  best_accuracy <- bestFit$Accuracy * 100
  best_accuracy <- format(round(best_accuracy, 2), nsmall = 2)
  best_kappa <- bestFit$Kappa
  best_kappa <- format(round(best_kappa, 2), nsmall = 2)
  
  # Return Object with Model, Accuracy, Kappa, K
  model2 <- list(fit = knn_fit,
                 accuracyTest = best_accuracy,
                 kappaTest = best_kappa,
                 bestK = best_k)
  
  return (model2)
}