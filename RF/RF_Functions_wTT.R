##################################################################################################
# Copyright (c) 2020, Ana Karen Reyna Rivas
# All rights reserved.

# This source code is licensed under the BSD-style license found in the LICENSE file in the root directory of this source tree.
##################################################################################################

# Save to Data Frame
saveFinalSummaryTable_rf_TT = function(dataframeValues, x, TRB, model,
                                    accuracy, kappa_model1,
                                    sensitivity_model1, specificity_model1,
                                    varImpMatrix2) {
  # Order Matrix in Ascendant
  varImpMatrix <- as.matrix(varImpMatrix2[order(varImpMatrix2[,1],decreasing=TRUE),])
  
  varImportance <- ""
  
  for(i in 1:nrow(varImpMatrix)) {
    varName <- ""
    var <- attributes(varImpMatrix)$dimnames[[1]][i]
    if(var == "SumNarcissisticPI")
      varName = "Narcissism"
    else if(var == "SumEthicsPositionsRelativismQs")
      varName = "Relativism"
    else if(var == "MeanOpenness")
      varName = "Openness"
    varImportance <- paste(varImportance, varName, "(", 
               format(round(varImpMatrix[c(i), c(1)], 2), nsmall = 2),
               ")", sep = "")
    if(i != nrow(varImpMatrix))
      varImportance <- paste(varImportance, ", ", sep = "")
  }
  
  i <- 0
  dataframeValues[x, i <- i + 1] = TRB
  dataframeValues[x, i <- i + 1] = model
  dataframeValues[x, i <- i + 1] = paste(accuracy, '%') 
  dataframeValues[x, i <- i + 1] = kappa_model1
  dataframeValues[x, i <- i + 1] = sensitivity_model1
  dataframeValues[x, i <- i + 1] = specificity_model1
  dataframeValues[x, i <- i + 1] = varImportance
  
  return (dataframeValues)
}

# Save to Data Frame
saveFinalSummaryTable_rf_TT_2 = function(dataframeValues, x, TRB,
                                       accuracy_model1, kappa_model1,
                                       accuracy_model2, kappa_model2) {
  i <- 0
  dataframeValues[x, i <- i + 1] = TRB
  dataframeValues[x, i <- i + 1] = paste(accuracy_model1, '%') 
  dataframeValues[x, i <- i + 1] = kappa_model1
  dataframeValues[x, i <- i + 1] = paste(accuracy_model2, '%') 
  dataframeValues[x, i <- i + 1] = kappa_model2
  
  return (dataframeValues)
}

# Get Model 1
get_model1_rf_TT <- function(version, seed, data_train, trControl){
  # Set Random Seed to Insure You Can Reproduce Results
  set.seed(seed)
  
  # Train Model
  fit <- train(SplitEntrepreneurIntention ~ . - Typerulebreaking - SplitEntrepreneurIntention,
               data = data_train,
               method = "rf",
               metric = "Accuracy",
               trControl = trControl)
  
  # Print Results
  print(fit)
  
  # Get and Format mtry, Accuracy, Kappa Values
  best_mtry <- fit$bestTune$mtry
  bestFit <- filter(fit$results, mtry == best_mtry)
  best_accuracy <- bestFit$Accuracy * 100
  best_accuracy <- format(round(best_accuracy, 2), nsmall = 2)
  best_kappa <- bestFit$Kappa
  best_kappa <- format(round(best_kappa, 2), nsmall = 2)
  
  # Return Object with Model, Accuracy, Kappa, mtry
  model1 <- list(fit = fit,
                 accuracyTest = best_accuracy,
                 kappaTest = best_kappa,
                 bestMtry = best_mtry)
  
  return (model1)
}

# Get Model 2
get_model2_rf_TT <- function(version, seed, fit, data_train, trControl){
  # Set Random Seed to Insure You Can Reproduce Results
  set.seed(seed)
  
  # Find the Best mtry
  print("Find Best mtry")
  tuneGrid <- expand.grid(.mtry = c(2:15))
  rf_mtry <- train(SplitEntrepreneurIntention ~ . - Typerulebreaking - SplitEntrepreneurIntention,
                   data = data_train,
                   method = "rf",
                   metric = "Accuracy",
                   tuneGrid = tuneGrid,
                   trControl = trControl,
                   importance = TRUE,
                   nodesize = 14,
                   ntree = 300)
  
  # Print Results
  print(rf_mtry)
  
  # Get Best mtry Value
  best_mtry <- rf_mtry$bestTune$mtry 
  print(paste("Best mtry: ", best_mtry))
  
  cat("\n")
  
  # Find the Best maxnodes
  print("Find Best maxnodes")
  store_maxnode <- list()
  tuneGrid <- expand.grid(.mtry = best_mtry)
  for (maxnodes in c(2: 15)) {
    # Set Random Seed to Insure You Can Reproduce Results
    set.seed(seed)
    # Train Model
    rf_maxnode <- train(SplitEntrepreneurIntention ~ . - Typerulebreaking - SplitEntrepreneurIntention,
                        data = data_train,
                        method = "rf",
                        metric = "Accuracy",
                        tuneGrid = tuneGrid,
                        trControl = trControl,
                        importance = TRUE,
                        nodesize = 14,
                        maxnodes = maxnodes,
                        ntree = 300)
    current_iteration <- toString(maxnodes)
    store_maxnode[[current_iteration]] <- rf_maxnode
  }
  # Arrange Results of the Model
  results_mtry <- resamples(store_maxnode)
  # Print Summary
  print(summary(results_mtry))
  
  # Get Best maxnodes Value
  best_maxnodes <- 0
  accuracy1 <- 0
  for (maxnodes in c(2: 15)) {
    current_iteration <- toString(maxnodes)
    accuracy <- store_maxnode[[current_iteration]]$results$Accuracy
    if(accuracy > accuracy1){
      accuracy1 <- store_maxnode[[current_iteration]]$results$Accuracy
      best_maxnodes <- as.numeric(current_iteration)
    }
  }
  print(paste("Best Accuracy for maxnodes: ", accuracy1))
  print(paste("Best maxnodes: ", best_maxnodes))
  
  cat("\n")
  
  # Find the Best ntrees
  print("Find Best ntrees")
  store_maxtrees <- list()
  for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
    # Set Random Seed to Insure You Can Reproduce Results
    set.seed(seed)
    # Train Model
    rf_maxtrees <- train(SplitEntrepreneurIntention ~ . - Typerulebreaking - SplitEntrepreneurIntention,
                         data = data_train,
                         method = "rf",
                         metric = "Accuracy",
                         tuneGrid = tuneGrid,
                         trControl = trControl,
                         importance = TRUE,
                         nodesize = 14,
                         maxnodes = best_maxnodes,
                         ntree = ntree)
    key <- toString(ntree)
    store_maxtrees[[key]] <- rf_maxtrees
  }
  # Arrange Results of the Model
  results_tree <- resamples(store_maxtrees)
  # Print Summary
  print(summary(results_tree))
  
  # Get Best ntree Value
  best_ntree <- 0
  accuracy1 <- 0
  for (ntree in c(250, 300, 350, 400, 450, 500, 550, 600, 800, 1000, 2000)) {
    current_iteration <- toString(ntree)
    accuracy <- store_maxtrees[[current_iteration]]$results$Accuracy
    if(accuracy > accuracy1){
      accuracy1 <- store_maxtrees[[current_iteration]]$results$Accuracy
      best_ntree <- as.numeric(current_iteration)
    }
  }
  print(paste("Best Accuracy for ntree: ", accuracy1))
  print(paste("Best ntree: ", best_ntree))
  
  # Best Control Parameters Summary
  cat("\n")
  print("Best Control Parameters Summary")
  print(paste("Best mtry: ", best_mtry))
  print(paste("Best maxnodes: ", best_maxnodes))
  print(paste("Best ntree: ", best_ntree))
  
  # Train RF with the Best Barameters = Final Model
  rf_fit <- train(SplitEntrepreneurIntention ~ . - Typerulebreaking - SplitEntrepreneurIntention,
                  data = data_train,
                  method = "rf",
                  metric = "Accuracy",
                  tuneGrid = tuneGrid,
                  trControl = trControl,
                  importance = TRUE,
                  nodesize = 14,
                  ntree = best_ntree,
                  maxnodes = best_maxnodes)
  
  # Print Results
  print(rf_fit)
  
  # Get and Format mtry, Accuracy, Kappa Values
  best_mtry <- rf_fit$bestTune$mtry
  bestFit <- filter(rf_fit$results, mtry == best_mtry)
  best_accuracy <- bestFit$Accuracy * 100
  best_accuracy <- format(round(best_accuracy, 2), nsmall = 2)
  best_kappa <- bestFit$Kappa
  best_kappa <- format(round(best_kappa, 2), nsmall = 2)
  
  # Return Object with Model, Accuracy, Kappa, CP
  model2 <- list(fit = rf_fit,
                 accuracyTest = best_accuracy,
                 kappaTest = best_kappa,
                 bestMtry = best_mtry)
  
  return (model2)
}