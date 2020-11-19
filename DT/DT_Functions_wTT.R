##################################################################################################
# Copyright (c) 2020, Ana Karen Reyna Rivas
# All rights reserved.

# This source code is licensed under the BSD-style license found in the LICENSE file in the root directory of this source tree.
##################################################################################################

# Save to Data Frame
saveFinalSummaryTable_dt_TT = function(dataframeValues, x, TRB, model,
                                       accuracy, kappa_model1,
                                       sensitivity_model1, specificity_model1,
                                       varImpMatrix2) {
  # Order Matrix in Ascendant
  varImpMatrix <- as.matrix(varImpMatrix2[order(varImpMatrix2[,1],decreasing=TRUE),])
  
  varImportance <- ""
  
  for(i in 1:nrow(varImpMatrix)) {
    varName <- attributes(varImpMatrix)$dimnames[[1]][i]
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
saveFinalSummaryTable_dt_TT_2 = function(dataframeValues, x, TRB,
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
get_model1_dt_TT <- function(version, seed, data_train, trControl){
  # Set Random Seed to Insure You Can Reproduce Results
  set.seed(seed)
  
  # Train Model
  fit <- train(SplitEntrepreneurIntention ~ . - Typerulebreaking - SplitEntrepreneurIntention,
               data = data_train,
               method = "rpart",
               metric = "Accuracy",
               trControl = trControl)
  
  # Print Results
  print(fit)
  
  # Get and Format CP, Accuracy, Kappa Values
  best_cp <- fit$bestTune$cp
  bestFit <- filter(fit$results, cp == best_cp)
  best_accuracy <- bestFit$Accuracy * 100
  best_accuracy <- format(round(best_accuracy, 2), nsmall = 2)
  best_kappa <- bestFit$Kappa
  best_kappa <- format(round(best_kappa, 2), nsmall = 2)
  best_cp <- format(round(best_cp, 2), nsmall = 2)

  # Return Object with Model, Accuracy, Kappa, CP
  model1 <- list(fit = fit,
                 accuracyTest = best_accuracy,
                 kappaTest = best_kappa,
                 bestCP = best_cp)
  return (model1)
}

# Get Model 2
get_model2_dt_TT <- function(version, seed, fit, data_train, trControl){
  # Set Random Seed to Insure You Can Reproduce Results
  set.seed(seed)
  
  # Train Model
  dt_cp <- train(SplitEntrepreneurIntention ~ . - Typerulebreaking - SplitEntrepreneurIntention,
                   data = data_train,
                   method = "rpart",
                   metric = "Accuracy",
                   trControl = trControl,
                  tuneLength = 15,
                  control = rpart.control(minsplit = 1, minbucket = 1, maxdepth = 10))
  
  # Print Results
  print(dt_cp)
  
  # Get and Format CP, Accuracy, Kappa Values
  best_cp <- dt_cp$bestTune$cp 
  bestFit <- filter(dt_cp$results, cp == best_cp)
  best_accuracy <- bestFit$Accuracy * 100
  best_accuracy <- format(round(best_accuracy, 2), nsmall = 2)
  best_kappa <- bestFit$Kappa
  best_kappa <- format(round(best_kappa, 2), nsmall = 2)

  # Return Object with Model, Accuracy, Kappa, CP
  model2 <- list(fit = dt_cp,
                 accuracyTest = best_accuracy,
                 kappaTest = best_kappa,
                 bestCP = best_cp)

  return (model2)
}