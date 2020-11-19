##################################################################################################
# Copyright (c) 2020, Ana Karen Reyna Rivas
# All rights reserved.

# This source code is licensed under the BSD-style license found in the LICENSE file in the root directory of this source tree.
##################################################################################################

# Color Format
colFmt = function(x,color){
  outputFormat = knitr::opts_knit$get("rmarkdown.pandoc.to")
  if(outputFormat == 'latex')
    paste("\\textcolor{",color,"}{",x,"}",sep="")
  else if(outputFormat == 'html')
    paste("<font color='",color,"'>",x,"</font>",sep="")
  else
    x
}

# Shuffle Data
shuffle_data <- function(df_final, seed){
  # Set Random Seed to Insure You Can Reproduce Results
  set.seed(seed)
  
  # Get Random Sample
  # Code obtained from [@Guru99DT].
  shuffle_index <- sample(1:nrow(df_final))
  head(shuffle_index)
  
  # Shuffle Data Frame
  # Code obtained from [@Guru99DT].
  df_final <- df_final[shuffle_index, ]
  head(df_final)
  
  return (df_final)
}

# Clean Data
clean_data <- function(df_final, variables){
  # Drop Variables
  # Code obtained from [@Guru99DT].
  clean_df_final <- df_final %>%
    dplyr::select(c(variables)) %>%
    # Remove NA observations
    na.omit()
  
  # Convert to Factor Level and Change Column Cames
  # SplitEntrepreneurIntention
  if ("SplitEntrepreneurIntention" %in% colnames(clean_df_final)){
    clean_df_final <- clean_df_final %>% mutate(SplitEntrepreneurIntention = factor(SplitEntrepreneurIntention, levels = c(1, 2), labels = c('low', 'high')))
  }
  
  # Age
  if ("v_2" %in% colnames(clean_df_final)){
    names(clean_df_final)[names(clean_df_final) == "v_2"] <- "Age"
  }
  # Sex
  if ("v_3" %in% colnames(clean_df_final)){
    clean_df_final <- clean_df_final %>% mutate(v_3 = factor(v_3, levels = c(1, 2, 3), labels = c('Female', 'Male', 'Other')))
    names(clean_df_final)[names(clean_df_final) == "v_3"] <- "Sex"
  }
  
  # Typerulebreaking
  if ("Typerulebreaking" %in% colnames(clean_df_final)){
    clean_df_final <- clean_df_final %>% mutate(Typerulebreaking = factor(Typerulebreaking, levels = c(1, 2), labels = c('URF', 'CRB')))
  }
  
  # SumNarcissisticPI
  if ("SumNarcissisticPI" %in% colnames(clean_df_final)){
    names(clean_df_final)[names(clean_df_final) == "SumNarcissisticPI"] <- "Narcissism"
  }
  # SumEthicsPositionsRelativismQs
  if ("SumEthicsPositionsRelativismQs" %in% colnames(clean_df_final)){
    names(clean_df_final)[names(clean_df_final) == "SumEthicsPositionsRelativismQs"] <- "Relativism"
  }
  # MeanOpenness
  if ("MeanOpenness" %in% colnames(clean_df_final)){
    names(clean_df_final)[names(clean_df_final) == "MeanOpenness"] <- "Openness"
  }
  
  # Print Data
  glimpse(clean_df_final)
  
  return (clean_df_final)
}

# Create Train/Test
create_train_test <- function(data, classVariable,size, seed) {
  # Set Random Seed to Insure You Can Reproduce Results
  set.seed(seed)
  
  # Create size/-size Split into Test and Train
  data_train <- data %>%
    group_by(!!! rlang::syms(classVariable)) %>%
    sample_frac(size)
  data_test <- anti_join(data, data_train)
  
  # Count Types in Whole Data Frame
  data %>%
    count(c(classVariable))
  
  # Count Types in data_train
  data_train %>%
    count(c(classVariable))
  
  # Count Types in data_test
  data_test %>%
    count(c(classVariable))
  
  # Object to Return
  trainTestList <- list(data_train = data_train, data_test = data_test)
  
  return (trainTestList)
}

# Stratified Sample
stratified_sample_TT = function(clean_df_final, classVariable, size, seed){
  # Create Stratified Samples
  # Code obtained from [@DataCampStratifiedSample] and [@EmblemStratifiedSample2018].
  data_train_test <- create_train_test(clean_df_final, classVariable, size, seed)
  data_train <- data_train_test$data_train
  data_test <- data_train_test$data_test
  
  # Object to Return
  trainTestList <- list(data_train = data_train, 
                        data_test = data_test)
  
  return (trainTestList)
}

# Evaluate Model Performance
evaluateModel <- function(fit, data_test, seed) {
  # Set Random Seed to Insure You Can Reproduce Results
  set.seed(seed)
  
  # Predict Model
  predict_unseen <- predict(fit, data_test)
  
  # Confusion Matrix
  confusion_matrix <- confusionMatrix(predict_unseen, data_test$SplitEntrepreneurIntention,
                                      positive = 'high')
  print(confusion_matrix)
  
  # Get and Format Values
  accuracy <- format(round(confusion_matrix$overall['Accuracy'] * 100, 2), nsmall = 2)
  kappa <- format(round(confusion_matrix$overall['Kappa'], 2), nsmall = 2)
  sensitivity <- format(round(confusion_matrix$byClass['Sensitivity'], 2), nsmall = 2)
  specificity <- format(round(confusion_matrix$byClass['Specificity'], 2), nsmall = 2)
  
  # Object to Return
  modelEvaluationMeasures <- list(accuracy = accuracy, kappa = kappa,
                                  sensitivity = sensitivity, specificity = specificity)
  
  return (modelEvaluationMeasures)
}