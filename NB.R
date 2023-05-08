NB <- function(undersampling) {
  
  # Load required packages
  library(caret)
  library(tidyr)
  library(klaR)
  library(e1071)
  library(dplyr)
  
  # setting seed for reproducibility
  set.seed(1234)

  #converting columns to required type
  undersampling$weight_KG <- as.numeric(undersampling$weight_KG)
  undersampling$height_Cm <- as.numeric(undersampling$height_Cm)
  undersampling$COVID_Positive_Flag <- factor(undersampling$COVID_Positive_Flag)
  undersampling$Disease <- factor(undersampling$Disease)
  # convert columns symptom columns to factors
  undersampling[, 5:length(undersampling)] <- lapply(undersampling[, 5:length(undersampling)], factor)
  
  
  # Create a training and test set
  train_idx <- sample(nrow(undersampling), nrow(undersampling) * 0.6)
  train_data <- undersampling[train_idx, ]
  test_data <- undersampling[-train_idx, ]
  
  # Assigning features and outcomes
  x = train_data[, 2:ncol(train_data)]
  y = train_data$Disease
  
  
  # create unique configuration which will be shared across all classification NB_models 
  ctrl <- trainControl(
    method = "cv", # used for configuring resampling method: in this case cross validation 
    number = 5, # instruct that it is 5 fold-cv
    classProbs = TRUE, 
    verboseIter = TRUE, # print output of each step
    savePredictions = TRUE)
  

  # Suppress warning Function
  suppressWarnings({
    
    set.seed(1234)
    # Train the NB_model
    NB_model <- train(x = train_data[, 2:ncol(train_data)], y = train_data$Disease, method = "nb", trControl = ctrl)
    
    #NB_model <- train(x = train_data[, 2:ncol(train_data)], y = train_data$Disease, method = "nb")
    
    # Print the NB_model
    print(NB_model)

    #Training Evalualtion
    print("Training Stat")
    
    # Extract evaluation metrics from the NB_model object
    accuracy <- NB_model$results[which.max(NB_model$results$Accuracy), "Accuracy"]
    sensitivity <- NB_model$results[which.max(NB_model$results$Accuracy), "Sensitivity"]
    specificity <- NB_model$results[which.max(NB_model$results$Accuracy), "Specificity"]
    kappa <- NB_model$results[which.max(NB_model$results$Accuracy), "Kappa"]
    
    # Print the evaluation metrics
    print(paste0("Accuracy: ", round(accuracy, 2)))
    print(paste0("Sensitivity: ", (sensitivity)))
    print(paste0("Specificity: ", specificity))
    print(paste0("Kappa: ", (kappa)))
    
    # evaluate on test data
    predicted_labels <- predict(NB_model, newdata = test_data)
    confusion_matrix <- table(predicted_labels, test_data$Disease)
    
    print("Test Accuracy")
  
    accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
    sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
    specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
    
    
    # Print the evaluation metrics
    print(paste0("Accuracy: ", round(accuracy, 2)))
    print(paste0("Sensitivity: ", round(sensitivity, 2)))
    print(paste0("Specificity: ", round(specificity, 2)))
    
  })
  
  return(NULL)
  
}
