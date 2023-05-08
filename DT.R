DT_R <- function(undersampling) {
  
  # Load required packages
  library(caret)
  library(tidyverse)
  library(e1071)
  library(rpart)
  library(rpart.plot)
  
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
  
  # noting that default splitting crit based on gini
  rf_model_01 <- rpart(Disease ~ ., data = train_data, method = "class", parms = list(split = "gini"),
                       control = rpart.control(minsplit = 30, minbucket = 10), cp = 0)
  
  # getting tuning grid for train function
  tg_clatr <- data.frame(cp = rf_model_01$cptable[,1])
  
  # setting seed for reproducibility
  set.seed(1234)
  
  
  # building classification tree model on the basis of tuning grid
  ctree_10cv <- train(x = train_data[,2:length(undersampling)], y = train_data$Disease,
                      method = "rpart", parms = list(split = "gini"),
                      control = rpart.control(minsplit = 30, minbucket = 10),
                      tuneGrid = tg_clatr,
                      trControl = trainControl(method = "cv", number = 5, selectionFunction = "oneSE"))
  
  #printing model output
  print(ctree_10cv)
  
  # plot the selected classification tree
  rpart.plot(ctree_10cv$finalModel)
  
  #Training Evalualtion
  print("Training Stat")
  
  # Extract evaluation metrics from the ctree_10cv object
  accuracy <- ctree_10cv$results[which.max(ctree_10cv$results$Accuracy), "Accuracy"]
  sensitivity <- ctree_10cv$results[which.max(ctree_10cv$results$Accuracy), "Sensitivity"]
  specificity <- ctree_10cv$results[which.max(ctree_10cv$results$Accuracy), "Specificity"]
  kappa <- ctree_10cv$results[which.max(ctree_10cv$results$Accuracy), "Kappa"]
  
  
  # Print the evaluation metrics
  print(paste0("Accuracy: ", round(accuracy, 2)))
  print(paste0("Sensitivity: ", (sensitivity)))
  print(paste0("Specificity: ", specificity))
  print(paste0("Kappa: ", (kappa)))
  
  
  # predict on test data
  print("Testing Model")
  dt_preds <- predict(ctree_10cv, test_data)
  
  # evaluate test data model performance
  confusion_matrix <- table(dt_preds, test_data$Disease)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  print(paste("Accuracy:", round(accuracy, 2)))
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
  
  
  # Print the evaluation metrics
  print(paste0("Accuracy: ", round(accuracy, 2)))
  print(paste0("Sensitivity: ", round(sensitivity, 2)))
  print(paste0("Specificity: ", round(specificity, 2)))
  
  return(NULL)
  
}