RF_R <- function(undersampling) {
  
  # Load required packages
  library(caret)
  library(tidyverse)
  library(randomForest)
  library(dplyr)
  
  # setting seed for reproducibility
  set.seed(1234)
  
  #converting columns to required type
  undersampling$COVID_Positive_Flag <- factor(undersampling$COVID_Positive_Flag)
  undersampling$Disease <- factor(undersampling$Disease)
  # convert columns symptom columns to factors
  undersampling[, 5:length(undersampling)] <- lapply(undersampling[, 5:length(undersampling)], factor)
  
  # Create a training and test set
  train_idx <- sample(nrow(undersampling), nrow(undersampling) * 0.6)
  train_data <- undersampling[train_idx, ]
  test_data <- undersampling[-train_idx, ]
  
  # Assigning features and outcomes
  x = train_data[,2:length(train_data)]
  y = train_data$Disease
  
  # setting seed for reproducibility
  set.seed(1234)
  
  # setting train control and number of folds and number of trees
  control <- trainControl(method="cv", number=5)
  metric <- "Accuracy"
  ntree= 50
  
  # building random forest model on the basis of tuning grid
  rf_default <- train(x = train_data[,2:length(train_data)], y = train_data$Disease, method = "rf", ntree = ntree,
                      tuneGrid = data.frame(mtry = 3),trControl=control)
  
  #printing the model
  print(rf_default)
  
  #visualizing the OOB error rate
  oob_df = data.frame(x = 1:ntree, oob = rf_default$finalModel$err.rate[,1])
  lp = ggplot(oob_df, aes(x = x, y=oob))+
    geom_line()+
    xlab("Number of Trees")+
    ylab ("OOB Error Rate")+
    theme_classic()+
    ggtitle('Random Forest OOB error rate', subtitle = 'Random Forest')
  
  # plot of number of trees vs. OOB error rate
  plot(lp)
 
  
  
  # # obtain OOB error rate for selected model
  print(rf_default$results$Accuracy)
  
  # selecting variable importance
  imp = varImp(rf_default$finalModel)
  
  # sorting the variable importance
  imp$feature = row.names(imp)
  imp = imp[order(imp$Overall, decreasing= TRUE),]
  
  # selecting top 15 variable importance
  select_top = 15
  imp = imp[1:select_top, ]
  imp = imp[order(imp$Overall, decreasing= FALSE),]
  
  # plotting variable importance using ggplot
  imp$feature = factor(imp$feature, levels = imp$feature)
  
  p = ggplot(imp, aes(x = feature, y = Overall),size = 14)+
    geom_bar(stat = 'identity', fill='#215DAF')+
    ggtitle('Variable importance plot', subtitle = 'Random Forest')+
    coord_flip()+
    xlab('Variable importance')+
    theme_classic()+
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))+
    theme(plot.title = element_text(face = "bold", size = 20))
  
  
  plot(p)

  #Training Evalualtion
  print("Training Stat")
  
  # Extract evaluation metrics from the rf_default object
  accuracy <- rf_default$results[which.max(rf_default$results$Accuracy), "Accuracy"]
  sensitivity <- rf_default$results[which.max(rf_default$results$Accuracy), "Sensitivity"]
  specificity <- rf_default$results[which.max(rf_default$results$Accuracy), "Specificity"]
  kappa <- rf_default$results[which.max(rf_default$results$Accuracy), "Kappa"]
  
  
  # Print the evaluation metrics
  print(paste0("Accuracy: ", round(accuracy, 2)))
  print(paste0("Sensitivity: ", (sensitivity)))
  print(paste0("Specificity: ", specificity))
  print(paste0("Kappa: ", (kappa)))
  
  
  
  # Evaluate the model on the test set
  print("Testing Model")
  
  # predict on test data
  rf_preds <- predict(rf_default, test_data)
  
  # evaluate test data model performance from confusion matrix
  confusion_matrix <- table(rf_preds, test_data$Disease)
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