GBM <- function(undersampling) {
  
  #import required library
  library(caret)
  library(tidyverse)
  library(e1071)
  library(gbm)
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
  
  
  # setting seed for reproducibility
  set.seed(1234)
  gbmGrid <-  expand.grid(interaction.depth = c(1, 5, 9),
                          n.trees = (1:30),
                          shrinkage = 0.1,
                          n.minobsinnode = 40)
  
  # Assigning features and outcomes
  x = train_data[, 2:ncol(train_data)]
  y = train_data$Disease
  
  # building boosted tree model on the basis of tuning grid, trcontrol
  model_boostclass <- train(x = x, y = y, verbose=FALSE,
                            method = "gbm", tuneGrid = gbmGrid, 
                            trControl = trainControl(method = "cv", number = 5, 
                                                     classProbs = TRUE, savePredictions = TRUE))
  
  # print best tune model
  print(model_boostclass$bestTune)
  
  
  #Training Evalualtion
  print("Training Stat")
  
  # Extract evaluation metrics from the model_boostclass object
  accuracy <- model_boostclass$results[which.max(model_boostclass$results$Accuracy), "Accuracy"]
  sensitivity <- model_boostclass$results[which.max(model_boostclass$results$Accuracy), "Sensitivity"]
  specificity <- model_boostclass$results[which.max(model_boostclass$results$Accuracy), "Specificity"]
  kappa <- model_boostclass$results[which.max(model_boostclass$results$Accuracy), "Kappa"]
  
  
  # Print the evaluation metrics
  print(paste0("Accuracy: ", round(accuracy, 2)))
  print(paste0("Sensitivity: ", (sensitivity)))
  print(paste0("Specificity: ", specificity))
  print(paste0("Kappa: ", (kappa)))
  
  
  
  print("Testing Model")
  
  # evaluate and predict on test data
  predict_boost <- predict(model_boostclass, newdata = test_data)
  # evaluate model performance
  confusion_matrix <- table(predict_boost, test_data$Disease)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  print(paste("Accuracy:", round(accuracy, 2)))
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
  
  
  # Print the evaluation metrics
  print(paste0("Accuracy: ", round(accuracy, 2)))
  print(paste0("Sensitivity: ", round(sensitivity, 2)))
  print(paste0("Specificity: ", round(specificity, 2)))
  
  
  # extract variable importance
  imp <- summary(model_boostclass$finalModel, method = relative.influence, normalize = TRUE, las = 2)
  
  # create a data frame to store variable importance
  imp_df <- data.frame(Variable = rownames(imp), Importance = imp, row.names = NULL)
  
  # select top 15 most important factors
  select_top = 15
  imp = imp_df[1:select_top, ]
  
  imp = imp[order(imp$Importance.rel.inf, decreasing= FALSE),]
  
  print(head(imp))
  imp$Variable = factor(imp$Variable, levels = imp$Variable)
  
  # plot the relative importance plot using ggplot
  p = ggplot(imp, aes(x = Variable, y = Importance.rel.inf))+
    geom_bar(stat = 'identity', fill='#2C95D6')+
    ggtitle('Variable importance plot', subtitle = 'Gradient Boosted Model')+
    coord_flip()+
    xlab('Variable importance')+
    theme_classic()+
    theme(axis.text.x = element_text(size = 14), axis.text.y = element_text(size = 14))+
    theme(plot.title = element_text(face = "bold", size = 20))
  
  plot(p)
  
  
  #plotting the partial dependence plot of top 3 variables
  print(plot(model_boostclass$finalModel, i = "disorder_urea_cycle_metabolism"))
  print(plot(model_boostclass$finalModel, i = "hereditary_motor_and_sensory_neuropathy"))
  print(plot(model_boostclass$finalModel, i = "gastrointestinal_complication"))
  
  return (NULL)
}

