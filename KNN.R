KNN <- function(undersampling) {
  
  #import required library
  library(caret)
  library(ggplot2)
  
  #converting columns to required type
  undersampling$weight_KG <- as.numeric(undersampling$weight_KG)
  undersampling$height_Cm <- as.numeric(undersampling$height_Cm)
  undersampling$COVID_Positive_Flag <- factor(undersampling$COVID_Positive_Flag)
  undersampling$Disease <- factor(undersampling$Disease)
  # convert columns symptom columns to factors
  undersampling[, 5:length(undersampling)] <- lapply(undersampling[, 5:length(undersampling)], factor)
  
  #scaling numerical columns since KNN is based on Eucliedean distance
  undersampling$height_Cm <- scale(undersampling$height_Cm)
  undersampling$weight_KG <- scale(undersampling$weight_KG)
  
  
  # Create a training and test set
  trainIndex <- createDataPartition(undersampling$Disease, p = .6, list = FALSE)
  
  # Splitting the training and test set based on index
  train <- undersampling[trainIndex, ]
  test <- undersampling[-trainIndex, ]
  
  
  # Assigning features and outcomes
  x = train[,2:ncol(train)]
  y = train$Disease
  
  
  # Train the KNN model with 1-10 neighbors and cross validation folds as 5
  knn_model <- train(x=x,y=y, method = "knn", trControl = trainControl(method = "cv", number = 5),tuneGrid = expand.grid(k = 1:30),metric = "Accuracy")
  
  
  # Make predictions on the test set
  print(knn_model)
  
  
  
  #Training Evalualtion
  print("Training Stat")
  
  # Extract evaluation metrics from the knn_model object
  accuracy <- knn_model$results[which.max(knn_model$results$Accuracy), "Accuracy"]
  sensitivity <- knn_model$results[which.max(knn_model$results$Accuracy), "Sensitivity"]
  specificity <- knn_model$results[which.max(knn_model$results$Accuracy), "Specificity"]
  kappa <- knn_model$results[which.max(knn_model$results$Accuracy), "Kappa"]
  
  
  # Print the evaluation metrics
  print(paste0("Accuracy: ", round(accuracy, 2)))
  print(paste0("Sensitivity: ", (sensitivity)))
  print(paste0("Specificity: ", specificity))
  print(paste0("Kappa: ", (kappa)))
  
  
  print("Testing Model")
  # Make predictions on the test set
  knn_preds <- predict(knn_model, newdata = test)
  
  # Evaluate the model test performance
  confusion_matrix <- table(knn_preds, test$Disease)
  
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  sensitivity <- confusion_matrix[2, 2] / sum(confusion_matrix[2, ])
  specificity <- confusion_matrix[1, 1] / sum(confusion_matrix[1, ])
  
  
  # Print the evaluation metrics
  print(paste0("Accuracy: ", round(accuracy, 2)))
  print(paste0("Sensitivity: ", round(sensitivity, 2)))
  print(paste0("Specificity: ", round(specificity, 2)))
  
  
  # Define color palette
  colors <- c("#0072B2", "#D55E00")
  
  # Convert the model evaluation results to dataframe
  df= data.frame(knn_model$results)
  
  # Create line plot
  p <- ggplot(df, aes(x = k)) +
    geom_line(aes(y = Accuracy, color = "Accuracy"), size = 1.5) +
    geom_line(aes(y = Kappa, color = "Kappa"), size = 1.5) +
    scale_color_manual(values = colors) +
    labs(x = "K Neighbours", y = "Values (5 fold Cross Validation)") +
    ggtitle("KNN Model Evaluation") +
    theme_bw(base_size = 18) +
    theme(plot.title = element_text(face = "bold", size = 20)) +
    scale_x_continuous(breaks = 1:30)
  
  
  
  plot(p)
  
  return(NULL)
  
}