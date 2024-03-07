setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")

###############  Single Run - LASSO ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot2 <- NonMot1  %>% select(-c(1)) #Remove PATNO

train_index <- sample(1:nrow(NonMot2), 0.8*nrow(NonMot2))
train_data <- NonMot2[train_index,]
test_data <- NonMot2[-train_index,]

fit <- cv.glmnet(as.matrix(train_data[,-1]), train_data[,1], family = 'binomial', type.measure = 'class', alpha = 1)

predictions <- predict(fit, newx = as.matrix(test_data[,-1]), type = 'class')

confusion_matrix <- table(predictions, test_data[,1])
colnames(confusion_matrix) <- c('Predicted Negative','Predicted Positive')
rownames(confusion_matrix) <- c('Actual negative', 'Actual Positive')
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print (paste ('Accuracy: ', accuracy))

###############  100 times Run - LASSO  ##############
output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
train_index <- sample(1:nrow(NonMot2), 0.8*nrow(NonMot2))
train_data <- NonMot2[train_index,]
test_data <- NonMot2[-train_index,]

fit <- cv.glmnet(as.matrix(train_data[,-1]), train_data[,1], family = 'binomial', type.measure = 'class', alpha = 1)

predictions <- predict(fit, newx = as.matrix(test_data[,-1]), type = 'class')

confusion_matrix <- table(predictions, test_data[,1])
colnames(confusion_matrix) <- c('Predicted Negative','Predicted Positive')
rownames(confusion_matrix) <- c('Actual negative', 'Actual Positive')
print(confusion_matrix)
accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
print (paste ('Accuracy: ', accuracy))

output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100LASSO.csv", row.names = F)



###############  100 times Run - Elastic Net  ##############
output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  train_index <- sample(1:nrow(NonMot2), 0.8*nrow(NonMot2))
  train_data <- NonMot2[train_index,]
  test_data <- NonMot2[-train_index,]
  
  fit <- cv.glmnet(as.matrix(train_data[,-1]), train_data[,1], family = 'binomial', type.measure = 'class', alpha = 0.5)
  
  predictions <- predict(fit, newx = as.matrix(test_data[,-1]), type = 'class')
  
  confusion_matrix <- table(predictions, test_data[,1])
  colnames(confusion_matrix) <- c('Predicted Negative','Predicted Positive')
  rownames(confusion_matrix) <- c('Actual negative', 'Actual Positive')
  print(confusion_matrix)
  accuracy <- sum(diag(confusion_matrix)) / sum(confusion_matrix)
  print (paste ('Accuracy: ', accuracy))
  
  output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100enet.csv", row.names = F)






###################### to determine alpha value suitable for my datset ##########
# Cross-validation to determine the optimal value of alpha
alpha_values <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
best_alpha <- 0
best_error <- Inf
for (alpha in alpha_values) {
  fit_cv <- cv.glmnet(as.matrix(train_data[,-1]), train_data[,1], family = "binomial", type.measure = "class", alpha = alpha)
  if (fit_cv$cvm[which.min(fit_cv$cvm)] < best_error) {
    best_alpha <- alpha
    best_error <- fit_cv$cvm[which.min(fit_cv$cvm)]
  }
}
print(paste("Optimal value of alpha: ", best_alpha))

# Grid search to determine the optimal value of alpha
alpha_values <- c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)
best_alpha <- 0
best_accuracy <- 0
for (alpha in alpha_values) {
  fit <- glmnet(as.matrix(train_data[,-1]), train_data[,1], family = "binomial", alpha = alpha)
  predictions <- predict(fit, newx = as.matrix(test_data[,-1]), type = "class")
  accuracy <- sum(predictions == test_data[,1])/nrow(test_data)
  if (accuracy > best_accuracy) {
    best_alpha <- alpha
    best_accuracy <- accuracy
  }
}

# Print the optimal value of alpha
print(paste("Optimal value of alpha (Cross-validation): ", optimal_alpha_cv))
print(paste("Optimal value of alpha (Grid search): ", best_alpha))






############  Neural Network ###########
library(neuralnet)
#Define the neural network architecture
NeuralNetwork <- neuralnet(formula = APPRDX ~.,data = train_data,hidden = c(10, 5),linear.output = TRUE,threshold = 0.05)

# Make predictions on the test set
predictions <- compute(NeuralNetwork, test_data[,-1])
predictions <- round(predictions$net.result)

# Evaluate the model
accuracy <- sum(predictions == test_data[,1])/nrow(test_data)
print(paste("Accuracy: ", accuracy))
plot(NeuralNetwork)

