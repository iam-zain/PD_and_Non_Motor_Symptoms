setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")

###############  Logistic Regression  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 0
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 1
control <- trainControl(method="cv",number=10,savePredictions="all",classProbs=TRUE)

output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  
train_index <- sample(1:nrow(NonMot1), 0.8*nrow(NonMot1))
train_data <- NonMot1[train_index,]
test_data <- NonMot1[-train_index,]

log_model <- train(APPRDX ~., data=train_data, method="glm", family=binomial, trControl=control)
predictions <- predict(log_model, newdata=test_data)
log_prediction <- ifelse(predictions > 0.5, 1, 0)
con_matrix <- table(log_prediction, test_data[,1])
accuracy <- sum(diag(con_matrix))/sum(con_matrix)
print (paste ('Accuracy: ', accuracy))
output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100LogReg.csv", row.names = F)
#summary(output)





###############  Random Forest  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method="cv",number=10,savePredictions="all",classProbs=TRUE)

cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  
  train_index <- sample(1:nrow(NonMot1), 0.8*nrow(NonMot1))
  train_data <- NonMot1[train_index,]
  test_data <- NonMot1[-train_index,]
  rf_model <- train(APPRDX ~., data=train_data, method="rf", trControl=control)
  print(rf_model)
  acc <- rf_model$results[1,2]
  output[i,1] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100RF.csv", row.names = F)









###############  glmnet / Elastic Net  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  
train_index <- sample(1:nrow(NonMot1), 0.8*nrow(NonMot1))
train_data <- NonMot1[train_index,]
test_data <- NonMot1[-train_index,]

#enet_hits <- train(APPRDX ~., data = train_data, method ="glmnet", trControl = control)
#enet_hits
## Accuracy was used to select the optimal model using the highest value.

enet_hits_int = train(APPRDX ~ . ^ 2, data = train_data,method = "glmnet", trControl = control, tuneLength = 10)

get_best_result = function(caret_fit) {
  best = which(rownames(caret_fit$results) == rownames(caret_fit$bestTune))
  best_result = caret_fit$results[best, ]
  rownames(best_result) = NULL
  best_result
}

get_best_result(enet_hits_int)

calc_acc = function(actual, predicted) {
  mean(actual == predicted)
}

accuracy = calc_acc(actual = test_data$APPRDX, predicted = predict(enet_hits_int, newdata = test_data))

print (paste ('Accuracy: ', accuracy))
output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100glmnet.csv", row.names = F)






###############  Ada Boost  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  
train_index <- sample(1:nrow(NonMot1), 0.8*nrow(NonMot1))
train_data <- NonMot1[train_index,]
test_data <- NonMot1[-train_index,]

fitGrid <- expand.grid(mfinal = (1:3)*3, maxdepth = c(1, 3), coeflearn = c("Breiman"))
ada_model <- train(APPRDX ~.,data = train_data, method ="AdaBoost.M1", trControl = control, tuneGrid = fitGrid, verbose = TRUE)
predictions <- predict(ada_model, newdata=test_data)
con_matrix <- table(predictions, test_data[,1])
accuracy <- sum(diag(con_matrix))/sum(con_matrix)
print (paste ('Accuracy: ', accuracy))
output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100AdaBoost.csv", row.names = F)







###############  XG Boost  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  
train_index <- sample(1:nrow(NonMot1), 0.8*nrow(NonMot1))
train_data <- NonMot1[train_index,]
test_data <- NonMot1[-train_index,]

fitGrid <- expand.grid(nrounds = 200,max_depth = 5,eta = 0.05,gamma = 0.01,
                         colsample_bytree = 0.75,min_child_weight = 0,subsample = 0.5)
XG_model <- train(APPRDX ~.,data = train_data, method ="xgbTree", trControl = control, tuneGrid = fitGrid, verbose = TRUE)
predictions <- predict(XG_model, newdata=test_data)
con_matrix <- table(predictions, test_data[,1])
accuracy <- sum(diag(con_matrix))/sum(con_matrix)
print (paste ('Accuracy: ', accuracy))
output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100XGBoost.csv", row.names = F)







###############  Neural Network  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  
train_index <- sample(1:nrow(NonMot1), 0.8*nrow(NonMot1))
train_data <- NonMot1[train_index,]
test_data <- NonMot1[-train_index,]

NeuralNet <- train(APPRDX ~.,data = train_data, method ="nnet", trControl = control, na.action = na.omit, trace = F)
predictions <- predict(NeuralNet, newdata=test_data)
con_matrix <- table(predictions, test_data[,1])
accuracy <- sum(diag(con_matrix))/sum(con_matrix)
print (paste ('Accuracy: ', accuracy))
output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100NeuralNet.csv", row.names = F)








###############  LDA  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:30){
  print(i)
  
  train_index <- sample(1:nrow(NonMot1), 0.8*nrow(NonMot1))
  train_data <- NonMot1[train_index,]
  test_data <- NonMot1[-train_index,]
  
  LDA_model <- train(APPRDX ~.,data = train_data, method ="lda", trControl = control, metric = 'Accuracy')
  predictions <- predict(LDA_model, newdata=test_data)
  con_matrix <- table(predictions, test_data[,1])
  accuracy <- sum(diag(con_matrix))/sum(con_matrix)
  print (paste ('Accuracy: ', accuracy))
  output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100LDA.csv", row.names = F)





###############  knn  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'

output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  
  NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'
  train_index <- sample(1:nrow(NonMot1), 0.8*nrow(NonMot1))
  train_data <- NonMot1[train_index,]
  test_data <- NonMot1[-train_index,]
  
  knn_model = train(APPRDX ~ ., data = train_data, method = "knn", trControl = control, 
                    preProcess = c("center", "scale"), tuneLength = 10)
  predictions <- predict(knn_model, newdata=test_data)
  con_matrix <- table(predictions, test_data[,1])
  accuracy <- sum(diag(con_matrix))/sum(con_matrix)
  print (paste ('Accuracy: ', accuracy))
  output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100knn.csv", row.names = F)






###############  svm  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  
train_index <- sample(1:nrow(NonMot1), 0.8*nrow(NonMot1))
train_data <- NonMot1[train_index,]
test_data <- NonMot1[-train_index,]

svm_model <- train(APPRDX ~.,data = train_data, method ="svmLinear", trControl = control)
predictions <- predict(svm_model, newdata=test_data)
con_matrix <- table(predictions, test_data[,1])
accuracy <- sum(diag(con_matrix))/sum(con_matrix)
print (paste ('Accuracy: ', accuracy))
output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100svmLinear.csv", row.names = F)


### radial ###
output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  
train_index <- sample(1:nrow(NonMot1), 0.8*nrow(NonMot1))
train_data <- NonMot1[train_index,]
test_data <- NonMot1[-train_index,]
  
svm_model <- train(APPRDX ~.,data = train_data, method ="svmRadial", trControl = control)
predictions <- predict(svm_model, newdata=test_data)
con_matrix <- table(predictions, test_data[,1])
accuracy <- sum(diag(con_matrix))/sum(con_matrix)
print (paste ('Accuracy: ', accuracy))
output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100svmRadial.csv", row.names = F)


### poly ###
output <- data.frame(matrix(ncol=1,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  
train_index <- sample(1:nrow(NonMot1), 0.8*nrow(NonMot1))
train_data <- NonMot1[train_index,]
test_data <- NonMot1[-train_index,]
  
svm_model <- train(APPRDX ~.,data = train_data, method ="svmPoly", trControl = control)
predictions <- predict(svm_model, newdata=test_data)
con_matrix <- table(predictions, test_data[,1])
accuracy <- sum(diag(con_matrix))/sum(con_matrix)
print (paste ('Accuracy: ', accuracy))
output[i,1] <- accuracy
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_100svmPoly.csv", row.names = F)









############      Plot    ###############

setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")

Non_Mot = read.csv("ModelClassification_45Feats_DiffMethods.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('knn', 'LogisticRegression', 'NeuralNetwork', 'LDA', 'svmLinear',	
                                                    'svmRadial','svmPoly','RandomForest','glmnet', 'AdaBoost','XGBoost')), 
                         y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") +
  theme(axis.text.x = element_text(size=18,angle = 50, vjust = 0.99, hjust=1,color="black")) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("Model Classification - Various Methods - Patient Vs Healthy Control")


