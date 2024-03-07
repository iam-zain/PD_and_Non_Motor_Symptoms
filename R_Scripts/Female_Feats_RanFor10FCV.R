setwd("Z:\\PPMI_Data\\Excels\\Only_Female")

########################################### 1
NonMots = read.csv('NonMotor_Categorized_Female.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #54, 30

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1000){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_Female_1000RanFor10FCV.csv")


########################################### 2
NonMots = read.csv('ImpFeats_RanFor_DataFemale.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #54, 30

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1000){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "ImpFeats_RanFor_DataFemale_1000RanFor10FCV.csv")




########################################### 3
NonMots = read.csv('ImpFeats_DecTree_DataFemale.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #54, 30

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1000){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "ImpFeats_DecTree_DataFemale_1000RanFor10FCV.csv")



########################################### 4
NonMots = read.csv('ImpFeats_NMI_DataFemale.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #54, 30

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1000){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "ImpFeats_NMI_DataFemale_1000RanFor10FCV.csv")



########################################### 5
NonMots = read.csv('ImpFeats_Boruta_DataFemale.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #54, 30

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1000){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "ImpFeats_Boruta_DataFemale_1000RanFor10FCV.csv")




########################################### 6
NonMots = read.csv('ImpFeats_inAll4_DataFemale.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #54, 30

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1000){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "ImpFeats_inAll4_DataFemale_1000RanFor10FCV.csv")





########################################### 7
NonMots = read.csv('ImpFeats_CommonIn2_DataFemale.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #54, 30

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1000){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "repeatedcv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "ImpFeats_CommonIn2_DataFemale_1000RanFor10FCV.csv")





