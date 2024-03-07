setwd("/home/system-02/zain")
NonMots = read.csv('NonMotor_Categorized_Male.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(50)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_45Feats_100RF10FCV.csv", row.names = F)


#########  Decision Tree  #########
NonMots = read.csv('DecTree_All45Feats_Data_Male.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(50)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inDecTree_from45Feats__Male_100RF10FCV.csv", row.names = F)


#########  Random Forest  #########
NonMots = read.csv('RanFor_All45Feats_Data_Male.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(50)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inRanFor_from45Feats_Male_100RF10FCV.csv", row.names = F)


#########  Boruta  #########
NonMots = read.csv('BorutaR_All45Feats_Data_Male.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(50)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inBoruta_from45Feats_Male_100RF10FCV.csv", row.names = F)



#########  NMI  #########
NonMots = read.csv('NMI_All45Feats_Data_Male.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(50)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inNMI_from45Feats_Male_100RF10FCV.csv", row.names = F)


#########  in all 4 Methods  #########
NonMots = read.csv('AllCommon_inAll45Feats_Data_Male.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(50)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inAllCommon_from45Feats_Male_100RF10FCV.csv", row.names = F)



#########  Common in any 2 among 4 Methods  #########
NonMots = read.csv('Any2Common_inAll45Feats_Data_Male.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(50)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inAny2Common_from45Feats_Male_100RF10FCV.csv", row.names = F)


################ Random 8 ###############
NonMot = read.csv('NonMotor_Categorized_Male.csv')
NonMot1 <- NonMot  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  NonMot2 <- dplyr::select(NonMot1, sample(seq_len(ncol(NonMot1[,2:46])), size = 8))
  NonMot2$APPRDX <- NonMot1$APPRDX
  df <- NonMot2 %>% group_by(APPRDX) %>% sample_n(50)
  df <- as.data.frame(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "Random8Feat_from45Feats_Male_100RF10FCV.csv", row.names = F)



















############################     Female    ###########################

NonMots = read.csv('NonMotor_Categorized_Female.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_45Feats_100RF10FCV.csv", row.names = F)


#########  Decision Tree  #########
NonMots = read.csv('DecTree_All45Feats_Data_Female.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inDecTree_from45Feats__Female_100RF10FCV.csv", row.names = F)


#########  Random Forest  #########
NonMots = read.csv('RanFor_All45Feats_Data_Female.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inRanFor_from45Feats_Female_100RF10FCV.csv", row.names = F)


#########  Boruta  #########
NonMots = read.csv('BorutaR_All45Feats_Data_Female.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inBoruta_from45Feats_Female_100RF10FCV.csv", row.names = F)



#########  NMI  #########
NonMots = read.csv('NMI_All45Feats_Data_Female.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inNMI_from45Feats_Female_100RF10FCV.csv", row.names = F)


#########  in all 4 Methods  #########
NonMots = read.csv('AllCommon_inAll45Feats_Data_Female.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inAllCommon_from45Feats_Female_100RF10FCV.csv", row.names = F)



#########  Common in any 2 among 4 Methods  #########
NonMots = read.csv('Any2Common_inAll45Feats_Data_Female.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_inAny2Common_from45Feats_Female_100RF10FCV.csv", row.names = F)


################ Random 8 ###############
NonMot = read.csv('NonMotor_Categorized_Female.csv')
NonMot1 <- NonMot  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(8)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  NonMot2 <- dplyr::select(NonMot1, sample(seq_len(ncol(NonMot1[,2:46])), size = 8))
  NonMot2$APPRDX <- NonMot1$APPRDX
  df <- NonMot2 %>% group_by(APPRDX) %>% sample_n(30)
  df <- as.data.frame(df)
  train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10, repeats = 10)
  fit.cv <- train (APPRDX~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "Random8Feat_from45Feats_Female_100RF10FCV.csv", row.names = F)





