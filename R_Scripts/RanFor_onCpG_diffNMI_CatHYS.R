setwd("Z:\\PPMI_Data\\Excels\\MultiClass\\NMI_CutOffs")

###############  Top 50  #################
NonMots = read.csv('NMI_Chr1_22_CpG_CatHYS_Top50Data.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. Cat_HYS from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$Cat_HYS) #Counting total number of each unique values C=131, P0=69, P1=73, P2=167

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(Cat_HYS) %>% sample_n(66)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"Cat_HYS"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10)
  fit.cv <- train (Cat_HYS~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NMI_Chr1_22_CpG_CatHYS_Top50_10FCV.csv", row.names = F)


###############  Top 100  #################
NonMots = read.csv('NMI_Chr1_22_CpG_CatHYS_Top100Data.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(Cat_HYS) %>% sample_n(66)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"Cat_HYS"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10)
  fit.cv <- train (Cat_HYS~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NMI_Chr1_22_CpG_CatHYS_Top100_10FCV.csv", row.names = F)


###############  Top 500  #################
NonMots = read.csv('NMI_Chr1_22_CpG_CatHYS_Top500Data.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(Cat_HYS) %>% sample_n(66)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"Cat_HYS"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10)
  fit.cv <- train (Cat_HYS~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NMI_Chr1_22_CpG_CatHYS_Top500_10FCV.csv", row.names = F)


###############  Top 1000  #################
NonMots = read.csv('NMI_Chr1_22_CpG_CatHYS_Top1000Data.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(Cat_HYS) %>% sample_n(66)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"Cat_HYS"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10)
  fit.cv <- train (Cat_HYS~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NMI_Chr1_22_CpG_CatHYS_Top1000_10FCV.csv", row.names = F)


###############  Top 2000  #################
NonMots = read.csv('NMI_Chr1_22_CpG_CatHYS_Top2000Data.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(Cat_HYS) %>% sample_n(66)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"Cat_HYS"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10)
  fit.cv <- train (Cat_HYS~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NMI_Chr1_22_CpG_CatHYS_Top2000_10FCV.csv", row.names = F)


###############  NMI > 0.04  #################
NonMots = read.csv('NMI_Chr1_22_CpG_CatHYS_NMI04Data.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(Cat_HYS) %>% sample_n(66)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"Cat_HYS"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10)
  fit.cv <- train (Cat_HYS~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NMI_Chr1_22_CpG_CatHYS_NMI04_10FCV.csv", row.names = F)











###############  45 Feats with HYS  #################

setwd("Z:\\PPMI_Data\\Excels\\MultiClass")

NonMots = read.csv('NonMotor_Categorized_withHYS.csv')
NonMot1 <- NonMots  %>% select(-c(1,2,3)) #Remove PATNO

#Converting Column4 i.e. Cat_HYS from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$Cat_HYS) #Counting total number of each unique values C=86, P0=0, P1=41, P2=115

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(Cat_HYS) %>% sample_n(40)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"Cat_HYS"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10)
  fit.cv <- train (Cat_HYS~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_withHYS_10FCV.csv", row.names = F)



###############  common 12 Feats with HYS  #################

setwd("Z:\\PPMI_Data\\Excels\\MultiClass")

NonMots = read.csv('NonMotor_Categorized_withHYS_common12.csv')
NonMot1 <- NonMots  %>% select(-c(1,2,3)) #Remove PATNO

#Converting Column4 i.e. Cat_HYS from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$Cat_HYS) #Counting total number of each unique values C=86, P0=0, P1=41, P2=115

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(Cat_HYS) %>% sample_n(40)
  df <- as.data.frame(df)
  class(df)
  train <- createDataPartition(df [,"Cat_HYS"], p=0.8, list = F)
  data_train <- df[train,]
  data_test <- df[-train,]
  control <- trainControl(method = "cv", number = 10)
  fit.cv <- train (Cat_HYS~., data = data_train, method = 'rf', trControl = control)
  print(fit.cv)
  acc <- fit.cv$results[1,2]
  output[i,1] <- i
  output[i,2] <- acc
}
stopCluster(cl)
output
write.csv(output, "NonMotor_Categorized_withHYS_common12_10FCV.csv", row.names = F)



