setwd ("Z:\\PPMI_Data\\Excel_Data\\NonMotors\\NonMot_Files")
NonMots = read.csv('BorutaFeatsUnrejected_OnAll45Feats.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(3)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1000){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(80)
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
write.csv(output, "BorutaFeatsUnrejected_OnAll45Feats_1000RanFor10FCV.csv")



########################### Feats common in at least 3 out of 5 different algorithms

NonMots = read.csv('FeatsAtLeast3TimesCommon_OnAll45Feats.csv')
# NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMots %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1000){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(80)
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
write.csv(output, "FeatsAtLeast3TimesCommon_OnAll45Feats_1000RanFor10FCV.csv")



########################### Feats common in at least 2 out of 5 different algorithms

NonMots = read.csv('FeatsAtLeast2TimesCommon_OnAll45Feats.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1000){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(80)
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
write.csv(output, "FeatsAtLeast2TimesCommon_OnAll45Feats_1000RanFor10FCV.csv")


########################### Feats common in at least 4 out of 5 different algorithms

NonMots = read.csv('FeatsAtLeast4TimesCommon_OnAll45Feats.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:1000){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(80)
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
write.csv(output, "FeatsAtLeast4TimesCommon_OnAll45Feats_1000RanFor10FCV.csv")


