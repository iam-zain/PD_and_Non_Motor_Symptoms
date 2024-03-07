setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\NonMot_Files_RAW_PreProcess\\Feats21_UnCategorized")
NonMots = read.csv('Feats21_unCateg_APPRDX.csv')
NonMot1 <- NonMots  %>% select(-c(1)) #Remove PATNO

#Converting Column4 i.e. APPRDX from integer to character 
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  df <- NonMot1 %>% group_by(APPRDX) %>% sample_n(80)
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
write.csv(output, "NonMotor_UnCategorizedFeats_100RF10FCV.csv", row.names = F)
