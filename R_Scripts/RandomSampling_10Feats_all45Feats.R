setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\NonMot_Files")
NonMot = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1 <- NonMot  %>% select(-c(1,2)) #Remove PATNO

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
  NonMot2 <- dplyr::select(NonMot1, sample(seq_len(ncol(NonMot1[,2:46])), size = 10))
  NonMot2$APPRDX <- NonMot1$APPRDX
  df <- NonMot2 %>% group_by(APPRDX) %>% sample_n(80)
  df <- as.data.frame(df)
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
write.csv(output, "Random10Feat_OnAll45Feats_1000RanFor10FCV.csv")
