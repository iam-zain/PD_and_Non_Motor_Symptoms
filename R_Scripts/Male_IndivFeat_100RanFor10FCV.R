setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")

###############   Male  ##################

NonMot1 = read.csv('NonMotor_Categorized_Male.csv')
dim(NonMot1)
NonMot2 <- NonMot1 %>% select(-"PATNO") #Removing PATNO column
NonMot2$APPRDX <- as.factor(NonMot2$APPRDX)
table(NonMot2$APPRDX)
NonMot2 <- as.data.frame(NonMot2)
output <- data.frame()

cl <- makeCluster(8)
registerDoParallel(cl)
for(i in 2:ncol(NonMot2)){
  print(colnames(NonMot2)[i])
  for(j in 1:100){
    print(i)
    print(j)
    df <- NonMot2 %>% group_by(APPRDX) %>% sample_n(55)
    df<-df[,c(1,i)]
    df <- as.data.frame(df)
    train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
    data_train <- df[train,]
    data_test <- df[-train,]
    control <- trainControl(method = "cv", number = 10, repeats = 10)
    fit.cv <- train (APPRDX~., data = df, method = 'svmLinear', trControl = control)
    print(fit.cv)
    pred <- predict(fit.cv, data_test)
    res <- confusionMatrix(table(data_test [, "APPRDX"], pred))
    res$table
    acc <- (res$table[1,1]+res$table[2,2])/sum(res$table)
    output[j,(i-1)] <- acc
  }
}
stopCluster(cl)

colnames(output)<-colnames(NonMot2)[2:ncol(NonMot2)]
write.csv(output, "NonMot_Indi45Feats_Male_100RF10FCV.csv")




###############   Female  ##################

NonMot1 = read.csv('NonMotor_Categorized_Female.csv')
dim(NonMot1)
NonMot2 <- NonMot1 %>% select(-"PATNO") #Removing PATNO column
NonMot2$APPRDX <- as.factor(NonMot2$APPRDX)
table(NonMot2$APPRDX)
NonMot2 <- as.data.frame(NonMot2)
output <- data.frame()

cl <- makeCluster(8)
registerDoParallel(cl)
for(i in 2:ncol(NonMot2)){
  print(colnames(NonMot2)[i])
  for(j in 1:100){
    print(i)
    print(j)
    df <- NonMot2 %>% group_by(APPRDX) %>% sample_n(30)
    df<-df[,c(1,i)]
    df <- as.data.frame(df)
    train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
    data_train <- df[train,]
    data_test <- df[-train,]
    control <- trainControl(method = "cv", number = 10, repeats = 10)
    fit.cv <- train (APPRDX~., data = df, method = 'svmLinear', trControl = control)
    print(fit.cv)
    pred <- predict(fit.cv, data_test)
    res <- confusionMatrix(table(data_test [, "APPRDX"], pred))
    res$table
    acc <- (res$table[1,1]+res$table[2,2])/sum(res$table)
    output[j,(i-1)] <- acc
  }
}
stopCluster(cl)
colnames(output)<-colnames(NonMot2)[2:ncol(NonMot2)]
write.csv(output, "NonMot_Indi45Feats_Female_100RF10FCV.csv")



###############   Combined  ##################

NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot2 <- NonMot1 %>% select(-"PATNO") #Removing PATNO column
NonMot2$APPRDX <- as.factor(NonMot2$APPRDX)
table(NonMot2$APPRDX)
NonMot2 <- as.data.frame(NonMot2)
output <- data.frame()

cl <- makeCluster(3)
registerDoParallel(cl)
for(i in 2:ncol(NonMot2)){
  print(colnames(NonMot2)[i])
  for(j in 1:100){
    print(i)
    print(j)
    df <- NonMot2 %>% group_by(APPRDX) %>% sample_n(80)
    df<-df[,c(1,i)]
    df <- as.data.frame(df)
    train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
    data_train <- df[train,]
    data_test <- df[-train,]
    control <- trainControl(method = "cv", number = 10, repeats = 10)
    fit.cv <- train (APPRDX~., data = df, method = 'svmLinear', trControl = control)
    print(fit.cv)
    pred <- predict(fit.cv, data_test)
    res <- confusionMatrix(table(data_test [, "APPRDX"], pred))
    res$table
    acc <- (res$table[1,1]+res$table[2,2])/sum(res$table)
    output[j,(i-1)] <- acc
  }
}
stopCluster(cl)
colnames(output)<-colnames(NonMot2)[2:ncol(NonMot2)]
write.csv(output, "NonMot_Indi45Feats_100RF10FCV.csv")
