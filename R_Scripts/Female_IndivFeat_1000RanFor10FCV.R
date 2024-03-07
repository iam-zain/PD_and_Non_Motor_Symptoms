setwd("Z:\\PPMI_Data\\Excels\\Only_Female")

NonMot1 = read.csv('NonMotor_Categorized_FemaleEdited.csv')
dim(NonMot1)
NonMot2 <- NonMot1 %>% select(-"PATNO") #Removing PATNO column
NonMot2$APPRDX <- as.factor(NonMot2$APPRDX)
NonMot2 <- as.data.frame(NonMot2)
output <- data.frame()
for(i in 2:ncol(NonMot2)){
  print(colnames(NonMot2)[i])
  for(j in 1:1){
    print(i)
    df <- NonMot2 #%>% group_by(APPRDX) %>% sample_n(30)
    df<-df[,c(1,i)]
    df <- as.data.frame(df)
    train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
    data_train <- df[train,]
    data_test <- df[-train,]
    control <- trainControl(method = "repeatedcv", number = 10, repeats = 2)
    fit.cv <- train (APPRDX~., data = NonMot2, method = 'rf', trControl = control)
    print(fit.cv)
    pred <- predict(fit.cv, data_test)
    res <- confusionMatrix(table(data_test [, "APPRDX"], pred))
    res$table
    acc <- (res$table[1,1]+res$table[2,2])/sum(res$table)
    output[j,(i-1)] <- acc
  }
}
colnames(output)<-colnames(NonMot2)[2:ncol(NonMot2)]
write.csv(output, "NonMot_Female_1000RanFor_10FCV.csv")
output

#warnings()
