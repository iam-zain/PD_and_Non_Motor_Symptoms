setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Main21")
NonMots = read.csv('NonMotor_MainFeatures_Categorized.csv')
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
write.csv(output, "NonMotor_Main22Feats_100RF10FCV.csv", row.names = F)


options(scipen = 999) #Viewing output values as decimal values

####  NMI #### 

Feat_Data = vroom("NonMotor_MainFeatures_Categorized.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1))
colnames(FeatData)[1]

{
  cl <- makeCluster(3)
  registerDoParallel(cl)
  dat_dsc <- discretize(FeatData)
  test_result <- 
    foreach(elem = 1:length(dat_dsc), .combine = rbind) %dopar%
    {
      tempdf1 <- as.data.frame(infotheo::mutinformation(dat_dsc[,1],dat_dsc[,elem], method = "emp"))
      rownames(tempdf1) <- colnames(dat_dsc[elem])
      tempdf1
    }
  stopCluster(cl)
}

colnames(test_result) <- ("MutInf_APPRDX")
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.64365307130)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_Main22Feats.csv")



# NMI Data
df_NMI <- read.csv('NMI_APPRDX_Main22Feats_Top10NMI.csv')
CpgNames <- df_NMI$Feature
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top10_from22Feats_Data.csv', row.names = F)

#########  NMI: Random Forest #########
NonMots = read.csv('Top10_from22Feats_Data.csv')
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
write.csv(output, "Top10NMI_from22Feats_100RF10FCV.csv", row.names = F)





################ Random 10 ###############
NonMot = read.csv('NonMotor_MainFeatures_Categorized.csv')
NonMot1 <- NonMot  %>% select(-c(1)) #Remove PATNO
NonMot1 <- NonMot1 %>% mutate(across(.cols = 1, .fns = factor))
NonMot1 <- as.data.frame(NonMot1)
table(NonMot1$APPRDX) #Counting total number of each unique values #164, 86

output <- data.frame(matrix(ncol=2,nrow=0))
cl <- makeCluster(4)
registerDoParallel(cl)
set.seed(1)
for(i in 1:100){
  print(i)
  NonMot2 <- dplyr::select(NonMot1, sample(seq_len(ncol(NonMot1[,2:23])), size = 10))
  NonMot2$APPRDX <- NonMot1$APPRDX
  df <- NonMot2 %>% group_by(APPRDX) %>% sample_n(80)
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
write.csv(output, "Random10Feat_fromMain22Feats_100RF10FCV.csv", row.names = F)




####### Plotting NMI ###########

setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Main21")
df <- read.csv("NMI_APPRDX_Main22Feats.csv", header = T)
df <- df  %>% select(-c(2))
df <- df %>% filter(!row_number() %in% c(1)) #Removing 1st row i.e. APPRDX
df %>% ggplot(aes(reorder(Features, Normalized), Normalized)) + geom_col (aes(fill= Normalized)) + 
  theme(axis.text = element_text(face="bold")) + ylab ("\nNMI Value") + 
  scale_fill_gradient(low="cornflowerblue",high="deeppink") + coord_flip() + labs (x = 'Features') +
  theme(axis.title = element_text(face="bold")) + theme(legend.title = element_text(face = "bold")) +
  theme(text = element_text(size=16)) + theme(axis.text=element_text(color="black")) +
  theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("NMI Value of 21 Features- Patient Vs Healthy Control")
