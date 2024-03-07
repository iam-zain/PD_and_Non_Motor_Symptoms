setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male")
df = vroom("Feats45_CategAge_APPRDX_Male_Edit.csv") 
head(df[1:8,1:8])
df1 <- df  %>% dplyr::select(-c("PATNO")) #Remove PATNO


df1$APPRDX = as.factor(df1$APPRDX)
df1 <- as.data.frame(df1)
set.seed(1)
train <- createDataPartition(df1 [,"APPRDX"], p=0.8, list = F)
data_train <- df1[train,]
data_test <- df1[-train,]

#Decision Tree 
DecTree <- train(APPRDX ~ ., data_train, method="rpart", trControl = trainControl(method = "cv"))
Feats_DecTree <- varImp(DecTree)
write.csv ((as.data.frame(Feats_DecTree$importance)), "ImpFeats_DecTree_Male.csv")


RanFor <- train(APPRDX~., data_train, method = "rf", trControl = trainControl(method = "cv"))
Feats_RanFor <- varImp(RanFor)
write.csv ((as.data.frame(Feats_RanFor$importance)), "ImpFeats_RanFor_Male.csv", row.names = T) 




#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = df1, doTrace = 2, maxRuns = 999)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
par(las = 2)  # 0 for parallel to the axis, 1 for horizontal, 2 for perpendicular to the axis
par(mar = c(5, 6, 4, 2) + 6) # Adjust margins
plot(res_bor, xlab = "")

df_Boruta <- df %>% select(PATNO, APPRDX,Hopkins_Recog , LetterNumber , MDSP_Fatigue , MDS_Depress , 
     MDS_Apathy , Modif_Boston , Montreal_Cognitive , REM_Dream , 
Symbol_Digit , Trail_Making_A , Trail_Making_B , UPSIT)
write.csv(df_Boruta, "BorutaR_All45Feats_Data_Male.csv", row.names = F)


### NMI
options(scipen = 999) #Viewing output values as decimal values
colnames(df1)[1]

{
  cl <- makeCluster(3)
  registerDoParallel(cl)
  dat_dsc <- discretize(df1)
  test_result <- 
    foreach(elem = 1:length(dat_dsc), .combine = rbind) %dopar%
    {
      tempdf <- as.data.frame(infotheo::mutinformation(dat_dsc[,1],dat_dsc[,elem], method = "emp"))
      rownames(tempdf) <- colnames(dat_dsc[elem])
      tempdf
    }
  stopCluster(cl)
}

colnames(test_result) <- ("MutInf_APPRDX")
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.6378887403998)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_All45Feats_Categorized_Male.csv")


df_NMI <- df %>% select(APPRDX,UPSIT,Trail_Making_B,MDSP_Fatigue,Montreal_Cognitive,Trail_Making_A
,MDS_Apathy,MDS_Depress,MDSP_SleepDay,SCOPA_Sex,MDSP_LightHead)
write.csv(df_NMI, "NMI_All45Feats_Data_Male.csv", row.names = F)


df_RanFor <- df %>% select(APPRDX,UPSIT,Symbol_Digit,MDSP_Fatigue,Trail_Making_B,REM_Dream,MDSP_Pain
,Geriatric_Depression,SCOPA_Sex,MDSP_SleepDay,MDSP_SleepNight)
write.csv(df_RanFor, "RanFor_All45Feats_Data_Male.csv", row.names = F)


df_DecTree <- df %>% select(APPRDX,UPSIT,Symbol_Digit,Trail_Making_B,MDSP_Fatigue,MDS_Apathy)
write.csv(df_DecTree, "DecTree_All45Feats_Data_Male.csv", row.names = F)

##########

df_allCommon <- df %>% select(APPRDX,Geriatric_Depression,Hopkins_Recog,LetterNumber,MDS_Apathy
,MDS_Depress,MDSP_Fatigue,MDSP_LightHead,MDSP_Pain,MDSP_SleepDay,MDSP_SleepNight,Modif_Boston
,Montreal_Cognitive,REM_Dream,SCOPA_Sex,Symbol_Digit,Trail_Making_A,Trail_Making_B,UPSIT)
write.csv(df_allCommon, "AllCommon_inAll45Feats_Data_Male.csv", row.names = F)


df_any2Common <- df %>% select(PATNO, APPRDX,MDS_Apathy,MDS_Depress,MDSP_Fatigue,MDSP_SleepDay
,Montreal_Cognitive,REM_Dream,SCOPA_Sex,Symbol_Digit,Trail_Making_A,Trail_Making_B,UPSIT)
write.csv(df_any2Common, "Any2Common_inAll45Feats_Data.csv", row.names = F)
