setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female")
df = vroom("Feats45_CategAge_APPRDX_Female_Edit.csv") 
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
write.csv ((as.data.frame(Feats_DecTree$importance)), "ImpFeats_DecTree_Female.csv")


RanFor <- train(APPRDX~., data_train, method = "rf", trControl = trainControl(method = "cv"))
Feats_RanFor <- varImp(RanFor)
write.csv ((as.data.frame(Feats_RanFor$importance)), "ImpFeats_RanFor_Female.csv", row.names = T) 




#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = df1, doTrace = 2, maxRuns = 999)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
par(las = 2)  # 0 for parallel to the axis, 1 for horizontal, 2 for perpendicular to the axis
par(mar = c(5, 6, 4, 2) + 6) # Adjust margins
plot(res_bor, xlab = "")


df_Boruta <- df %>% select(PATNO, APPRDX,MDSP_Constipate, Montreal_Cognitive, SCOPA_Gastro, UPSIT)
write.csv(df_Boruta, "BorutaR_All45Feats_Data_Female.csv", row.names = F)


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
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.65175656117)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_All45Feats_Categorized_Female.csv")

df_NMI <- df %>% select(PATNO, APPRDX,UPSIT,Montreal_Cognitive,SCOPA_Gastro,MDSP_Constipate
,MDSP_Pain,SCOPA_Urine,STAIS,MDSP_Urine,MDSP_LightHead,Lexical_Fluency)
write.csv(df_NMI, "NMI_All45Feats_Data_Female.csv", row.names = F)


df_RanFor <- df %>% select(PATNO, APPRDX,UPSIT,Montreal_Cognitive,MDSP_Pain,Semantic,Benton
,SCOPA_Thermo,SCOPA_Sex,LetterNumber,MDSP_Constipate,REM_Dream)
write.csv(df_RanFor, "RanFor_All45Feats_Data_Female.csv", row.names = F)


df_DecTree <- df %>% select(PATNO, APPRDX,UPSIT,MDSP_Constipate,Trail_Making_B
,MDSP_Pain,Montreal_Cognitive)
write.csv(df_DecTree, "DecTree_All45Feats_Data_Female.csv", row.names = F)

##########

df_allCommon <- df %>% select(APPRDX,Benton,LetterNumber,Lexical_Fluency,MDSP_Constipate
,MDSP_LightHead,MDSP_Pain,MDSP_Urine,Montreal_Cognitive,REM_Dream,SCOPA_Gastro,SCOPA_Sex
,SCOPA_Thermo,SCOPA_Urine,Semantic,STAIS,Trail_Making_B,UPSIT)
write.csv(df_allCommon, "AllCommon_inAll45Feats_Data_Female.csv", row.names = F)


df_any2Common <- df %>% select(PATNO, APPRDX,MDSP_Constipate,MDSP_Pain,Montreal_Cognitive,SCOPA_Gastro,UPSIT)
write.csv(df_any2Common, "Any2Common_inAll45Feats_Data_Female.csv", row.names = F)
