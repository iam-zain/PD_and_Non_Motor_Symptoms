############################################################
# Feature Selection Script for Parkinson’s Disease Study
#
# Aim:
#   - Perform feature selection using multiple approaches 
#     (NMI, Decision Tree, Random Forest, Boruta)
#   - Identify the most informative features for classifying
#     PD patients vs. healthy controls
#
# Input:
#   - Feats45_Categ.csv (categorical non-motor symptom dataset, n=45 features)
#
# Expected Output:
#   - NMI_All45Feats_Categorized.csv
#   - ImpFeats_DecTree.csv
#   - ImpFeats_RanFor.csv
#   - BorutaR_All45Feats_Data.csv
#   - NMI_All45Feats_Data.csv
#   - RanFor_All45Feats_Data.csv
#   - DecTree_All45Feats_Data.csv
#   - AllCommon_inAll45Feats_Data.csv
#   - Any2Common_inAll45Feats_Data.csv
#
# This script saves the results as CSV files for downstream 
# analysis and reproducibility.
############################################################









setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Files")
df = vroom("Feats45_Categ.csv") 
head(df[1:8,1:8])

### NMI
options(scipen = 999) #Viewing output values as decimal values
colnames(df)[1]

{
  cl <- makeCluster(3)
  registerDoParallel(cl)
  dat_dsc <- discretize(df)
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
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.64276421136)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_All45Feats_Categorized.csv")



df$APPRDX = as.factor(df$APPRDX)
df <- as.data.frame(df)
set.seed(1)
train <- createDataPartition(df [,"APPRDX"], p=0.8, list = F)
data_train <- df[train,]
data_test <- df[-train,]

#Decision Tree 
DecTree <- train(APPRDX ~ ., data_train, method="rpart", trControl = trainControl(method = "cv"))
Feats_DecTree <- varImp(DecTree)
write.csv ((as.data.frame(Feats_DecTree$importance)), "ImpFeats_DecTree.csv")


RanFor <- train(APPRDX~., data_train, method = "rf", trControl = trainControl(method = "cv"))
Feats_RanFor <- varImp(RanFor)
write.csv ((as.data.frame(Feats_RanFor$importance)), "ImpFeats_RanFor.csv", row.names = T) 




#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = df, doTrace = 2, maxRuns = 999)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
# sel_bor <- TentativeRoughFix(res_bor)
# CpG_attributes <- attStats(res_bor)
# imp_Conf_CpG <- getConfirmedFormula(res_bor) #Shows only confirmed important
par(las = 2)  # 0 for parallel to the axis, 1 for horizontal, 2 for perpendicular to the axis
par(mar = c(5, 6, 4, 2) + 6) # Adjust margins
plot(res_bor, xlab = "")

df_Boruta <- df %>% select(APPRDX,MDSP_Fatigue , MDS_Apathy , Montreal_Cognitive , REM_Dream , SCOPA_Gastro , 
Symbol_Digit , Trail_Making_A , Trail_Making_B ,UPSIT)
write.csv(df_Boruta, "BorutaR_All45Feats_Data.csv", row.names = F)


df_NMI <- df %>% select(APPRDX,UPSIT,Montreal_Cognitive,Trail_Making_B,MDSP_Fatigue,Symbol_Digit,MDSP_Constipate
,SCOPA_Gastro,MDS_Apathy,MDSP_LightHead,Trail_Making_A)
write.csv(df_NMI, "NMI_All45Feats_Data.csv", row.names = F)


df_RanFor <- df %>% select(APPRDX,UPSIT,MDSP_Fatigue,Symbol_Digit,Trail_Making_B,Montreal_Cognitive
,Lexical_Fluency,LetterNumber,REM_Dream,SCOPA_Sex,Modif_Boston)
write.csv(df_RanFor, "RanFor_All45Feats_Data.csv", row.names = F)


df_DecTree <- df %>% select(APPRDX,UPSIT,Trail_Making_B,MDSP_Fatigue,Montreal_Cognitive,Symbol_Digit)
write.csv(df_DecTree, "DecTree_All45Feats_Data.csv", row.names = F)


df_allCommon <- df %>% select(APPRDX,LetterNumber,Lexical_Fluency,MDS_Apathy,MDSP_Constipate,MDSP_Fatigue
,MDSP_LightHead,Modif_Boston,Montreal_Cognitive,REM_Dream,SCOPA_Gastro,SCOPA_Sex,Symbol_Digit
,Trail_Making_A,Trail_Making_B,UPSIT)
write.csv(df_allCommon, "AllCommon_inAll45Feats_Data.csv", row.names = F)


df_any2Common <- df %>% select(APPRDX,MDS_Apathy,MDSP_Fatigue,Montreal_Cognitive,REM_Dream
,SCOPA_Gastro,Symbol_Digit,Trail_Making_A,Trail_Making_B,UPSIT)
write.csv(df_any2Common, "Any2Common_inAll45Feats_Data.csv", row.names = F)
