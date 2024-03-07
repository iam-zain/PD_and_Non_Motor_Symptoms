setwd("Z:\\PPMI_Data\\Excels\\Only_Female")
options(scipen = 999) #Viewing output values as decimal values

Non_Mot_All = read.csv("NonMotor_Categorized_Female.csv",header = T)
Non_Mot = Non_Mot_All [,-c(1)] # Removing PATNO
Non_Mot$APPRDX = as.factor(Non_Mot$APPRDX)

set.seed(1)
train <- createDataPartition(Non_Mot [,"APPRDX"], p=0.8, list = F)
data_train <- Non_Mot[train,]
data_test <- Non_Mot[-train,]

#Decision Tree 
DecTree <- train(APPRDX ~ ., data_train, method="rpart", trControl = trainControl(method = "cv"))
Feats_DecTree <- varImp(DecTree)
write.csv ((as.data.frame(Feats_DecTree$importance)), "ImpFeats_DecTree.csv")

#RandomForest
RanFor <- train(APPRDX~., data_train, method = "rf", trControl = trainControl(method = "cv"))
Feats_RanFor <- varImp(RanFor)
write.csv ((as.data.frame(Feats_RanFor$importance)), "ImpFeats_RanFor.csv", row.names = T) 


ImpFeats_DecTree_DataFemale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category","MDSP_Constipate_Category",
"MDSP_Pain_Category","Montreal_Cognitive_Category","SCOPA_Gastro_Category")]


ImpFeats_RanFor_DataFemale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category","Montreal_Cognitive_Category",
"MDSP_Pain_Category","Semantic_Category","Benton_Category","SCOPA_Thermo_Category","Hopkins_Recog_Category",
"SCOPA_Sex_Category","Hopkins_Category","LetterNumber_Category")]


ImpFeats_NMI_DataFemale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category","Montreal_Cognitive_Category",
"SCOPA_Gastro_Category","MDSP_Constipate_Category","MDSP_Pain_Category","SCOPA_Urine_Category","STAIS_Category",
"MDSP_Urine_Category","MDSP_LightHead_Category","Lexical_Fluency_Category")]


ImpFeats_Boruta_DataFemale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category","MDSP_Constipate_Category",
"Montreal_Cognitive_Category","Hopkins_Recog_Category","SCOPA_Gastro_Category")] #Boruta perforemd in other script

write.csv(ImpFeats_DecTree_DataFemale, "ImpFeats_DecTree_DataFemale.csv", row.names = F)
write.csv(ImpFeats_RanFor_DataFemale, "ImpFeats_RanFor_DataFemale.csv", row.names = F)
write.csv(ImpFeats_NMI_DataFemale, "ImpFeats_NMI_DataFemale.csv", row.names = F)
write.csv(ImpFeats_Boruta_DataFemale, "ImpFeats_Boruta_DataFemale.csv", row.names = F)

ImpFeats_inAll4_DataFemale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category","Montreal_Cognitive_Category",
"MDSP_Constipate_Category","MDSP_Pain_Category","SCOPA_Gastro_Category","Hopkins_Recog_Category","Benton_Category",
"Hopkins_Category","LetterNumber_Category","Lexical_Fluency_Category","MDSP_LightHead_Category","MDSP_Urine_Category",
"SCOPA_Sex_Category","SCOPA_Thermo_Category","SCOPA_Urine_Category","Semantic_Category","STAIS_Category")]
write.csv(ImpFeats_inAll4_DataFemale, "ImpFeats_inAll4_DataFemale.csv", row.names = F)

ImpFeats_CommonIn2_DataFemale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category","Montreal_Cognitive_Category",
"MDSP_Constipate_Category","MDSP_Pain_Category","SCOPA_Gastro_Category","Hopkins_Recog_Category")]
write.csv(ImpFeats_CommonIn2_DataFemale, "ImpFeats_CommonIn2_DataFemale.csv", row.names = F)


