setwd("/home/system-02/zain")
options(scipen = 999) #Viewing output values as decimal values

Non_Mot_All = read.csv("NonMotor_Categorized_Male.csv",header = T)
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


ImpFeats_DecTree_DataMale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category","Symbol_Digit_Category",
"MDSP_Fatigue_Category","Trail_Making_B_Category","Montreal_Cognitive_Category")]


ImpFeats_RanFor_DataMale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category","Symbol_Digit_Category",
"LetterNumber_Category","MDS_Depress_Category","MDSP_Pain_Category","MDSP_Fatigue_Category",
"MDSP_SleepDay_Category","Geriatric_Depression_Category","Trail_Making_B_Category","Modif_Boston_Category")]


ImpFeats_NMI_DataMale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category","MDSP_Fatigue_Category",
"Montreal_Cognitive_Category","Trail_Making_B_Category","MDS_Apathy_Category","MDS_Depress_Category",
"MDSP_SleepDay_Category","MDSP_LightHead_Category","SCOPA_Sex_Category","SCOPA_Gastro_Category")]


ImpFeats_Boruta_DataMale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category", "LetterNumber_Category",
"MDS_Depress_Category","MDS_Apathy_Category","MDSP_Fatigue_Category","Montreal_Cognitive_Category","SCOPA_Sex_Category",
"Symbol_Digit_Category","Trail_Making_B_Category")] #Boruta perforemd in other script

write.csv(ImpFeats_DecTree_DataMale, "ImpFeats_DecTree_DataMale.csv", row.names = F)
write.csv(ImpFeats_RanFor_DataMale, "ImpFeats_RanFor_DataMale.csv", row.names = F)
write.csv(ImpFeats_NMI_DataMale, "ImpFeats_NMI_DataMale.csv", row.names = F)
write.csv(ImpFeats_Boruta_DataMale, "ImpFeats_Boruta_DataMale.csv", row.names = F)

ImpFeats_inAll4_DataMale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category","MDSP_Fatigue_Category",
"Montreal_Cognitive_Category","Trail_Making_B_Category","LetterNumber_Category","MDS_Apathy_Category",
"MDS_Depress_Category","SCOPA_Sex_Category","Symbol_Digit_Category","Benton_Category","Clock_Category",
"COGSTATE","Epworth_Category","Geriatric_Depression_Category","Hopkins_Category","Hopkins_Recog_Category",
"Lexical_Fluency_Category","MDS_Cognition_Category","MDSP_LightHead_Category",
"MDSP_SleepDay_Category","SCOPA_Gastro_Category")]
write.csv(ImpFeats_inAll4_DataMale, "ImpFeats_inAll4_DataMale.csv", row.names = F)

ImpFeats_CommonIn2_DataMale = Non_Mot_All [,c('PATNO','APPRDX', "UPSIT_3_Category","MDSP_Fatigue_Category",
"Montreal_Cognitive_Category","Trail_Making_B_Category","LetterNumber_Category","MDS_Apathy_Category",
"MDS_Depress_Category","SCOPA_Sex_Category","Symbol_Digit_Category")]
write.csv(ImpFeats_CommonIn2_DataMale, "ImpFeats_CommonIn2_DataMale.csv", row.names = F)


