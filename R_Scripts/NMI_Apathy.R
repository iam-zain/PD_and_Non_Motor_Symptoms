setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\Apathy")
options(scipen = 999) #Viewing output values as decimal values

####  NMI #### 

Feat_Data = vroom("Apathy_with_MethylomeDetails.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,5,6,7,8))
colnames(FeatData)[1]

{
  cl <- makeCluster(4)
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
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.6025032053)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_Apathy.csv")





#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = FeatData, doTrace = 2, maxRuns = 999)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
imp_CpG_Data <- Feat_Data %>% select(PATNO, APPRDX,cg08409074 , cg01081395 , cg02522978 , cg03605226 , 
                                cg02157463 , cg17741809 , cg26200585)
write.csv(imp_CpG_Data, "BorutaR_Apathy_CpG.csv", row.names = F)



# NMI
df_NMI <- read.csv('CpGList_Top50NMI_Apathy.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_Apathy_Data.csv', row.names = F)

# Boruta
df_NMI <- read.csv('Apathy_Boruta_Top50.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50Boruta_Apathy_Data.csv', row.names = F)

# Common
df_NMI <- read.csv('Common_Cpg_Apathy.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Common10_Apathy_Data.csv', row.names = F)
