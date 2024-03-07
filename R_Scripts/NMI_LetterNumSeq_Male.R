setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\TrailMakeA")
options(scipen = 999) #Viewing output values as decimal values

###################### 1. APPRDX [Patient, Healthy] Wise ###################### 

Feat_Data = vroom("TrailMakeA_with_MethylomeDetails_Male.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,4,5,6,7,9))
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

colnames(test_result) <- ("MutInf")
test_result$Normalized <- (test_result$MutInf)/(0.6087039594)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_TrailMakeA_Male.csv")


# NMI
df_NMI <- read.csv('CpGList_Top50NMI_TrailMakeA_Male.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_TrailMakeA_Data_Male.csv', row.names = F)




setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\Dream")
###################### 1. APPRDX [Patient, Healthy] Wise ###################### 

Feat_Data = vroom("Dream_with_MethylomeDetails_Male.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,4,5,6,7,9))
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

colnames(test_result) <- ("MutInf")
test_result$Normalized <- (test_result$MutInf)/(0.6029636624)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_Dream_Male.csv")


# NMI
df_NMI <- read.csv('CpGList_Top50NMI_Dream_Male.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_Dream_Data_Male.csv', row.names = F)




setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\Apathy")
Feat_Data = vroom("ApathyCateg_Methylome_Male.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,5,6,7,8))
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

colnames(test_result) <- ("MutInf")
test_result$Normalized <- (test_result$MutInf)/(0.6152018065)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_Apathy_Male.csv")


# NMI
df_NMI <- read.csv('CpGList_Top50NMI_Apathy_Male.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_Apathy_Data_Male.csv', row.names = F)





setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\ScopaSex")
Feat_Data = vroom("SexMaleCateg_Methylome_APPRDX.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,4,5))
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

colnames(test_result) <- ("MutInf")
test_result$Normalized <- (test_result$MutInf)/(0.6152018065)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_ScopaSex_Male.csv")


# NMI
df_NMI <- read.csv('CpGList_Top50NMI_ScopaSex_Male.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_ScopaSex_Data_Male.csv', row.names = F)






###################### Hopkins ###################### 

setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Male\\GeneCpG\\Hopkins")

Feat_Data = vroom("Hopkins_with_MethylomeDetails_Male.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,5,6,7,8))
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

colnames(test_result) <- ("MutInf")
test_result$Normalized <- (test_result$MutInf)/(0.6029636624)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_Hopkins_Male.csv")




#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = FeatData, doTrace = 2, maxRuns = 999)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
imp_CpG_Data <- Feat_Data %>% select(PATNO, APPRDX,  cg14123992, cg21228068, cg23867254, cg11489262, 
                                       cg06947206, cg00314622)
write.csv(imp_CpG_Data, "BorutaR_Hopkins_Male.csv", row.names = F)




# Boruta
df_NMI <- read.csv('Hopkins_BorutaMale_Top50.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50Boruta_Hopkins_Data.csv', row.names = F)

# NMI
df_NMI <- read.csv('CpGList_Top50NMI_Hopkins_Male.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_Hopkins_Data.csv', row.names = F)
