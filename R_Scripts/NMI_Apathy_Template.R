setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\GeneCpG\\TrailMakeA")
options(scipen = 999) #Viewing output values as decimal values

####  Trail Making A #### 

Feat_Data = vroom("TrailMakeA_with_MethylomeDetails.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,4,5,6,8))
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
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.6097598961)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_TrailMakeA.csv")

# NMI
df_NMI <- read.csv('CpGList_Top50NMI_TrailMakeA.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_TrailMakeA_Data.csv', row.names = F)








####  Dream #### 

setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\GeneCpG\\Dream")

Feat_Data = vroom("Dream_with_MethylomeDetails.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,4,5,6,8))
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
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.6259236500)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_Dream.csv")


# NMI
df_NMI <- read.csv('CpGList_Top50NMI_Dream.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_Dream_Data.csv', row.names = F)




setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\Gastro")

####  NMI #### 

Feat_Data = vroom("Gastro_with_MethylomeDetails.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,5,6,7,8))
colnames(FeatData)[1]

{
  cl <- makeCluster(8)
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
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.602503205)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_Gastro.csv")



# Boruta
df_NMI <- read.csv('Gastro_Boruta_Top50.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50Boruta_Gastro_Data.csv', row.names = F)

# NMI
df_NMI <- read.csv('CpGList_Top50NMI_Gastro.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_Gastro_Data.csv', row.names = F)






#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = FeatData, doTrace = 2, maxRuns = 999)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
imp_CpG_Data <- Feat_Data %>% select(PATNO, APPRDX,cg06308537)
write.csv(imp_CpG_Data, "BorutaR_Gastro_CpG.csv", row.names = F)





setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\LetterNumber")

####  NMI #### 

Feat_Data = vroom("LetterNumber_with_MethylomeDetails.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,5,6,7,8))
colnames(FeatData)[1]

{
  cl <- makeCluster(8)
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
write.csv(test_result, "NMI_APPRDX_LetterNumber.csv")




# Boruta
df_NMI <- read.csv('LetterNumber_Boruta_Top50.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50Boruta_LetterNumber_Data.csv', row.names = F)

# NMI
df_NMI <- read.csv('CpGList_Top50NMI_LetterNumber.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_LetterNumber_Data.csv', row.names = F)







#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = FeatData, doTrace = 2, maxRuns = 999)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
imp_CpG_Data <- Feat_Data %>% select(PATNO, APPRDX,)
write.csv(imp_CpG_Data, "BorutaR_LetterNumber_CpG.csv", row.names = F)








####  Lexical #### 
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\LexicalFluency")

Feat_Data = vroom("LexicalFluency_with_MethylomeDetails.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,5,6,7,8))
colnames(FeatData)[1]

{
  cl <- makeCluster(8)
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
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.60755587)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_LexicalFluency.csv")



# Boruta
df_NMI <- read.csv('LexicalFluency_Boruta_Top50.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50Boruta_LexicalFluency_Data.csv', row.names = F)

# NMI
df_NMI <- read.csv('CpGList_Top50NMI_LexicalFluency.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_LexicalFluency_Data.csv', row.names = F)





#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = FeatData, doTrace = 2, maxRuns = 999)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
imp_CpG_Data <- Feat_Data %>% select(PATNO, APPRDX,cg04001014 , cg03691812 , cg18504826 , ch.14.1421228R , 
                                       cg17217677 , cg18223453 , cg02618319 , cg25911279)
write.csv(imp_CpG_Data, "BorutaR_LexicalFluency_CpG.csv", row.names = F)




####  TrailMakeB #### 
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\TrailMakeB")

Feat_Data = vroom("TrailMakeB_with_MethylomeDetails.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,2,3,5,6,7,8))
colnames(FeatData)[1]

{
  cl <- makeCluster(8)
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
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.602503205)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_TrailMakeB.csv")



#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = FeatData, doTrace = 2, maxRuns = 999)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
imp_CpG_Data <- Feat_Data %>% select(PATNO, APPRDX,cg19393302 , cg26030741...8002 , cg10729106 , cg04958915 , 
                                       cg22397783 , ch.14.1421228R , cg11201297 , cg09495953 , cg06173919)
write.csv(imp_CpG_Data, "BorutaR_TrailMakeB_CpG.csv", row.names = F)




# Boruta
df_NMI <- read.csv('TrailMakeB_Boruta_Top50.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50Boruta_TrailMakeB_Data.csv', row.names = F)

# NMI
df_NMI <- read.csv('CpGList_Top50NMI_TrailMakeB.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- Feat_Data$APPRDX
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
write.csv(SelectCpGsRaw, 'Top50NMI_TrailMakeB_Data.csv', row.names = F)
