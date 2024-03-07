setwd("Z:\\PPMI_Data\\Excels\\Only_Female\\GeneCpG\\UPSIT")
options(scipen = 999) #Viewing output values as decimal values

###################### 1. APPRDX [Patient, Healthy] Wise ###################### 

Feat_Data = vroom("UPSIT3Categ_Methylome_Female.csv")
FeatData <- Feat_Data %>% select(-c(1,2,3,4,6,7))
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

colnames(test_result) <- ("MutInf_UPSIT")
test_result$Normalized <- (test_result$MutInf_UPSIT)/(0.5730569171)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_UPSIT_Female.csv")


###################### 2. Category [Normal,Mild,Severe] Wise ###################### 

FeatData <- Feat_Data %>% select(-c(1,2,3,4,5,7))
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

colnames(test_result) <- ("MutInf_UPSIT")
test_result$Normalized <- (test_result$MutInf_UPSIT)/(1.042171286)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_CategWise_UPSIT_Female.csv")



###################### 3. HYS Wise ###################### 

FeatData <- Feat_Data %>% select(-c(1,2,4,5,6,7))
Feat_Data = Feat_Data[Feat_Data$HYS != 4 & Feat_Data$HYS != 5, ]         # conditions
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

colnames(test_result) <- ("MutInf_UPSIT")
test_result$Normalized <- (test_result$MutInf_UPSIT)/(1.138890376)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_HYS_UPSIT_Female.csv")

###################### 4. Within Patient, Category Wise ###################### 

FeatData <- Feat_Data [which(Feat_Data$APPRDX == 1),]
FeatData <- FeatData %>% select(-c(1,2,3,4,5,7))
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

colnames(test_result) <- ("MutInf_UPSIT")
test_result$Normalized <- (test_result$MutInf_UPSIT)/(1.053841550)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_WithinPatient_CategWise_UPSIT_Female.csv")

###################### 5. Within Healthy, Category Wise ###################### 

FeatData <- Feat_Data [which(Feat_Data$APPRDX == 2),]
FeatData <- FeatData %>% select(-c(1,2,3,4,5,7))
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

colnames(test_result) <- ("MutInf_UPSIT")
test_result$Normalized <- (test_result$MutInf_UPSIT)/(0.78242438)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_WithinHealthy_CategWise_UPSIT_Female.csv")
