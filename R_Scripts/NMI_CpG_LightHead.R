setwd("/home/system-02/zain")
options(scipen = 999) #Viewing output values as decimal values

###################### 1. APPRDX [Patient, Healthy] Wise ###################### 

Feat_Data = vroom("LightHeadCateg_Methylome_APPRDXFilter.csv")
Feat_Data = Feat_Data[Feat_Data$PATNO != 4709, ]     # conditions
FeatData <- Feat_Data %>% select(-c(1,2,3,4,6))
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

colnames(test_result) <- ("MutInf_LightHead")
test_result$Normalized <- (test_result$MutInf_LightHead)/(0.6025032053)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_APPRDX_LightHead.csv")


###################### 2. Category [Normal,Mild,Severe] Wise ###################### 

FeatData <- Feat_Data %>% select(-c(1,2,3,4,5))
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

colnames(test_result) <- ("MutInf_LightHead")
test_result$Normalized <- (test_result$MutInf_LightHead)/(0.587719881)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_CategWise_LightHead.csv")


###################### 3. Gender [Male, Female] Wise ###################### 

FeatData <- Feat_Data %>% select(-c(1,2,3,5,6))
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

colnames(test_result) <- ("MutInf_LightHead")
test_result$Normalized <- (test_result$MutInf_LightHead)/(0.632401876)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_Gender_LightHead.csv")

###################### 4. HYS Wise ###################### 

FeatData <- Feat_Data %>% select(-c(1,2,4,5,6))
Feat_Data = Feat_Data[Feat_Data$HYS != 4 & Feat_Data$HYS != 5, ]         # conditions
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

colnames(test_result) <- ("MutInf_LightHead")
test_result$Normalized <- (test_result$MutInf_LightHead)/(1.159476211)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_HYS_LightHead.csv")

###################### 5. Within Patient, Category Wise ###################### 

FeatData <- Feat_Data [which(Feat_Data$APPRDX == 1),]
FeatData <- FeatData %>% select(-c(1,2,3,4,5))
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

colnames(test_result) <- ("MutInf_LightHead")
test_result$Normalized <- (test_result$MutInf_LightHead)/(0.603883265)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_WithinPatient_CategWise_LightHead.csv")

###################### 6. Within Healthy, Category Wise ###################### 

FeatData <- Feat_Data [which(Feat_Data$APPRDX == 2),]
FeatData <- FeatData %>% select(-c(1,2,3,4,5))
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

colnames(test_result) <- ("MutInf_LightHead")
test_result$Normalized <- (test_result$MutInf_LightHead)/(0.49408450)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_WithinHealthy_CategWise_LightHead.csv")
