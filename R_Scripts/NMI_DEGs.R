
 # 1. Female  ------------------------------------------------------------------
setwd("S:\\PPMI_RNA\\Processed\\Female")
options(scipen = 999) #Viewing output values as decimal values

Feat_Data = vroom("All_Counts_PatAndHealthy_Disease_Female.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,3))
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

colnames(test_result) <- ("MutInf")
test_result$Normalized <- (test_result$MutInf)/(0.658480280034)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_AllCounts_Female.csv")


 # 2. Male  --------------------------------------------------------------------

setwd("S:\\PPMI_RNA\\Processed\\Male")

Feat_Data = vroom("All_Counts_PatAndHealthy_Disease_Male.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1,3))
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

colnames(test_result) <- ("MutInf")
test_result$Normalized <- (test_result$MutInf)/(0.63249287118)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_AllCounts_Male.csv")


 # 3. Combined  ----------------------------------------------------------------

setwd("S:\\PPMI_RNA\\Processed")

Feat_Data = vroom("All_Counts_PatAndHealthy_Disease.csv")
head(Feat_Data)[1:8,1:8]
FeatData <- Feat_Data %>% select(-c(1))
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

colnames(test_result) <- ("MutInf")
test_result$Normalized <- (test_result$MutInf)/(0.6427802044)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_AllCounts.csv")

