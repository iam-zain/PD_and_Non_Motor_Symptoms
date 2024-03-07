setwd("/home/system-02/zain")
options(scipen = 999) #Viewing output values as decimal values

###################### 1. APPRDX [Patient, Healthy] Wise ###################### 

Feat_Data = vroom("NonMotor_Categorized_Male.csv")
#Feat_Data = Feat_Data[Feat_Data$PATNO != 4709, ]     # conditions
FeatData <- Feat_Data %>% select(-c(1))
colnames(FeatData)[1]

{
  cl <- makeCluster(7)
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
test_result$Normalized <- (test_result$MutInf_APPRDX)/(0.639261674391)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "ImpFeats_NMI.csv", row.names = T)
