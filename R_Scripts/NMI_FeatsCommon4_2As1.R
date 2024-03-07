setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\NMI\\NMI_2CategAsOne") 
df <- read.csv("NonMot_Common4_Character_2As1.csv", header = T)

options(scipen = 999) #Viewing output values as decimal values
colnames(df)[1]

{
  cl <- makeCluster(3)
  registerDoParallel(cl)
  dat_dsc <- discretize(df)
  test_result <- 
    foreach(elem = 1:length(dat_dsc), .combine = rbind) %dopar%
    {
      tempdf1 <- as.data.frame(infotheo::mutinformation(dat_dsc[,1],dat_dsc[,elem], method = "emp"))
      rownames(tempdf1) <- colnames(dat_dsc[elem])
      tempdf1
    }
  stopCluster(cl)
}

colnames(test_result) <- ("MutInf_TwoAsOne")
test_result$Normalized <- (test_result$MutInf_TwoAsOne)/(0.643653071)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_Common4_TwoAsOne.csv")
