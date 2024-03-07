setwd ("Z:\\PPMI_Data\\Excels\\Raw_And_More") 
options(scipen = 999) #Viewing output values as decimal values
df1 <- vroom("Male_Chromosome.csv")
head(df1[1:11,1:11])
df <- df1 %>% select(-c(1,2,3))

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

colnames(test_result) <- ("MutInf_AllChr")
test_result$Normalized <- (test_result$MutInf_AllChr)/(0.61601732)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_Chr_Male.csv")

## Extracting data of specific CpG lists
CpG <- read.csv("CpG08_Male.csv")
cpgname <- CpG$CpG08
Extract12 <- df[, cpgname]
Extract12$APPRDX <- df$APPRDX
write.csv(Extract12, "NMI08_Chr_Male.csv")
