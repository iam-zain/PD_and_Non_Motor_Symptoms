setwd("Z:\\PPMI_Data\\Excels\\MultiClass")
options(scipen = 999) #Viewing output values as decimal values

####  NMI #### 

Feat_Data = vroom("Chr1_22_CpG_CatHYS_Data.csv")
head(Feat_Data)[1:3,1:9]
FeatData <- Feat_Data %>% select(-c(1,2,4,5,6))
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
test_result$Normalized <- (test_result$MutInf_APPRDX)/(1.316973127)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_Chr1_22_CpG_CatHYS_Data.csv")



############  Taking Top CpGs Data  ##############
setwd("Z:\\PPMI_Data\\Excels\\MultiClass\\NMI_CutOffs")

# NMI 2000
df_NMI <- read.csv('NMI_Chr1_22_CpG_CatHYS_Top2000.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
SelectCpGsRaw$Cat_HYS <- Feat_Data$Cat_HYS
write.csv(SelectCpGsRaw, 'NMI_Chr1_22_CpG_CatHYS_Top2000Data.csv', row.names = F)

# NMI 1000
df_NMI <- read.csv('NMI_Chr1_22_CpG_CatHYS_Top1000.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
SelectCpGsRaw$Cat_HYS <- Feat_Data$Cat_HYS
write.csv(SelectCpGsRaw, 'NMI_Chr1_22_CpG_CatHYS_Top1000Data.csv', row.names = F)

# NMI 500
df_NMI <- read.csv('NMI_Chr1_22_CpG_CatHYS_Top500.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
SelectCpGsRaw$Cat_HYS <- Feat_Data$Cat_HYS
write.csv(SelectCpGsRaw, 'NMI_Chr1_22_CpG_CatHYS_Top500Data.csv', row.names = F)

# NMI > 0.04 (317)
df_NMI <- read.csv('NMI_Chr1_22_CpG_CatHYS_NMI04.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
SelectCpGsRaw$Cat_HYS <- Feat_Data$Cat_HYS
write.csv(SelectCpGsRaw, 'NMI_Chr1_22_CpG_CatHYS_NMI04Data.csv', row.names = F)

# NMI 100
df_NMI <- read.csv('NMI_Chr1_22_CpG_CatHYS_Top100.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
SelectCpGsRaw$Cat_HYS <- Feat_Data$Cat_HYS
write.csv(SelectCpGsRaw, 'NMI_Chr1_22_CpG_CatHYS_Top100Data.csv', row.names = F)

# NMI 50
df_NMI <- read.csv('NMI_Chr1_22_CpG_CatHYS_Top50.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = Feat_Data[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$PATNO <- Feat_Data$PATNO
SelectCpGsRaw$Cat_HYS <- Feat_Data$Cat_HYS
write.csv(SelectCpGsRaw, 'NMI_Chr1_22_CpG_CatHYS_Top50Data.csv', row.names = F)

