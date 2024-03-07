setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\NMI") 
df1 <- read.csv("Any2Common_inAll45Feats_Data_Male_Character.csv", header = T)
df <- df1 %>% select(-c(1,2))
df<-as_tibble(df)
#df%>% mutate(paste(df$UPSIT,df$MDSP_Fatigue))
for(i in 1:10){
  for(j in (i+1):11){
    df<-cbind(df,paste0(df[,i],df[,j]))
    colnames(df)[ncol(df)]<-paste0(colnames(df)[i],colnames(df)[j])
  }
}
df$APPRDX <- df1$APPRDX
write.csv(df,'NonMot_Common4_Character_2As1_Male.csv', row.names = F)


########## Adding columns whose values are integer (not character)
df <- read.csv("NMI_Common4_Transposed_Male.csv", header = T)

#df%>% mutate(paste(df$UPSIT,df$MDSP_Fatigue))
for(i in 1:7){
  for(j in (i+1):8){
    df<-cbind(df,(df[,i]+df[,j]))
    colnames(df)[ncol(df)]<-paste0(colnames(df)[i],colnames(df)[j])
  }
}

write.csv(df,'NonMot_Common4_Sum2Feats_2As1_Male_Long.csv', row.names = F)



## Sum ##
# Get the number of cells in the Feats data
# create a vector of the Feats and Normalized values
feats <- c("UPSIT", "Trail_Making_B", "MDSP_Fatigue", "Montreal_Cognitive", "Trail_Making_A",
           "MDS_Apathy", "MDS_Depress", "MDSP_SleepDay", "SCOPA_Sex", "REM_Dream", "Symbol_Digit")
normalized <- c(0.29572311, 0.132244619, 0.121252697, 0.092082946, 0.07089861,
                0.052948774, 0.045531464, 0.03988453, 0.034657666, 0.019952413,
                0.011266044)

# generate all possible pairs of feats
feats_pairs <- combn(feats, 2)

# compute the sum of each pair and store in a dataframe
results_df <- data.frame(Feat1 = character(),
                         Feat2 = character(),
                         Sum = numeric(),
                         stringsAsFactors = FALSE)

for (i in 1:ncol(feats_pairs)) {
  feat1 <- feats_pairs[1,i]
  feat2 <- feats_pairs[2,i]
  sum_feats <- normalized[feats == feat1] + normalized[feats == feat2]
  results_df <- rbind(results_df, data.frame(Feat1 = feat1, Feat2 = feat2, Sum = sum_feats))
}

# print the results
print(results_df)
write.csv(results_df, 'NMI_Common4_Transposed_Male.csv')






df <- read.csv("NonMot_Common4_Character_2As1_Male.csv", header = T)

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
test_result$Normalized <- (test_result$MutInf_TwoAsOne)/(0.63788874)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_Common4_TwoAsOne_Male.csv")
