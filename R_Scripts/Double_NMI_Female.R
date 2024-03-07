setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\NMI") 
df1 <- read.csv("Any2Common_inAll45Feats_Data_Female_Character.csv", header = T)
df <- df1 %>% select(-c(1,2))
df<-as_tibble(df)
#df%>% mutate(paste(df$UPSIT,df$MDSP_Fatigue))
for(i in 1:4){
  for(j in (i+1):5){
    df<-cbind(df,paste0(df[,i],df[,j]))
    colnames(df)[ncol(df)]<-paste0(colnames(df)[i],colnames(df)[j])
  }
}
df$APPRDX <- df1$APPRDX
write.csv(df,'NonMot_Common4_Character_2As1_Female_Raw.csv', row.names = F)


########## Adding columns whose values are integer (not character)
df <- read.csv("NMI_Common4_Transposed_Female.csv", header = T)

#df%>% mutate(paste(df$UPSIT,df$MDSP_Fatigue))
for(i in 1:4){
  for(j in (i+1):5){
    df<-cbind(df,(df[,i]+df[,j]))
    colnames(df)[ncol(df)]<-paste0(colnames(df)[i],colnames(df)[j])
  }
}

write.csv(df,'NonMot_Common4_Sum2Feats_2As1_Female_Long.csv', row.names = F)




df <- read.csv("NonMot_Common4_Character_2As1_Female.csv", header = T)

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
test_result$Normalized <- (test_result$MutInf_TwoAsOne)/(0.651756561)
test_result <- arrange(test_result, desc(Normalized))
write.csv(test_result, "NMI_Common4_TwoAsOne_Female_Raw.csv")
