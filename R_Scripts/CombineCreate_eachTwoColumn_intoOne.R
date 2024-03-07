setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\NMI\\NMI_2CategAsOne") 
df1 <- read.csv("Any2Common_inAll45Feats_DataCharacter.csv", header = T)
df <- df1 %>% select(-c(1,2))
df<-as_tibble(df)
#df%>% mutate(paste(df$UPSIT,df$MDSP_Fatigue))
for(i in 1:11){
  for(j in (i+1):12){
    df<-cbind(df,paste0(df[,i],df[,j]))
    colnames(df)[ncol(df)]<-paste0(colnames(df)[i],colnames(df)[j])
  }
}
df$APPRDX <- df1$APPRDX
write.csv(df,'NonMot_Common4_Character_2As1.csv', row.names = F)


########## Adding columns whose values are integer (not character)
df <- read.csv("NMI_Common4_Transposed.csv", header = T)

#df%>% mutate(paste(df$UPSIT,df$MDSP_Fatigue))
for(i in 1:11){
  for(j in (i+1):12){
    df<-cbind(df,(df[,i]+df[,j]))
    colnames(df)[ncol(df)]<-paste0(colnames(df)[i],colnames(df)[j])
  }
}

write.csv(df,'NonMot_Common4_Sum2Feats_2As1_Long.csv', row.names = F)
