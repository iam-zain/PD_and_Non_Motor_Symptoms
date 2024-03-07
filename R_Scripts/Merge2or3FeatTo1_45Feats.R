setwd ("Z:\\PPMI_Data\\Excels\\CollaborativeFiltering")

########## Merge two Columns Values Into One New Column ########
df1 <- read.csv("NonMotor_SocioBehavior_CategorizedAll.csv", header = T)
df <- df1 %>% select(-c(1,2))

#df%>% mutate(paste(df$UPSIT,df$MDSP_Fatigue))
for(i in 1:44){
  for(j in (i+1):45){
    df<-cbind(df,(df[,i]+df[,j]))
    colnames(df)[ncol(df)]<-paste0(colnames(df)[i],' ',colnames(df)[j])
  }
}
df$Patient_ID <- df1$Patient_ID
write.csv(df,'NonMotor_2MergeTo1.csv', row.names = F)


########## Merge Three Columns Values Into One New Column ########

df <- df1 %>% select(-c(1,2))
for(i in 1:43){
  for(j in (i+1):44){
    for(k in (i+2):45){
    df<-cbind(df,(df[,i]+df[,j] +df[,k]))
    colnames(df)[ncol(df)]<-paste0(colnames(df)[i],' ',colnames(df)[j],' ',colnames(df)[k])
    }
  }
}
df$Patient_ID <- df1$Patient_ID
df <- df[,46:27480] # Removing feature (single feature), only keeping 3 feature merged as one
df <- df %>% relocate(Patient_ID, .before = `Benton COGSTATE Clock`) 
write.csv(df,'NonMotor_3MergeTo1.csv', row.names = F)





##########################  Melt  ########################

####### 2 Features
df = read.csv("NonMotor_2MergeTo1.csv",header = T)
df_long <- melt (df, id.vars = c("Patient_ID"), variable.name = "Severity")
write.csv(df_long,'NonMotor_2MergeTo1_LongCol.csv', row.names = F)

####### 3 Features
df = vroom("NonMotor_3MergeTo1.csv")
df_long <- melt (df, id.vars = c("Patient_ID"), variable.name = "Features")
write.csv(df_long,'NonMotor_3MergeTo1_LongCol.csv', row.names = F)
