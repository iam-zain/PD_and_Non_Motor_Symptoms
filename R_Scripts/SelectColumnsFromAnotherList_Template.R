setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Male\\GeneCpG\\Xtras\\TopCpG_NMI_Compare")

df <- read.csv('CommonCpG_ofEachFeats_Male.csv')
df_NMI <- read.csv('CpG_CommonChrNMI_comFeat_Male.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = df[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$PATNO <- df$PATNO
SelectCpGsRaw$APPRDX <- df$APPRDX
write.csv(SelectCpGsRaw, 'CpG_CommonChrNMI_comFeat_Male_Data.csv', row.names = F)


setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Female\\GeneCpG\\Xtras\\TopCpG_NMI_Compare")

df <- read.csv('CommonCpG_ofEachFeats_Female.csv')
df_NMI <- read.csv('CpG_CommonChrNMI_comFeat_Female.csv')
CpgNames <- df_NMI$CpG
SelectCpGsRaw = df[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$PATNO <- df$PATNO
SelectCpGsRaw$APPRDX <- df$APPRDX
write.csv(SelectCpGsRaw, 'CpG_CommonChrNMI_comFeat_Female_Data.csv', row.names = F)
