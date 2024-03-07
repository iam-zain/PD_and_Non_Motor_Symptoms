setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\Apathy")
df = read.csv('Apathy_with_MethylomeDetails.csv')

df10 = read.csv ('Apathy_10CpG.csv')
CpgNames <- df10$CpG
SelectCpGsRaw = df[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- df$APPRDX
write.csv(SelectCpGsRaw, 'Top10_Apathy_Data.csv', row.names = F)

df25 = read.csv ('Apathy_25CpG.csv')
CpgNames <- df25$CpG
SelectCpGsRaw = df[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- df$APPRDX
write.csv(SelectCpGsRaw, 'Top25_Apathy_Data.csv', row.names = F)

df75 = read.csv ('Apathy_75CpG.csv')
CpgNames <- df75$CpG
SelectCpGsRaw = df[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- df$APPRDX
write.csv(SelectCpGsRaw, 'Top75_Apathy_Data.csv', row.names = F)

df100 = read.csv ('Apathy_100CpG.csv')
CpgNames <- df100$CpG
SelectCpGsRaw = df[,CpgNames] # Taking those columns out from new data
SelectCpGsRaw$APPRDX <- df$APPRDX
write.csv(SelectCpGsRaw, 'Top100_Apathy_Data.csv', row.names = F)
