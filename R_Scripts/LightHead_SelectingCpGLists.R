setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Feature_CpG_Gene_Association\\LightHead_Processed_Files\\Boruta_VariousCpGList") 
df <- read.csv("LightHeadCateg_Methylome_APPRDXFilter.csv", header = T)

df10 <- read.csv('LightHead_Boruta_10.csv')
CpgNames = df10$CpG # Selecting CpGs
dframe10 <- df[,CpgNames] # Fetching data for those CpGs
dframe10$APPRDX <- df$APPRDX
write.csv(dframe10, 'LightHead10.csv', row.names = F)

## 25
df25 <- read.csv('LightHead_Boruta_25.csv')
CpgNames = df25$CpG # Selecting CpGs
dframe25 <- df[,CpgNames] # Fetching data for those CpGs
dframe25$APPRDX <- df$APPRDX
write.csv(dframe25, 'LightHead25.csv', row.names = F)

## 75
df75 <- read.csv('LightHead_Boruta_75.csv')
CpgNames = df75$CpG # Selecting CpGs
dframe75 <- df[,CpgNames] # Fetching data for those CpGs
dframe75$APPRDX <- df$APPRDX
write.csv(dframe75, 'LightHead75.csv', row.names = F)

## 100
df100 <- read.csv('LightHead_Boruta_100.csv')
CpgNames = df100$CpG # Selecting CpGs
dframe100 <- df[,CpgNames] # Fetching data for those CpGs
dframe100$APPRDX <- df$APPRDX
write.csv(dframe100, 'LightHead100.csv', row.names = F)
