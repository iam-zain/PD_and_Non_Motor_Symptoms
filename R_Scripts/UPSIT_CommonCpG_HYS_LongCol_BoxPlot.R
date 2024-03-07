setwd ("Z:\\PPMI_Data\\Excel_Data\\NonMotors\\Feature_CpG_Gene_Association\\UPSIT_Processed_Files")
df = read.csv("UPSIT_CommonCpG_inTop50_NMIVsBorutaData.csv",header = T)
df <- df  %>% select(-c(3))
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'

### HYS0 ### cg05142211, cg13959611, cg04875789, cg10061292
df_HYS0 <- df [which(df$HYS == 0),]
df_1 <- df_HYS0  %>% select(c(1,2,3,4,5,11))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")

df_long$HYS = as.character(df_long$HYS) 

ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.9), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("4 Common CpG Methylation - UPSIT - HYS=0")


### HYS1 ### cg05142211, cg13959611, cg04875789, cg10061292
df_HYS1 <- df [which(df$HYS == 1),]
df_1 <- df_HYS1  %>% select(c(1,2,3,4,5,11))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.9), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("4 Common CpG Methylation - UPSIT - HYS=1")


### HYS2 ### cg05142211, cg13959611, cg04875789, cg10061292
df_HYS2 <- df [which(df$HYS == 2),]
df_1 <- df_HYS2  %>% select(c(1,2,3,4,5,11))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.9), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("4 Common CpG Methylation - UPSIT - HYS=2")


### HYS3 ### cg05142211, cg13959611, cg04875789, cg10061292
df_HYS3 <- df [which(df$HYS == 3),]
df_1 <- df_HYS3  %>% select(c(1,2,3,4,5,11))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.9), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("4 Common CpG Methylation - UPSIT - HYS=3")








### HYS0 ### 
df_HYS0 <- df [which(df$HYS == 0),]
df_1 <- df_HYS0  %>% select(c(1,2,6,7,9,10,12))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.88, 0.9), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("5 Common CpG Methylation - UPSIT - HYS=0")


### HYS1 ### 
df_HYS1 <- df [which(df$HYS == 1),]
df_1 <- df_HYS1  %>% select(c(1,2,6,7,9,10,12))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.88, 0.9), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("5 Common CpG Methylation - UPSIT - HYS=1")


### HYS2 ### 
df_HYS2 <- df [which(df$HYS == 2),]
df_1 <- df_HYS2  %>% select(c(1,2,6,7,9,10,12))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.88, 0.9), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("5 Common CpG Methylation - UPSIT - HYS=2")


### HYS3 ### 
df_HYS3 <- df [which(df$HYS == 3),]
df_1 <- df_HYS3  %>% select(c(1,2,6,7,9,10,12))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.88, 0.9), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("5 Common CpG Methylation - UPSIT - HYS=3")

