setwd ("Z:\\PPMI_Data\\Excel_Data\\NonMotors\\Feature_CpG_Gene_Association\\UPSIT_Processed_Files")
df = read.csv("UPSIT_CommonCpG_inTop50_NMIVsBorutaData.csv",header = T)
df <- df  %>% select(-c(3)) #Removing UPSIT Score Column
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'

#### 1. cg05142211 ####
df_1 <- df  %>% select(c(1,2,3))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")

df_long$HYS = as.character(df_long$HYS) #UPSIT_Methyl_HYS_wise_inPatCont_cg05142211

ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.85), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg05142211 Methylation -UPSIT ")

#### 2. cg13959611 ####
df_1 <- df  %>% select(c(1,2,4))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.1), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg13959611 Methylation -UPSIT ")

#### 3. cg04875789 ####
df_1 <- df  %>% select(c(1,2,5))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.85), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg04875789 Methylation -UPSIT")

#### 4. cg03682895 ####
df_1 <- df  %>% select(c(1,2,6))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.1), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg03682895 Methylation -UPSIT")

#### 5. cg18322516 ####
df_1 <- df  %>% select(c(1,2,7))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.1), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg18322516 Methylation -UPSIT")

#### 6. cg24215869 ####
df_1 <- df  %>% select(c(1,2,8))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.85), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg24215869 Methylation -UPSIT")

#### 7. cg03827764 ####
df_1 <- df  %>% select(c(1,2,9))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.1), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg03827764 Methylation -UPSIT")

#### 8. cg24628866 ####
df_1 <- df  %>% select(c(1,2,10))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.11), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg24628866 Methylation -UPSIT")

#### 9. cg10061292 ####
df_1 <- df  %>% select(c(1,2,11))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.85), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg10061292 Methylation -UPSIT")

#### 10. cg00260798 ####
df_1 <- df  %>% select(c(1,2,12))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.88), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg00260798 Methylation -UPSIT")

