setwd ("Z:\\PPMI_Data\\Excel_Data\\NonMotors\\Feature_CpG_Gene_Association\\LightHead_Processed_Files")
df = read.csv("LightHeadCateg_Methylome_APPRDX_Common.csv",header = T)
df <- df  %>% select(-c(1,2,4,6))
df <- df[!(df$HYS == 4), ] #Removing HYS4
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'

### HYS0 ###
df_HYS0 <- df [which(df$HYS == 0),]
df_long <- melt (df_HYS0, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS)
df_long$APPRDX = as.character(df_long$APPRDX)

ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.9, 0.9), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("Common CpG Methylation - LightHead - HYS=0")

### HYS1 ###
df_HYS1 <- df [which(df$HYS == 1),]
df_long <- melt (df_HYS1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS)
df_long$APPRDX = as.character(df_long$APPRDX)
ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.9, 0.9), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("Common CpG Methylation - LightHead - HYS=1")

### HYS2 ###
df_HYS2 <- df [which(df$HYS == 2),]
df_long <- melt (df_HYS2, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS)
df_long$APPRDX = as.character(df_long$APPRDX)
ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.9, 0.9), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("Common CpG Methylation - LightHead - HYS=2")

### HYS3 ###
df_HYS3 <- df [which(df$HYS == 3),]
df_long <- melt (df_HYS1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS)
df_long$APPRDX = as.character(df_long$APPRDX)
ggplot(df_long, aes(x= CpG, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.9, 0.9), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("Common CpG Methylation - LightHead - HYS=3")

