setwd ("Z:\\PPMI_Data\\Excels\\Only_Male\\GeneCpG\\Apathy")
df = read.csv("Common_Apathy_CpG_Male.csv",header = T)
df <- df  %>% select(-c(1)) #Removing PATNO Column
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'

#### 1. cg06485214 ####
df_1 <- df  %>% select(c(1,2,3))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")

df_long$HYS = as.character(df_long$HYS) #Apathy_Methyl_HYS_wise_inPatCont_cg05142211

ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.15), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("Apathy- cg06485214 Methylation -Male")

#### 2. cg27521571 ####
df_1 <- df  %>% select(c(1,2,4))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.15), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("Apathy- cg27521571 Methylation -Male")

#### 3. cg01447828 ####
df_1 <- df  %>% select(c(1,2,5))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.15), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("Apathy- cg01447828 Methylation -Male")

#### 4. cg02157463 ####
df_1 <- df  %>% select(c(1,2,6))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.80, 0.87), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("Apathy- cg02157463 Methylation -Male")

#### 5. cg11865360 ####
df_1 <- df  %>% select(c(1,2,7))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.85), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("Apathy- cg11865360 Methylation -Male")

#### 6. cg03605226 ####
df_1 <- df  %>% select(c(1,2,8))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.85), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("Apathy- cg03605226 Methylation -Male")

#### 7. cg03984780 ####
df_1 <- df  %>% select(c(1,2,9))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.85), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("Apathy- cg03984780 Methylation -Male")

#### 8. cg05791411 ####
df_1 <- df  %>% select(c(1,2,10))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.85), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("Apathy- cg05791411 Methylation -Male")

#### 9. cg07777378 ####
df_1 <- df  %>% select(c(1,2,11))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.85), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("Apathy- cg07777378 Methylation -Male")

#### 10. cg18181070 ####
df_1 <- df  %>% select(c(1,2,12))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.85), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("Apathy- cg18181070 Methylation -Male")


#### 11. cg18262028 ####
df_1 <- df  %>% select(c(1,2,13))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.86, 0.85), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  xlab("\nHYS") + ylab("Methylation Value\n") +
  ggtitle ("Apathy- cg18262028 Methylation -Male")


