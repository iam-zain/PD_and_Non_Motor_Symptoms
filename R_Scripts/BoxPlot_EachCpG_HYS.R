setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\Apathy")
df = read.csv("Common10_Apathy_DataHYS.csv",header = T)
df <- df  %>% select(-c(1)) #Removing PATNO
df <- df[!(df$HYS == 4), ] #Removing HYS4
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'

#### 1. cg00356916 ####
df_1 <- df  %>% select(c(1,2,3))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")

df_long$HYS = as.character(df_long$HYS) #Apathy_Methyl_HYS_wise_inPatCont_cg05142211

ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.81, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + ylab ("Methylation Value\n") +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  ggtitle ("cg02157463 Methylation -Apathy")

#### 2. cg24183574 ####
df_1 <- df  %>% select(c(1,2,4))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + ylab ("Methylation Value\n") +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  ggtitle ("cg04333485 Methylation -Apathy")

#### 3. cg12526471 ####
df_1 <- df  %>% select(c(1,2,5))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + ylab ("Methylation Value\n") +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  ggtitle ("cg08187983 Methylation -Apathy")

#### 4. cg25101936 ####
df_1 <- df  %>% select(c(1,2,6))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + ylab ("Methylation Value\n") +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  ggtitle ("cg11486829 Methylation -Apathy")

#### 5. cg21049840 ####
df_1 <- df  %>% select(c(1,2,7))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + ylab ("Methylation Value\n") +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  ggtitle ("cg03605226 Methylation -Apathy")

#### 6. cg12683454 ####
df_1 <- df  %>% select(c(1,2,8))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + ylab ("Methylation Value\n") +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  ggtitle ("cg01366849 Methylation -Apathy")

#### 7. cg13271751 ####
df_1 <- df  %>% select(c(1,2,9))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + ylab ("Methylation Value\n") +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  ggtitle ("cg14498209 Methylation -Apathy")

#### 8. cg12220605 ####
df_1 <- df  %>% select(c(1,2,10))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + ylab ("Methylation Value\n") +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  ggtitle ("cg01081395 Methylation -Apathy")

#### 9. cg12220605 ####
df_1 <- df  %>% select(c(1,2,11))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + ylab ("Methylation Value\n") +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  ggtitle ("cg17741809 Methylation -Apathy")

#### 10. cg12220605 ####
df_1 <- df  %>% select(c(1,2,12))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 16, color = "black"),
        legend.background = element_rect(fill="azure",linewidth=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) + ylab ("Methylation Value\n") +
  theme(axis.text=element_text(color="black"))+  theme(plot.title = element_text(face="bold", size=18)) +
  theme(axis.title = element_text(face="bold")) + 
  ggtitle ("cg26200585 Methylation -Apathy")
