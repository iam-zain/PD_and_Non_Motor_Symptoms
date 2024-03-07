setwd ("Z:\\PPMI_Data\\Excel_Data\\NonMotors\\Feature_CpG_Gene_Association\\Constipation_Processed_Files")
df = read.csv("ConstipationCateg_Methylome_APPRDX_Common.csv",header = T)
df <- df  %>% select(-c(1,2,4,6))
df <- df[!(df$HYS == 4), ] #Removing HYS4
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'

#### 1. cg13332222 ####
df_1 <- df  %>% select(c(1,2,3))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")

df_long$HYS = as.character(df_long$HYS) #Constipation_Methyl_HYS_wise_inPatCont_cg05142211

ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg13332222 Methylation -Constipation")

#### 2. cg20349377 ####
df_1 <- df  %>% select(c(1,2,4))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+
  geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.15), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg20349377 Methylation -Constipation")

#### 3. cg17528967 ####
df_1 <- df  %>% select(c(1,2,5))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.78, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg17528967 Methylation -Constipation")
		
#### 4. cg12298429 ####
df_1 <- df  %>% select(c(1,2,6))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg12298429 Methylation -Constipation")

#### 5. cg27281389 ####
df_1 <- df  %>% select(c(1,2,7))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg27281389 Methylation -Constipation")

#### 6. cg18704989 ####
df_1 <- df  %>% select(c(1,2,8))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg18704989 Methylation -Constipation")

#### 7. cg21899921 ####
df_1 <- df  %>% select(c(1,2,9))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg21899921 Methylation -Constipation")

#### 8. cg05142211 ####
df_1 <- df  %>% select(c(1,2,10))
df_long <- melt (df_1, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 
ggplot(df_long, aes(x= HYS, y=value, color = APPRDX))+ geom_boxplot()+  ylab("Methylation Value") + 
  theme(legend.position=c(0.85, 0.85), axis.text = element_text(face="bold"),text = element_text(size = 15, color = "black"),
        legend.background = element_rect(fill="azure",size=0.1, linetype="solid", colour ="black"),
        panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("cg05142211 Methylation -Constipation")


