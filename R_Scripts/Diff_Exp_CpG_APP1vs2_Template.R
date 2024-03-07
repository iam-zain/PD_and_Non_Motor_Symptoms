setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Male\\GeneCpG\\Hopkins")
options(scipen = 999) #Viewing output values as decimal values

dframe <- readTargets("Hopkins_with_MethylomeDetails_Male.csv",sep = ",") #limma
dframe1 = dframe [, -c(1,2,3,6,7,8)] #removing few columns


############# 1. HYS 0 in Patient vs All in Healthy ######################
df_HYS <- subset(dframe1, APPRDX == 2 | (APPRDX == 1 & HYS == 0))
df_HYS <- arrange(df_HYS, (APPRDX)) #arranging or sorting df as per APPRDX
table (df_HYS$APPRDX) #checking data head
df_HYS <- t(df_HYS) #Transposing data
head (df_HYS [1:11, 1:6])
df_HYS = df_HYS [-c(1,2), ] #Removing APPRDX row [ as not required now]
head(df_HYS [1:6, 1:6])

group <- factor(c(rep("Patient",20),rep("Healthy",32))) #Grouping as per categories available
design <- model.matrix(~group) #Designing
y <- DGEList(counts=df_HYS, group=group)
y <- estimateDisp(y, design)
fit <- glmQLFit(y, design)
qlf.2vs1 <- glmQLFTest(fit, coef=2)
HYS0 = topTags(qlf.2vs1, n=3, adjust.method="BH", sort.by="PValue", p.value=1) # top DEs
write.csv(HYS0, 'HYS0_topCpG_Hopkins_Male.csv')



############# 1. HYS 1 in Patient vs All in Healthy ######################
df_HYS <- subset(dframe1, APPRDX == 2 | (APPRDX == 1 & HYS == 1))
df_HYS <- arrange(df_HYS, (APPRDX)) #arranging or sorting df as per APPRDX
table (df_HYS$APPRDX) #checking data head
df_HYS <- t(df_HYS) #Transposing data
df_HYS = df_HYS [-c(1,2), ] #Removing APPRDX row [ as not required now]
head(df_HYS [1:6, 1:6])

group <- factor(c(rep("Patient",13),rep("Healthy",32))) #Grouping as per categories available
design <- model.matrix(~group) #Designing
y <- DGEList(counts=df_HYS, group=group)
y <- estimateDisp(y, design)
fit <- glmQLFit(y, design)
qlf.2vs1 <- glmQLFTest(fit, coef=2)
HYS0 = topTags(qlf.2vs1, n=3, adjust.method="BH", sort.by="PValue", p.value=1) # top DEs
write.csv(HYS0, 'HYS1_topCpG_Hopkins_Male.csv')



############# 1. HYS 1 in Patient vs All in Healthy ######################
df_HYS <- subset(dframe1, APPRDX == 2 | (APPRDX == 1 & HYS == 2))
df_HYS <- arrange(df_HYS, (APPRDX)) #arranging or sorting df as per APPRDX
table (df_HYS$APPRDX) #checking data head
df_HYS <- t(df_HYS) #Transposing data
df_HYS = df_HYS [-c(1,2), ] #Removing APPRDX row [ as not required now]
head(df_HYS [1:6, 1:6])

group <- factor(c(rep("Patient",41),rep("Healthy",32))) #Grouping as per categories available
design <- model.matrix(~group) #Designing
y <- DGEList(counts=df_HYS, group=group)
y <- estimateDisp(y, design)
fit <- glmQLFit(y, design)
qlf.2vs1 <- glmQLFTest(fit, coef=2)
HYS0 = topTags(qlf.2vs1, n=3, adjust.method="BH", sort.by="PValue", p.value=1) # top DEs
write.csv(HYS0, 'HYS2_topCpG_Hopkins_Male.csv')















#VolcanoPlot
excel2v1$Diff_Express_CpG <- "No" #Creating column with diff express
excel2v1$Diff_Express_CpG[excel2v1$logFC > 0.05 & excel2v1$PValue < 0.7] <- "Up"
excel2v1$Diff_Express_CpG[excel2v1$logFC < -0.05 & excel2v1$PValue < 0.7] <- "Down"
excel2v1$delabel <- NA #Creating column with list of Diff exp
excel2v1$delabel[excel2v1$Diff_Express_CpG != "No"] <- excel2v1$CpgIDs[excel2v1$Diff_Express_CpG != "No"]

ggplot(data=excel2v1, aes(x=logFC, y=-log10(PValue), col=Diff_Express_CpG, label=delabel)) + 
  geom_point() + ggtitle("Diff Exp CpG UPSIT3- Healthy vs Patient") + theme_minimal() + geom_text_repel()+
  theme_minimal_hgrid() + scale_x_continuous(breaks =seq(-1, 1, 0.05)) +
  theme(legend.position=c(0.83, 0.9), legend.background = element_rect(fill="white",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(panel.grid.major.x = element_line(color = "grey",size = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.2,linetype = 3))

write.csv(excel2v1, "DEC_UPSIT3_APPRDX_2v1.csv")



############# 2. Within Patient  ######################

dframe1 <- dframe [which(dframe$APPRDX == 1),]
dframe1 = dframe [, -c(1,2,3,4,5)] #removing few columns
dframe1 <- arrange(dframe1, (UPSIT_3)) #arranging or sorting df as per UPSIT_3
table (dframe1$UPSIT_3) #checking data head #139 210 109 
dframe1 <- t(dframe1) #Transposing data
head (dframe1 [1:11, 1:11])
dframe1 = dframe1 [-c(1), ] #Removing UPSIT_3 row [ as not required now]
head(dframe1 [1:11, 1:11])

group <- factor(c(rep("Normal",139),rep("Mild",210), rep("Severe", 109)))
design <- model.matrix(~group)
y <- DGEList(counts=dframe1, group=group)
y <- estimateDisp(y, design)
fit <- glmQLFit(y, design)

qlf.2vs1 <- glmQLFTest(fit, coef=2)
qlf.3vs1 <- glmQLFTest(fit, coef=3)
qlf.3vs2 <- glmQLFTest(fit, contrast=c(0,-1,1))

topTags(qlf.2vs1, n=10, adjust.method="BH", sort.by="PValue", p.value=1) # top DEs

excel2v1 <- qlf.2vs1@.Data[[17]] #creating df
excel3v1 <- qlf.3vs1@.Data[[17]]
excel3v2 <- qlf.3vs2@.Data[[17]]
write.csv(excel2v1, "UPSIT3_DE_CategWise_2v1.csv")
write.csv(excel3v1, "UPSIT3_DE_CategWise_3v1.csv")
write.csv(excel3v2, "UPSIT3_DE_CategWise_3v2.csv")

###### 2.1. Mild vs Normal Volcano Plot ######

excel2v1 <- qlf.2vs1@.Data[[17]] #creating df
excel2v1$CpgIDs = rownames(excel2v1)
head(excel2v1)
write.csv(excel2v1, "UPSIT3_DE_APPRDXWise_2v1.csv")

excel2v1$Diff_Express_CpG <- "No" #Creating column with diff express
excel2v1$Diff_Express_CpG[excel2v1$logFC > 0.1 & excel2v1$PValue < 0.5] <- "Up"
excel2v1$Diff_Express_CpG[excel2v1$logFC < -0.05 & excel2v1$PValue < 0.6] <- "Down"
excel2v1$delabel <- NA #Creating column with list of Diff exp
excel2v1$delabel[excel2v1$Diff_Express_CpG != "No"] <- excel2v1$CpgIDs[excel2v1$Diff_Express_CpG != "No"]

ggplot(data=excel2v1, aes(x=logFC, y=-log10(PValue), col=Diff_Express_CpG, label=delabel)) + 
  geom_point() + ggtitle("Diff Exp CpG UPSIT3 Patient- Mild vs Normal") + theme_minimal() + geom_text_repel()+
  theme_minimal_hgrid() + scale_x_continuous(breaks =seq(-1, 1, 0.05)) +
  theme(legend.position=c(0.7, 0.9), legend.background = element_rect(fill="white",size=0.1,
                                                                      linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=2))+
  theme(panel.grid.major.x = element_line(color = "grey",size = 0.2,linetype = 3))+
  theme(panel.grid.major.y = element_line(color = "grey",size = 0.2,linetype = 3))

write.csv(excel2v1, "UPSIT3_DE_APPRDXWise_2v1.csv")

###### 2.2. Severe vs Normal Volcano Plot ###### 

excel3v1 <- qlf.3vs1@.Data[[17]] #creating df
excel3v1$CpgIDs = rownames(excel3v1)
head(excel3v1)
write.csv(excel3v1, "UPSIT3_DE_Categ2v0Wise_3v1.csv")

excel3v1$Diff_Expressed <- "NO" #Creating column with diff express
excel3v1$Diff_Expressed[excel3v1$logFC > 0.2 & excel3v1$PValue < 0.5] <- "UP"
excel3v1$Diff_Expressed[excel3v1$logFC < -0.3 & excel3v1$PValue < 0.5] <- "DOWN"
excel3v1$delabel <- NA #Creating column with list of Diff exp
excel3v1$delabel[excel3v1$Diff_Expressed != "NO"] <- excel3v1$CpgIDs[excel3v1$Diff_Expressed != "NO"]
ggplot(data=excel3v1, aes(x=logFC, y=-log10(PValue), col=Diff_Expressed, label=delabel)) + 
  geom_point() + ggtitle("Diff_Exp_CpG_UPSIT3_Categ2v0Wise_3v1") + theme_minimal() + geom_text()

###### 2.3. Severe vs Mild Volcano Plot ###### 

excel3v2 <- qlf.3vs2@.Data[[17]] #creating df
excel3v2$CpgIDs = rownames(excel3v2)
head(excel3v2)
write.csv(excel3v2, "UPSIT3_DE_Categ2v1Wise_3v2.csv")

excel3v2$Diff_Expressed <- "NO" #Creating column with diff express
excel3v2$Diff_Expressed[excel3v2$logFC > 0.3 & excel3v2$PValue < 0.5] <- "UP"
excel3v2$Diff_Expressed[excel3v2$logFC < -0.3 & excel3v2$PValue < 0.5] <- "DOWN"
excel3v2$delabel <- NA #Creating column with list of Diff exp
excel3v2$delabel[excel3v2$Diff_Expressed != "NO"] <- excel3v2$CpgIDs[excel3v2$Diff_Expressed != "NO"]
ggplot(data=excel3v2, aes(x=logFC, y=-log10(PValue), col=Diff_Expressed, label=delabel)) + 
  geom_point() + ggtitle("Diff_Exp_CpG_UPSIT3_Categ2v1Wise_3v2") + theme_minimal() + geom_text()

