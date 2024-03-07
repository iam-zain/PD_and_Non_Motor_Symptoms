########## Combined ###########

###### 1. Feature wise compare ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\RanFor")
Non_Mot = read.csv("NonMot_RanFor_Compare.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_Features','Decision_Tree','Random_Forest','Boruta','NMI',
'All15_in4Methods','Common9_in4Methods','Random_9Features')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, color = "black")) +
  ylab("Model Accuracy\n") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title = element_text(face = "bold")) + 
  theme(axis.text = element_text(face = "bold")) + 
  theme(text = element_text(size = 18)) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  ggtitle (" Various set of Features- Patient Vs Healthy Control ")



###### 2. Whole chromosome: CpG wise compare ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\Xtras\\CommonCpG_Data_12CommonFeats")
Non_Mot = read.csv("BoxPlot_ChrNMI_CommonFeats_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('NMI_0.05','Random_58CpG','NMI_0.04','Random_619CpG',
'Common_from11Feats','Random_54CpG','Common_inNMIandFeats','Random_9CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, color = "black")) +
  ylab("Model Accuracy\n") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title = element_text(face = "bold")) + 
  theme(axis.text = element_text(face = "bold")) + 
  theme(text = element_text(size = 18)) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  scale_fill_manual(values=c("#36C3D1","#36C3D1","#36C3D1","#36C3D1","#F1746B","#F1746B","#F1746B","#F1746B")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control")



###### 3. Uncategorized vs Categorized Feature ######

### Uncategorized only ###
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Main21")
Non_Mot = read.csv("BoxPlot_RanFor_Diff_Analyses_Main21Categorized.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_21Features','Top_10Features','Random_10Features')),
  y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, color = "black")) +
  ylab("Model Accuracy\n") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title = element_text(face = "bold")) + 
  theme(axis.text = element_text(face = "bold")) + 
  theme(text = element_text(size = 18)) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  ggtitle (" Main set of Features- Patient Vs Healthy Control")


### Uncategorized Vs Categorized: 45 Features ###
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\RanFor")
Non_Mot = read.csv("Uncategorized_Categorized_45Feats_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('Uncategorized','Categorized')),
  y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot() + scale_y_continuous (breaks = seq(0, 1, by = 0.02)) + 
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 18, color = "black")) +
  ylab("Model Accuracy\n") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title = element_text(face = "bold")) + 
  theme(axis.text = element_text(face = "bold")) + 
  theme(text = element_text(size = 18)) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  ggtitle ("45 Features - Uncategorized Vs Categorized")


### Uncategorized Vs Categorized: Top Features ###
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\RanFor")
Non_Mot = read.csv("BoxPlot_RanFor_Diff_Analyses_21Vs45Categorized_Top.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('Top_10Features','Common_12Features')),
  y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.99, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle (" Top10-Main Vs Common12-sub Features- Patient Vs Healthy Control")





###### 4. Each Feature: CpG wise compare ######

### Apathy ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\Apathy")
Non_Mot = read.csv("Apathy_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_93CpG','Random_93CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - Apathy")



### Cognition ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\Cognition")
Non_Mot = read.csv("Cognition_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_97CpG','Random_97CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - Cognition")


### Constipation ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\Constipation")
Non_Mot = read.csv("Constipation_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_93CpG','Random_93CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - Constipation")


### Dream ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\GeneCpG\\Dream")
Non_Mot = read.csv("Dream_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_84CpG','Random_84CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - Dream")


### Fatigue ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\Fatigue")
Non_Mot = read.csv("Fatigue_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_97CpG','Random_97CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - Fatigue")


### Gastro ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\Gastro")
Non_Mot = read.csv("Gastro_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_98CpG','Random_98CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - Gastro")



### LetterNumber ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\LetterNumber")
Non_Mot = read.csv("LetterNumber_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_96CpG','Random_96CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - LetterNumber")


### Symbol Digit ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\SymbolDigit")
Non_Mot = read.csv("SymbDigi_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_99CpG','Random_99CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - SymbolDigit")


### Trail Making A ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\GeneCpG\\TrailMakeA")
Non_Mot = read.csv("TrailMakeA_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_97CpG','Random_97CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - TrailMakeA")


### TrailMakeB ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\TrailMakeB")
Non_Mot = read.csv("TrailMakeB_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_97CpG','Random_97CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - TrailMakeB")



### UPSIT ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\UPSIT")
Non_Mot = read.csv("UPSIT_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_92CpG','Random_92CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - UPSIT")




###### 5 Individual all 45 features ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Files")
Non_Mot = read.csv("NonMot_Indi45Feats_Combined_100svmL.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = Feature, y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, color = "black")) +
  ylab("Model Accuracy\n") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title = element_text(face = "bold")) + 
  theme(axis.text = element_text(face = "bold")) + 
  theme(text = element_text(size = 18)) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  ggtitle ("SVM Linear - Model Accuracy with Individual Features - Patient Vs Healthy Control")




### 10 vs 25 vs 50 vs 75 vs 100 ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\GeneCpG\\Apathy\\Diferent_Sets")
Non_Mot = read.csv("Apathy_10vs25vs50vs75vs100_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('CpG_10','CpG_25','CpG_50','CpG_75','CpG_100')),
                         y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 18, angle = 0, vjust = 1, hjust = 0.5, color = "black")) +
  ylab("Model Accuracy\n") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title = element_text(face = "bold")) + 
  theme(axis.text = element_text(face = "bold")) + 
  theme(text = element_text(size = 18)) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  scale_fill_brewer(palette="RdPu") +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control (Apathy)")




########### 100 vs 1000 Run ############
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\GeneCpG\\Apathy")
Non_Mot = read.csv("Apathy_100Vs1000_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = Feature,y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.95, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1")) +
  ggtitle (" 100 times Vs 1000 times- Patient Vs Healthy Control (Apathy)")




########### Various Model ############

setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Various_Models")
Non_Mot = read.csv('Various_ML_Model_Compare.csv', fileEncoding='latin1',check.names=F)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')

ggplot(Non_Mot_Long, aes(x = reorder(Feature, Model_Accuracy), y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 1, hjust = 1, color = "black")) +
  ylab("Model Accuracy\n") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title = element_text(face = "bold")) + 
  theme(axis.text = element_text(face = "bold")) + 
  theme(text = element_text(size = 18)) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  ggtitle ("Model Comparison - Patient Vs Healthy Control (Uncategorized)")

