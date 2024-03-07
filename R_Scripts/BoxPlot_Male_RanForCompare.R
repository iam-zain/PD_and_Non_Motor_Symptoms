########## Male ###########

###### 1. Feature wise compare ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\RanFor")
Non_Mot = read.csv("NonMot_Male_RanFor_Compare.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_Features','Decision_Tree','Random_Forest','Boruta','NMI',
'All18_in4Methods','Common11_in4Methods','Random_11Features')),y = Model_Accuracy, fill = Feature)) + 
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
  ggtitle (" Various set of Features- Patient Vs Healthy Control - Male")


###### 2. Whole Genome: CpG wise compare ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\Xtras\\Top50_eachFeats")
Non_Mot = read.csv("CpG_Compare_NMI_Feat_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','NMI_07','Random_67CpG','NMI_06','Random_327CpG',
'NMI_055','Random_707CpG','Top50s_11Feat','Random_338CpG')), y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#36C3D1","#36C3D1","#F1746B","#F1746B","#F1746B","#F1746B","#36C3D1")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - Male")


###### 3. Whole Genome with Common Features: CpG wise compare ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\Xtras\\Top50_eachFeats")
Non_Mot = read.csv("CpG_Compare_NMI_withCommonFeat_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('Common11_Features','NMI_07','NMI_07_withComFeats','NMI_06',
'NMI_06_withComFeats','NMI_055','NMI_055_withComFeats','Top50s_11Feat','Top50s_withComFeats')), y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 1, hjust = 1, color = "black")) +
  ylab("Model Accuracy\n") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title = element_text(face = "bold")) + 
  theme(axis.text = element_text(face = "bold", size = 20)) + 
  theme(text = element_text(size = 20)) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(plot.title = element_text(face = "bold", size = 20)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#36C3D1","#F1746B","#36C3D1","#F1746B","#36C3D1","#F1746B")) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - Male")


###### 4. Each Feature: CpG wise compare ######
### UPSIT ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Male\\GeneCpG\\UPSIT")
Non_Mot = read.csv("UPSIT_CpG_Ranfor_Compare_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_96CpG','Random_96CpG')),y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle ("Male - Various set of CpGs- Patient Vs Healthy Control - UPSIT")


### TrailMakingB ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\TrailMakingB")
Non_Mot = read.csv("TrlMakB_CpG_Ranfor_Compare_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_97CpG','Random_97CpG')),y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle ("Male - Various set of CpGs- Patient Vs Healthy Control - TrailMaking-B")


### TrailMakingA ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\TrailMakingA")
Non_Mot = read.csv("TrailMakeA_CpG_Ranfor_Compare_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_100CpG','Random_100CpG')),y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle ("Male - Various set of CpGs- Patient Vs Healthy Control - TrailMaking-A")


### SymbDigi ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\SymbDigi")
Non_Mot = read.csv("SymbDigi_CpG_Ranfor_Compare_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_99CpG','Random_99CpG')),y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle ("Male - Various set of CpGs- Patient Vs Healthy Control - Symbol Digit")


### SleepDay ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\SleepDay")
Non_Mot = read.csv("SleepDay_CpG_Ranfor_Compare_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_95CpG','Random_95CpG')),y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle ("Male - Various set of CpGs- Patient Vs Healthy Control - SleepDay")




### ScopaSex ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\ScopaSex")
Non_Mot = read.csv("ScopaSex_CpG_Ranfor_Compare_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_96CpG','Random_96CpG')),y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle ("Male - Various set of CpGs- Patient Vs Healthy Control - ScopaSex")



### Fatigue ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\Fatigue")
Non_Mot = read.csv("Fatigue_CpG_Ranfor_Compare_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_98CpG','Random_98CpG')),y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle ("Male - Various set of CpGs- Patient Vs Healthy Control - Fatigue")



### Depression ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\Depression")
Non_Mot = read.csv("Depression_CpG_Ranfor_Compare_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_98CpG','Random_98CpG')),y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle ("Male - Various set of CpGs- Patient Vs Healthy Control - Depression")



### Dream ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\Dream")
Non_Mot = read.csv("Dream_CpG_Ranfor_Compare_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_90CpG','Random_90CpG')),y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle ("Male - Various set of CpGs- Patient Vs Healthy Control - Dream")



### Cognition ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\Cognition")
Non_Mot = read.csv("Cognition_CpG_Ranfor_Compare_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_99CpG','Random_99CpG')),y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle ("Male - Various set of CpGs- Patient Vs Healthy Control - Cognition")



### Apathy ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\Apathy")
Non_Mot = read.csv("Apathy_CpG_Ranfor_Compare_Male.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_89CpG','Random_89CpG')),y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#F1746B","#F1746B","#36C3D1","#36C3D1")) +
  ggtitle ("Male - Various set of CpGs- Patient Vs Healthy Control - Apathy")


###### 4 Whole Genome: CpG common and features ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\GeneCpG\\Xtras\\Top50_eachFeats")
Non_Mot = read.csv("NMI_andTop50CpGs_withFeatures_Compare.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('Common_11Features','NMI_06','NMI06_withFeatures','Top50s_11Features',
'Top50s_with11Features')), y = Model_Accuracy, fill = Feature)) + 
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
  scale_fill_manual(values=c("#DCD6F7","#F1746B","#36C3D1","#F1746B","#36C3D1","#F1746B")) +
  ggtitle ("CpGs And Top Features - Patient Vs Healthy Control - Male")



###### 5 Individual all 45 features ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male")
Non_Mot = read.csv("NonMot_Indi45Feats_Male_100svmL.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = Feature, y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot() + 
  theme_bw() +
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 16, angle = 50, vjust = 1, hjust = 1, color = "black")) +
  ylab("Model Accuracy\n") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title = element_text(face = "bold")) + 
  theme(axis.text = element_text(face = "bold")) + 
  theme(text = element_text(size = 18)) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(plot.title = element_text(face = "bold", size = 18)) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  ggtitle ("SVM Linear - Model Accuracy with Individual Features - Patient Vs Healthy Control - Male")
