########## Female ###########

###### 1. Feature wise compare ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\RanFor")
Non_Mot = read.csv("NonMot_Female_RanFor_Compare.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_Features','Decision_Tree','Random_Forest','Boruta','NMI',
'All17_in4Methods','Common5_in4Methods','Random_5Features')),y = Model_Accuracy, fill = Feature)) + 
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
  ggtitle (" Various set of Features- Patient Vs Healthy Control - Female")


###### 2. Whole Genome: CpG wise compare ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\GeneCpG\\Xtras\\Top50_eachFeats")
Non_Mot = read.csv("CpG_Compare_NMI_Feat_Female.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')

ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','NMI_15','Random_62CpG','NMI_13','Random_332CpG','NMI_12',
'Random_728CpG','Top50s_5Feat','Random_228CpG')), y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) + 
  theme_bw() +  # This sets the background to white
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 18, angle = 45, vjust = 0.99, hjust = 1, color = "black")) +
  ylab("Model Accuracy\n") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title = element_text(face = "bold")) + 
  theme(axis.text = element_text(face = "bold")) + 
  theme(text = element_text(size = 18)) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(plot.title = element_text(face = "bold", size = 18)) +
  scale_fill_manual(values=c("#DCD6F7","#36C3D1","#36C3D1","#36C3D1","#F1746B","#F1746B","#F1746B","#F1746B","#36C3D1")) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle (" Various set of CpGs- Patient Vs Healthy Control - Female")


###### 3. Whole Genome with Common Features: CpG wise compare ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\GeneCpG\\Xtras\\Top50_eachFeats")
Non_Mot = read.csv("CpG_Compare_NMI_withCommonFeat_Female.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature, level=c('Common5_Features','NMI_15','NMI_15_withComFeats','NMI_13',
  'NMI_13_withComFeats','NMI_12','NMI_12_withComFeats','Top50s_5Feat','Top50s_withComFeats')), 
  y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) + 
  theme_bw() +  # This sets the background to white
  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size = 20, angle = 45, vjust = 1.05, hjust = 1, color = "black")) +
  ylab("Model Accuracy\n") +
  theme(axis.title.x = element_blank()) + 
  theme(axis.title = element_text(face = "bold")) + 
  theme(axis.text = element_text(face = "bold", size = 20)) + 
  theme(text = element_text(size = 20)) + 
  theme(axis.text = element_text(color = "black")) + 
  theme(plot.title = element_text(face = "bold", size = 20)) +
  scale_fill_manual(values = c("#DCD6F7", "#36C3D1", "#F1746B", "#36C3D1", "#F1746B", "#36C3D1", "#F1746B", "#36C3D1", "#F1746B")) +
  theme(panel.border = element_rect(colour = "black", fill = NA, size = 1)) + 
  ggtitle("Various set of CpGs - Patient Vs Healthy Control - Female")



###### 4. Each Feature: CpG wise compare ######
### Cognition ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\GeneCpG\\Cognition")
Non_Mot = read.csv("Cognition_CpG_Ranfor_Compare_Female.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_98CpG','Random_98CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) + 
  theme_bw() +  # This sets the background to white
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
  ggtitle ("Female -  Various set of CpGs- Patient Vs Healthy Control - Cognition")

### Constipation ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\GeneCpG\\Constipation")
Non_Mot = read.csv("Constipation_CpG_Ranfor_Compare_Female.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_98CpG','Random_98CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) + 
  theme_bw() +  # This sets the background to white
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
  ggtitle ("Female -  Various set of CpGs- Patient Vs Healthy Control - Constipation")


### Gastro ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\GeneCpG\\Gastro")
Non_Mot = read.csv("Gastro_CpG_Ranfor_Compare_Female.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_97CpG','Random_97CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) + 
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
  ggtitle ("Female -  Various set of CpGs- Patient Vs Healthy Control - Gastro")



### Pain ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\GeneCpG\\Pain")
Non_Mot = read.csv("Pain_CpG_Ranfor_Compare_Female.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_99CpG','Random_99CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) + 
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
  ggtitle ("Female -  Various set of CpGs- Patient Vs Healthy Control - Pain")



### UPSIT ### 
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\GeneCpG\\UPSIT")
Non_Mot = read.csv("UPSIT_CpG_Ranfor_Compare_Female.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random_50CpG',
'Merged_95CpG','Random_95CpG')),y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) + 
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
  ggtitle ("Female -  Various set of CpGs- Patient Vs Healthy Control - UPSIT")




###### 4 Whole Genome: Top features,Top 50 CpGs and NMI  ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\RanFor")
Non_Mot = read.csv("NMI_andTop50CpGs_withFeatures_Compare_Female.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('Common_5Features','NMI_13','NMI13_with5Features','Top50s_in5Features',
'Top50s_with5Features')), y = Model_Accuracy, fill = Feature)) + 
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
  ggtitle ("CpGs And Top Features - Patient Vs Healthy Control - Female")



###### 5 Individual all 45 features ######
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female")
Non_Mot = read.csv("NonMot_Indi45Feats_Female_100svmL.csv",header = T)
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
  ggtitle ("SVM Linear - Model Accuracy with Individual Features - Patient Vs Healthy Control - Female")
