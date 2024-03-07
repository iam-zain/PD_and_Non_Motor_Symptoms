setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Feature_CpG_Gene_Association\\UPSIT_Processed_Files")

Non_Mot = read.csv("UPSIT_DiffCpGAnalyses_RanFor.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')

ggplot(Non_Mot_Long, aes(x = factor(Feature,level=c('All_CpG','Top50_Boruta','Top50_NMI','Random50_CpG',
'BorutaR7_CpG','Common10_CpG','Random10_CpG')), y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=12, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.9, hjust=1)) + 
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("Random Forest Model Comparison- CpGs of UPSIT- Patient Vs Healthy Control")
