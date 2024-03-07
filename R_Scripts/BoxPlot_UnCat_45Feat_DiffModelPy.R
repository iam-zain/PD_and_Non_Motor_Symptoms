
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Various_Models")
Non_Mot = read.csv('Various_ML_Model_Compare.csv', fileEncoding='latin1',check.names=F)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')

ggplot(Non_Mot_Long, aes(x = reorder(Feature, Model_Accuracy), y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=18, angle = 50, vjust = 0.99, hjust=1, color="black")) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("Model Comparison - Patient Vs Healthy Control (Uncategorized)")

