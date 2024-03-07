#### Female
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Female\\RanFor")
Non_Mot = read.csv("NonMot_Indi45Feats_Female_100RF10FCV.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')

ggplot(Non_Mot_Long, aes(x = Feature, y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=12, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.99, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("Random Forest - Model Accuracy with Individual Features - Patient Vs Healthy Control - Female")


#### Male
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Male\\RanFor")
Non_Mot = read.csv("NonMot_Indi45Feats_Male_100RF10FCV.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')

ggplot(Non_Mot_Long, aes(x = Feature, y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=12, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.99, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("Random Forest - Model Accuracy with Individual Features - Patient Vs Healthy Control - Male")


#### Combined
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")
Non_Mot = read.csv("NonMot_Indi45Feats_100RF10FCV.csv",header = T)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')

ggplot(Non_Mot_Long, aes(x = Feature, y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  theme(axis.text.x = element_text(size=12, angle=45, color="black")) +
  theme(axis.text.x = element_text(angle = 50, vjust = 0.99, hjust=1)) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=16)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("Random Forest - Model Accuracy with Individual Features - Patient Vs Healthy Control")

