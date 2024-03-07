### Female: CpGs obtained from Top 50 CpGs from Top/Selected 5 Features ###

setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\Model_CpG")
Non_Mot = read.csv('ML_Model_Compare_CpG_Female.csv', fileEncoding='latin1',check.names=F)
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')

ggplot(Non_Mot_Long, aes(x = reorder(Feature, Model_Accuracy), y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot(position = position_dodge()) +  theme(legend.position = "none") + 
  scale_y_continuous (breaks = seq(0, 100, by = 0.05)) +
  theme(axis.text.x = element_text(size=18, angle = 50, vjust = 0.99, hjust=1, color="black")) + ylab ("Model Accuracy\n") +
  theme(axis.title.x=element_blank()) + theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(face="bold")) + theme(text = element_text(size=18)) + 
  theme(axis.text=element_text(color="black")) + theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("Model Compare: Patient Vs Healthy: Top50 CpGs of Top5 Features: Female")


## Time taken to run the model ##
df_Bar = read.csv("ML_Model_Compare_CpG_Female_Time_Modify.csv")
df_Bar$Model <- factor(df_Bar$Model, levels = df_Bar$Model[order(df_Bar$Time)])
ggplot(df_Bar, aes(x = Model, y = Time, fill = Model)) + 
  geom_bar(stat = 'identity') + 
  theme(legend.position = 'none') +
  theme(axis.text.x = element_text(size=14, face = 'bold', angle = 50, vjust = 0.98, hjust=1, color="black")) +
  ylab ("Time (in seconds)\n") + scale_y_continuous (breaks = seq(0, 800, by = 50)) +
  theme(axis.title.x=element_blank()) + 
  theme(axis.title = element_text(face="bold")) + 
  theme(axis.text = element_text(color="black",face="bold")) + 
  theme(text = element_text(size=16)) +
  theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("Bar plot: Time taken to run the model")


### Male: CpGs obtained from Top 50 CpGs from Top/Selected 5 Features ###
