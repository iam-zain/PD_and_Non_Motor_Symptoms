setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")
df <- read.csv("NMI_All45Feats_Categorized.csv", header = T)
df <- df %>% filter(!row_number() %in% c(1)) #Removing 1st row i.e. APPRDX
df %>% ggplot(aes(reorder(Features, Normalized), Normalized)) + geom_col (aes(fill= Normalized)) + 
  theme(axis.text = element_text(face="bold")) + ylab ("\nNMI Value") + 
  scale_fill_gradient(low="cornflowerblue",high="deeppink") + coord_flip() + labs (x = 'Features') +
  theme(axis.title = element_text(face="bold")) + theme(legend.title = element_text(face = "bold")) +
  theme(text = element_text(size=16)) + theme(axis.text=element_text(color="black")) +
  theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("NMI Value of 45 Features- Patient Vs Healthy Control")

# Saving image using ggsave
#ggsave("NMI_45Feats_Plot.pdf",width = 9, height = 11)
#ggsave("NMI_45Feats_Plot.tiff",width = 9, height = 11)

setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Main21")
df <- read.csv("NMI_All45Feats_Categorized.csv", header = T)
df <- df %>% filter(!row_number() %in% c(1)) #Removing 1st row i.e. APPRDX
df %>% ggplot(aes(reorder(Features, Normalized), Normalized)) + geom_col (aes(fill= Normalized)) + 
  theme(axis.text = element_text(face="bold")) + ylab ("\nNMI Value") + 
  scale_fill_gradient(low="cornflowerblue",high="deeppink") + coord_flip() + labs (x = 'Features') +
  theme(axis.title = element_text(face="bold")) + theme(legend.title = element_text(face = "bold")) +
  theme(text = element_text(size=16)) + theme(axis.text=element_text(color="black")) +
  theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("NMI Value of 45 Features- Patient Vs Healthy Control")



###########  Male  ##############

setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Male")
df <- read.csv("NMI_Imp_Feats_Male.csv", header = T)
df <- df %>% filter(!row_number() %in% c(1)) #Removing 1st row i.e. APPRDX
df %>% ggplot(aes(reorder(Features, Normalized), Normalized)) + geom_col (aes(fill= Normalized)) + 
  theme(axis.text = element_text(face="bold")) + ylab ("\nNMI Value") + 
  scale_fill_gradient(low="cornflowerblue",high="deeppink") + coord_flip() + labs (x = 'Features') +
  theme(axis.title = element_text(face="bold")) + theme(legend.title = element_text(face = "bold")) +
  theme(text = element_text(size=16)) + theme(axis.text=element_text(color="black")) +
  theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("NMI Value of 45 Features- Patient Vs Healthy Control - Male")


###########  Female  ##############

setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Female")
df <- read.csv("NMI_Imp_Feats_Female.csv", header = T)
df <- df %>% filter(!row_number() %in% c(1)) #Removing 1st row i.e. APPRDX
df %>% ggplot(aes(reorder(Features, Normalized), Normalized)) + geom_col (aes(fill= Normalized)) + 
  theme(axis.text = element_text(face="bold")) + ylab ("\nNMI Value") + 
  scale_fill_gradient(low="cornflowerblue",high="deeppink") + coord_flip() + labs (x = 'Features') +
  theme(axis.title = element_text(face="bold")) + theme(legend.title = element_text(face = "bold")) +
  theme(text = element_text(size=16)) + theme(axis.text=element_text(color="black")) +
  theme(plot.title = element_text(face="bold", size=18)) +
  ggtitle ("NMI Value of 45 Features- Patient Vs Healthy Control - Female")

