###### Male  #######

setwd('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male\\NMI')
data = read.csv('NMI_Common4_TwoAsOne_DifferenceMale.csv')
# Create a column for color based on the Difference value
data$Color <- ifelse(data$Difference < 0, "#F1746B", "#36C3D1")

# Create the vertical bar plot with bars oriented 90 degrees
ggplot(data, aes(x = reorder(Features, Difference), y = Difference, fill = Color)) +
  geom_bar(stat = "identity") + scale_fill_identity() +
  labs(x = "Two Features combined as one", y = "NMI Difference") +
  theme_minimal() + coord_flip() + scale_y_continuous (breaks = seq(-1, 1, by = 0.05)) +
  theme(axis.text.x = element_text(size=16, angle=0, vjust = 0.99, hjust=1, color="black")) +
  theme(axis.text.y = element_text(size=14, color="black")) + guides(fill = FALSE) +
  theme(axis.title=element_text(face='bold')) + theme(plot.title = element_text(face="bold", size=16)) +
  theme(axis.text = element_text(color="black", face="bold")) + theme(text = element_text(size=16)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("Synergistic effects in Male")




######  Female  #######

setwd('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female\\NMI')
data = read.csv('NMI_Common4_TwoAsOne_DifferenceFemale.csv')
# Create a column for color based on the Difference value
data$Color <- ifelse(data$Difference < 0, "#F1746B", "#36C3D1")

# Create the vertical bar plot with bars oriented 90 degrees
ggplot(data, aes(x = reorder(Features, Difference), y = Difference, fill = Color)) +
  geom_bar(stat = "identity") + scale_fill_identity() +
  labs(x = "Two Features combined as one", y = "NMI Difference") +
  theme_minimal() + coord_flip() + scale_y_continuous (breaks = seq(-1, 1, by = 0.05)) +
  theme(axis.text.x = element_text(size=16, angle=0, vjust = 0.99, hjust=1, color="black")) +
  theme(axis.text.y = element_text(size=14, color="black")) + guides(fill = FALSE) +
  theme(axis.title=element_text(face='bold')) + theme(plot.title = element_text(face="bold", size=16)) +
  theme(axis.text = element_text(color="black", face="bold")) + theme(text = element_text(size=16)) +
  theme(panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("Synergistic effects in Female")

