## Male  -----------------------------------------------------------------------
setwd('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male')

df <- read.csv('NMI_All45Feats_Normalized_Male.csv')
df$Feature <- factor(df$Feature, levels = df$Feature)

ggplot(df, aes(x = Feature, y = Normalized)) +
  geom_line(aes(group = 1)) + # Connect points with lines
  theme_minimal() +
  geom_point() +   scale_y_continuous(breaks = seq(0, 0.33, by = 0.05)) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1, face = "bold", colour = 'black'), # Rotate x-axis labels and make them bold
        axis.text.y = element_text(size = 12, face = "bold", colour = 'black'), # Make y-axis labels bold
        axis.title = element_text(size = 14, face = "bold", colour = 'black'), # Make axis titles bold
        plot.title = element_text(size = 16, face = "bold", colour = 'black'), # Make plot title bold
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) + # Add border to the plot
  labs(x = "Feature", y = "NMI Value\n", title = "Male: NMI values of all the 45 features")


##  Female  --------------------------------------------------------------------
setwd('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female')

df <- read.csv('NMI_All45Feats_Normalized_Female.csv')
df$Feature <- factor(df$Feature, levels = df$Feature)

ggplot(df, aes(x = Feature, y = Normalized)) +
  geom_line(aes(group = 1)) + # Connect points with lines
  theme_minimal() +
  geom_point() +   scale_y_continuous(breaks = seq(0, 0.33, by = 0.05)) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1, face = "bold", colour = 'black'), # Rotate x-axis labels and make them bold
        axis.text.y = element_text(size = 12, face = "bold", colour = 'black'), # Make y-axis labels bold
        axis.title = element_text(size = 14, face = "bold", colour = 'black'), # Make axis titles bold
        plot.title = element_text(size = 16, face = "bold", colour = 'black'), # Make plot title bold
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) + # Add border to the plot
  labs(x = "Feature", y = "NMI Value\n", title = "Female: NMI values of all the 45 features")


##  Combined  ------------------------------------------------------------------
setwd('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Files')

df <- read.csv('NMI_All45Feats_Normalized.csv')
df$Feature <- factor(df$Feature, levels = df$Feature)

ggplot(df, aes(x = Feature, y = Normalized)) +
  geom_line(aes(group = 1)) + # Connect points with lines
  theme_minimal() +
  geom_point() +   scale_y_continuous(breaks = seq(0, 0.33, by = 0.05)) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1, face = "bold", colour = 'black'), # Rotate x-axis labels and make them bold
        axis.text.y = element_text(size = 12, face = "bold", colour = 'black'), # Make y-axis labels bold
        axis.title = element_text(size = 14, face = "bold", colour = 'black'), # Make axis titles bold
        plot.title = element_text(size = 16, face = "bold", colour = 'black'), # Make plot title bold
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) + # Add border to the plot
  labs(x = "Feature", y = "NMI Value\n", title = "Combined: NMI values of all the 45 features")
