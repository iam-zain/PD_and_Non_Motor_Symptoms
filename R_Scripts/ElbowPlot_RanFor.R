## Male  -----------------------------------------------------------------------
setwd('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male')

df <- read.csv('ImpFeats_RanFor_Male.csv')
df$Feature <- factor(df$Feature, levels = df$Feature)

ggplot(df, aes(x = Feature, y = Overall)) +
  geom_line(aes(group = 1)) + # Connect points with lines
  theme_minimal() +
  geom_point() +   scale_y_continuous(breaks = seq(0, 110, by = 10)) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1, face = "bold", colour = 'black'), # Rotate x-axis labels and make them bold
        axis.text.y = element_text(size = 12, face = "bold", colour = 'black'), # Make y-axis labels bold
        axis.title = element_text(size = 14, face = "bold", colour = 'black'), # Make axis titles bold
        plot.title = element_text(size = 16, face = "bold", colour = 'black'), # Make plot title bold
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) + # Add border to the plot
  labs(x = "Feature", y = "Value\n", title = "Male: Random Forest values of all the 45 features")


##  Female  --------------------------------------------------------------------
setwd('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Female')

df <- read.csv('ImpFeats_RanFor_Female.csv')
df$Feature <- factor(df$Feature, levels = df$Feature)

ggplot(df, aes(x = Feature, y = Overall)) +
  geom_line(aes(group = 1)) + # Connect points with lines
  theme_minimal() +
  geom_point() +   scale_y_continuous(breaks = seq(0, 110, by = 10)) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1, face = "bold", colour = 'black'), # Rotate x-axis labels and make them bold
        axis.text.y = element_text(size = 12, face = "bold", colour = 'black'), # Make y-axis labels bold
        axis.title = element_text(size = 14, face = "bold", colour = 'black'), # Make axis titles bold
        plot.title = element_text(size = 16, face = "bold", colour = 'black'), # Make plot title bold
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) + # Add border to the plot
  labs(x = "Feature", y = "Value\n", title = "Female: Random Forest values of all the 45 features")


##  Combined  ------------------------------------------------------------------
setwd('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Files')

df <- read.csv('ImpFeats_RanFor.csv')
df$Feature <- factor(df$Feature, levels = df$Feature)

ggplot(df, aes(x = Feature, y = Overall)) +
  geom_line(aes(group = 1)) + # Connect points with lines
  theme_minimal() +
  geom_point() +   scale_y_continuous(breaks = seq(0, 110, by = 10)) +
  theme(axis.text.x = element_text(size = 12, angle = 90, hjust = 1, face = "bold", colour = 'black'), # Rotate x-axis labels and make them bold
        axis.text.y = element_text(size = 12, face = "bold", colour = 'black'), # Make y-axis labels bold
        axis.title = element_text(size = 14, face = "bold", colour = 'black'), # Make axis titles bold
        plot.title = element_text(size = 16, face = "bold", colour = 'black'), # Make plot title bold
        panel.border = element_rect(colour = "black", fill = NA, linewidth = 1)) + # Add border to the plot
  labs(x = "Feature", y = "Value\n", title = "Combined: Random Forest values of all the 45 features")
