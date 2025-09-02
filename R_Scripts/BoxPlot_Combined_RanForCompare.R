############################################################
# Feature Set & Model Comparison Script
#
# Aim:
#   - Compare model accuracy across different feature sets 
#     (all features, decision tree, random forest, Boruta, NMI, etc.)
#   - Compare performance of various machine learning models
#     (with Random Forest emphasized as reference)
#   - Highlight and evaluate the top 9 important non-motor features
#     with a focus on UPSIT
#   - Visualize differences using boxplots with custom color schemes
#
# Input:
#   - NonMot_RanFor_Compare.csv (feature set accuracy results)
#   - Various_ML_Model_Compare.csv (ML model accuracy results)
#   - NonMot_Indi_9Feats_Combined_100svmL.csv (accuracy of top 9 features)
#
# Expected Output:
#   - Box_NonMot_RanFor_Compare.svg (feature set comparison plot)
#   - Various_ML_Model_Compare.svg (ML model comparison plot)
#   - Box_ImpFeats.svg (important features with UPSIT emphasis)
#
# This script produces boxplots comparing feature sets, machine learning 
# models, and top non-motor features, highlighting the importance of UPSIT.
############################################################









##  1. Feature wise compare  ---------------------------------------------------
setwd('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\RanFor')
Non_Mot = read.csv("NonMot_RanFor_Compare.csv", header = TRUE)
Non_Mot = Non_Mot[, -6]
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')

# Update Feature names
Non_Mot_Long$Feature <- recode(Non_Mot_Long$Feature,
                               "Common9_in4Methods" = "In_Any_2",
                               "Random_9Features" = "Random_9_Features"
)

# Define custom colors
custom_colors <- c(
  "All_Features" = "#F1746B",
  "Decision_Tree" = "#36C3D1",
  "Random_Forest" = "#F2CC8F",
  "Boruta" = "#82CDA9",
  "NMI" = "#577399",
  "In_Any_2" = "#B15E9C",
  "Random_9_Features" = "#DCD6F7"
)

# Feature order
feature_order <- c("All_Features", "Decision_Tree", "Random_Forest", "Boruta", "NMI",
                   "In_Any_2", "Random_9_Features")

# Custom axis labels with color styling
axis_labels <- c(
  "All_Features" = "<span style='color:#666666;'>All_Features</span>",
  "Decision_Tree" = "<span style='color:#666666;'>Decision_Tree</span>",
  "Random_Forest" = "<span style='color:#666666;'>Random_Forest</span>",
  "Boruta" = "<span style='color:#666666;'>Boruta</span>",
  "NMI" = "<span style='color:#666666;'>NMI</span>",
  "In_Any_2" = "<span style='color:black;'>In_Any_2</span>",
  "Random_9_Features" = "<span style='color:#666666;'>Random_9_Features</span>"
)

p1 <- ggplot(Non_Mot_Long, aes(x = factor(Feature, level = feature_order), 
                               y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot() + 
  scale_fill_manual(values = custom_colors) +
  theme_bw() +
  theme(legend.position = "none") + 
  scale_x_discrete(labels = axis_labels) +
  theme(
    axis.text.x = ggtext::element_markdown(size = 24, angle = 90, vjust = 0.4, hjust = 1),
    axis.text.y = element_text(color = "grey40", size = 24),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 24),
    text = element_text(size = 24),
    plot.title = element_text(size = 20),
    panel.border = element_rect(colour = "grey", fill = NA, size = 1)
  ) +
  ylab("Model Accuracy") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  ggtitle("Various set of Features")



p1


########### Various Model ############

setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Various_Models")
Non_Mot <- read.csv('Various_ML_Model_Compare.csv', fileEncoding='latin1', check.names=FALSE)
Non_Mot_Long <- gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')

# Reorder the factor levels based on median accuracy
Non_Mot_Long$Feature <- reorder(Non_Mot_Long$Feature, Non_Mot_Long$Model_Accuracy)
ordered_levels <- levels(Non_Mot_Long$Feature)

# Define colors for axis labels: Random_Forest in black, rest in grey40
axis_labels <- setNames(
  sapply(ordered_levels, function(x) {
    if (x == "Random_Forest") {
      sprintf("<span style='color:black;'>%s</span>", x)
    } else {
      sprintf("<span style='color:grey40;'>%s</span>", x)
    }
  }),
  ordered_levels
)

# Define your original fill colors as needed
my_colors <- c(
  "#F1746B", "#36C3D1", "#F2CC8F", "#82CDA9", "#577399", "#DCD6F7",
  "#B15E9C", "#E3A587", "#D4A5C2", "#A5C8E1", "#F9D56E", "#6AC47E",
  "#B07AA1", "#FF9DA7", "#A3D5A6", "#5D9BD3", "#DDA0DD", "#FDBE72",
  "#92A8D1", "#C4DFAA"
)
named_colors <- setNames(my_colors[1:length(ordered_levels)], ordered_levels)

# Plot
p2 <- ggplot(Non_Mot_Long, aes(x = Feature, y = Model_Accuracy, fill = Feature)) + 
  geom_boxplot() + 
  scale_fill_manual(values = named_colors) +
  scale_x_discrete(labels = axis_labels) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_markdown(size = 24, angle = 90, vjust = 0.4, hjust = 1),
    axis.text.y = element_text(color = "grey40", size = 24),
    axis.title.x = element_blank(),
    axis.title.y = element_text(size = 24),
    text = element_text(size = 24),
    plot.title = element_text(size = 20),
    panel.border = element_rect(colour = "grey", fill = NA, size = 1)
  ) +
  ylab("Model Accuracy") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1)) +
  ggtitle("Various ML Model (Uncategorized)")

print(p2)



#
library(cowplot)

# Align both the horizontal and vertical margins
aligned <- align_plots(p1, p2, align = "hv", axis = "tblr")

# Save the aligned grobs; the panels are now identically sized
ggsave("Box_NonMot_RanFor_Compare.svg", plot = ggdraw(aligned[[1]]), width = 4.2, height = 6, dpi = 300)
ggsave("Various_ML_Model_Compare.svg", plot = ggdraw(aligned[[2]]), width = 10.2, height = 6, dpi = 300)








#  Important 9 Features  -------------------------------------------------------

setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Files")
Non_Mot = read.csv("NonMot_Indi_9Feats_Combined_100svmL.csv", header = TRUE)

# Reshape to long format
Non_Mot_Long = gather(Non_Mot, key = 'Feature', value = 'Model_Accuracy')
Non_Mot_Long$Feature <- factor(Non_Mot_Long$Feature, levels = unique(Non_Mot_Long$Feature))

# Custom color palette
# custom_colors <- c(
#   "#F1746B", "#36C3D1", "#F2CC8F", "#82CDA9", "#577399", 
#   "#DCD6F7", "#B15E9C", "#E3A587", "#D4A5C2")

# Focus will be on UPSIT
custom_colors <- c(
  "#E3EAF0", "#E3EAF0", "#E3EAF0", "#E3EAF0", "#E3EAF0", 
  "#E3EAF0", "#E3EAF0", "#E3EAF0", "#36C3D1")


label_colors <- ifelse(levels(Non_Mot_Long$Feature) == "UPSIT", "black", "grey40")

p1 <- ggplot(Non_Mot_Long, aes(x = Feature, y = Model_Accuracy, fill = Feature)) +
  geom_boxplot() +
  scale_fill_manual(values = custom_colors) +
  theme_bw() +
  theme(
    legend.position = "none",
    axis.text.x = element_text(
      size = 24, angle = 90, vjust = 0.4, hjust = 1,
      colour = label_colors    # this is the key line
    ),
    axis.title.x = element_blank(),
    axis.text = element_text(size = 24), 
    text = element_text(size = 24),
    plot.title = element_text(size = 20),
    panel.border = element_rect(colour = "grey", fill = NA, size = 1)
  ) +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  ylab("Model Accuracy") +
  ggtitle("SVM Linear: Important Features")

# Align both the horizontal and vertical margins
aligned <- align_plots(p1, align = "hv", axis = "tblr")
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Plots")
ggsave("Box_ImpFeats.svg", plot = ggdraw(aligned[[1]]), width = 5.4, height = 6, dpi = 300)
