# Methylation Data: Age of healthy
ages <- c(69.1,59.4,81.9,83.7,31.9,79.3,57.5,66.3,60,60.4,64.5,71.5,47.2,48.1,31.2,75.8,56.9,56.4,72,70.2,52.6,62.6,63.9,65,64.1,44.8,61.1,59.8,70,62.3,70.7,72.6,69.6,69.8,64,69.9,67.8,66.5,69.6,51.4,67.1,70.5,60.8,81.3,69,65.7,47.8,56.8,77.3,70.8,60.4,54.6,69.1,32.3,44,67,41.6,64.1,61.3,45.2,53.2,60.8,70.6,39.2,61.4,57.5,81.8,51,66.9,57,61.3,48.3,76.9,66.5,71.1,45.5,72.9,62,59.1,64.7,61.8,42.3,70.3,55.4,64.3,60.3,49.4,65.7,45.8,57.7,61,69.2,49.2,62.9,52.5,49.2,69,43.1,64.2,66,77.3,52.4,76,71.7,65.8,55.2,68.1,61.9,71.3,78.8,43.2,53.9,63.6,69,58.4,82.7,66.5,72.5,65.3,63.7,63.9,47.5,49.2,75.1,68,72.6,73.5,66.6,72,67.2,68.1,60.3,71.1)

# Define age ranges
age_ranges <- cut(ages, breaks = c(30, 40, 50, 60, 70, 80, 90), labels = c("30-40", "40-50", "50-60", "60-70", "70-80", "80-90"))

# Create a table of frequencies
age_table <- table(age_ranges)
age_df <- as.data.frame(age_table)
age_df$Age_Range <- as.character(age_df$age_ranges)

# Define a function to generate a gradient color palette
gradient_palette <- colorRampPalette(c("#36C3D1","#F1746B"))
# Generate gradient colors
gradient_colors <- gradient_palette(length(age_table))

# Create ggplot
ggplot(age_df, aes(x = Age_Range, y = Freq, fill = Age_Range)) +
  geom_bar(stat = "identity") +
  labs(title = "Methylation data: Age distribution of healthy", x = "\nAge Range (in years)", y = "Frequency\n", fill = "Age Range") +
  scale_fill_manual(values = gradient_colors, name = "Age Range") +
  theme_bw() + 
  theme(
    axis.text = element_text(face = "bold", size = 14, color = 'black'),
    axis.title = element_text(face = "bold", size = 16, color = 'black'),
    axis.text.x = element_text(color = 'black'),
    legend.position = "none",
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_text(face = "bold", size = 18)
  ) +
  geom_text(aes(label = Freq), vjust = -0.5, size = 4) +
  scale_y_continuous(breaks = seq(0, max(age_df$Freq), by = 10)) +
  geom_hline(yintercept = seq(0, max(age_df$Freq), by = 50), linetype = "dotted", color = "gray", size = 0.2)

