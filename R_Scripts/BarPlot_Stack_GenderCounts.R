
data <- data.frame(
  Name = c("Methylation Data", "Total Participants", "Answered All"),
  Male = c(308, 403, 167),
  Female = c(150, 216, 84))

# Reshape the data to long format for plotting
data_long <- tidyr::pivot_longer(data, cols = c(Male, Female), names_to = "Gender", values_to = "Value")

# Plot the stacked bar plot
ggplot(data_long, aes(x = Name, y = Value, fill = Gender)) +
  geom_bar(stat = "identity") +
  labs(title = "Gender-wise distribution of participants", x = "", y = "Total number of participants\n", fill = "Gender") +
  scale_fill_manual(values = c("#36C3D1","#F1746B"), name = "Gender") + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(0, 700, by = 50)) +
  theme(
    axis.text = element_text(face = "bold", size = 14, color = 'black'),
    axis.title = element_text(face = "bold", size = 16, color = 'black'),
    axis.text.x = element_text(color = 'black'),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(fill = "azure", color = "black"),
    legend.title = element_text(face = "bold", size = 14),
    panel.grid.major = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_text(face = "bold", size = 18)) +
  geom_hline(yintercept = seq(0, 700, by = 50), linetype = "dotted", color = "gray", size = 0.2) +
  geom_text(aes(label = Value, y = Value), size = 5, position = position_stack(vjust = 0.5))
