
## Patient  --------------------------------------------------------------------

data <- data.frame(
  Name = c("Methylation Data", "Total Participants", "Answered All"),
  HYS_0 = c(69, 0, 0),
  HYS_1 = c(73, 185, 82),
  HYS_2 = c(167, 236, 83),
  HYS_3 = c(13, 2, 0),
  HYS_4 = c(3, 0, 0))

# Reshape the data to long format for plotting
data_long <- tidyr::pivot_longer(data, cols = c(HYS_0, HYS_1, HYS_2, HYS_3, HYS_4), names_to = "HYS", values_to = "Value")

# Plot the stacked bar plot
ggplot(data_long, aes(x = Name, y = Value, fill = HYS)) +
  geom_bar(stat = "identity") +
  labs(title = "HYS-wise distribution of Patient", x = "", y = "Total number of participants\n", fill = "HYS") +
  scale_fill_manual(values = c("#F2CC8F", "#F1746B", "#36C3D1", "#DCD6F7", "#82CDA9"), name = "HYS") + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(0, 440, by = 40)) +
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
  geom_hline(yintercept = seq(0, 440, by = 40), linetype = "dotted", color = "gray", linewidth = 0.2) +
  geom_text(aes(label = Value, y = Value), size = 5, position = position_stack(vjust = 0.5))





## Healthy  --------------------------------------------------------------------

data <- data.frame(
  Name = c("Methylation Data", "Total Participants", "Answered All"),
  HYS_0 = c(57, 193, 86),
  HYS_1 = c(21, 2, 0),
  HYS_2 = c(53, 0, 0),
  HYS_3 = c(2, 0, 0),
  HYS_4 = c(0, 0, 0))

data_long <- tidyr::pivot_longer(data, cols = c(HYS_0, HYS_1, HYS_2, HYS_3, HYS_4), names_to = "HYS", values_to = "Value")

ggplot(data_long, aes(x = Name, y = Value, fill = HYS)) +
  geom_bar(stat = "identity") +
  labs(title = "HYS-wise distribution of Healthy", x = "", y = "Total number of participants\n", fill = "HYS") +
  scale_fill_manual(values = c("#F2CC8F", "#F1746B", "#36C3D1", "#DCD6F7", "#82CDA9"), name = "HYS") + 
  theme_bw() + 
  scale_y_continuous(breaks = seq(0, 200, by = 20)) +
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
  geom_hline(yintercept = seq(0, 200, by = 20), linetype = "dotted", color = "gray", linewidth = 0.2) +
  geom_text(aes(label = Value, y = Value), size = 5, position = position_stack(vjust = 0.5))
