############################################################
# Visualization of Enriched Pathways (Bubble Plot)
#
# Aim:
#   - Visualize enriched pathways using a bubble plot
#   - Show relationship between gene count and statistical significance
#   - Highlight pathways with larger gene sets and lower p-values
#
# Input:
#   - Selected_Pathways_GeneCount.csv 
#       (columns: Pathway, Gene_Count, p_val)
#
# Expected Output:
#   - Bubble plot showing enriched pathways (ggplot2 output)
#     • X-axis: Gene count
#     • Y-axis: Pathway name (reversed order for readability)
#     • Bubble size: Gene count
#     • Bubble color: p-value (log scale gradient)
#
# This script generates a publication-ready visualization
# of enriched pathways, emphasizing pathway size and significance.
############################################################









# Set working directory
setwd('S:\\PPMI_RNA\\Processed\\WGCNA_UPSIT')

# Load data
df <- read.csv('Selected_Pathways_GeneCount.csv', encoding = "UTF-8-BOM")
df <- df[, 1:3]

colnames(df) <- c("Pathway", "Gene_Count", "p_val")


ggplot(df, aes(x = Gene_Count, 
               y = Pathway, 
               size = Gene_Count, 
               color = p_val)) +
  geom_point() +
  labs(
    x = "Gene Count",
    y = "Key Pathway",
    title = "",
    color = "p-value",
    size = "Gene Count"
  ) +
  scale_x_continuous(breaks = seq(0, 80, by = 15)) +
  scale_y_discrete(limits = rev) +  # Reverse the y-axis order
  scale_color_gradient(low = "#36C3D1", high = "#F1746B", trans = "log10") +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18, color = 'black'),
    axis.title = element_text(size = 18, color = 'black'),
    axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1, color = 'grey40'),
    legend.position = "right",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    plot.title = element_text(size = 18)
  )
