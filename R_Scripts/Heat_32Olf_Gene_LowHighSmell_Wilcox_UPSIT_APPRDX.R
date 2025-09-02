############################################################
# Gene Expression Analysis: UPSIT Low vs High (AROMA Genes)
#
# Aim:
#   - Compare gene expression between Low vs High UPSIT groups
#   - Perform Wilcoxon rank-sum tests (Mann–Whitney U) with FDR correction
#   - Identify significantly altered genes from a predefined gene list (AROMA genes)
#   - Visualize differential expression using boxplots with significance labels
#   - Generate a heatmap of significant AROMA genes across UPSIT groups
#
# Input:
#   - VST_Norm_RNA_Counts_HavingUPSIT_CommonIn4_Gene_wUPSITScore.csv
#   - Predefined gene list (AROMA genes associated with olfaction and PD)
#
# Expected Output:
#   - List of significant AROMA genes with adjusted p-values
#   - Boxplots highlighting expression differences (Low vs High UPSIT)
#   - Heatmap of significant genes with UPSIT group annotation
#
# This script evaluates the role of key AROMA genes in smell loss 
# and visualizes their expression patterns in relation to UPSIT scores.
############################################################









# Set working directory
setwd('S:\\PPMI_RNA\\Processed\\WGCNA_UPSIT')

# Load data
df <- read.csv('VST_Norm_RNA_Counts_HavingUPSIT_CommonIn4_Gene_wUPSITScore.csv')
head(df[1:4, 1:6])
# Filter rows where UPSIT is either below 19 or above 33 as per above categorization criteria
filtered_df <- df[df$UPSIT < 19 | df$UPSIT > 33, ]

# Change the UPSIT values to categorical labels
filtered_df$UPSIT <- ifelse(filtered_df$UPSIT < 19, "Low", "High")

# Dropping columns (keeping gene expression data)
filtered_df <- filtered_df[ , 5:346]
filtered_df <- filtered_df %>% 
  dplyr::select(0, UPSIT, everything())
head(filtered_df[1:4, 1:6])


filtered_df$UPSIT <- factor(filtered_df$UPSIT, levels = c("High", "Low"))
# filtered_df$APPRDX <- ifelse(filtered_df$APPRDX == 1, "Diseased", "Control")
# filtered_df$APPRDX <- factor(filtered_df$APPRDX, levels = c("Control", "Diseased"))


# Reshape data to long format for easier analysis
df_long <- reshape2::melt(filtered_df, id.vars = "UPSIT", variable.name = "gene")

# Calculate p-values using the Wilcoxon rank-sum test (Mann-Whitney U test)
p_values <- filtered_df %>%
  pivot_longer(cols = -UPSIT, names_to = "gene", values_to = "value") %>%
  group_by(gene) %>%
  summarise(p_value = wilcox.test(value ~ UPSIT)$p.value)

# Adjust p-values using the Benjamini-Hochberg method (FDR control)
p_values$p_adj <- p.adjust(p_values$p_value, method = "BH")

# Calculate mean expression per gene for each UPSIT group
mean_expr <- filtered_df %>%
  pivot_longer(cols = -UPSIT, names_to = "gene", values_to = "value") %>%
  group_by(gene, UPSIT) %>%
  summarise(mean_value = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = UPSIT, values_from = mean_value)

# Add a column indicating whether expression is More or Less in High compared to Low
p_values$expression_change <- ifelse(
  mean_expr$High > mean_expr$Low, "More", "Less"
)

# Now save the CSV with the new column
#write.csv(p_values, 'Wilcox_p_values_HighVsLow_All_with_expression_change.csv', row.names = FALSE)

# Filter significant genes based on adjusted p-value (e.g., p-adj < 0.05)
significant_genes <- p_values %>% filter(p_adj < 0.05) %>% pull(gene)

# Define your gene list of interest
gene_list <- c(
  "APLP2", "APAF1", "ATP6V1B2", "BCL6", "BEST1", "CAP1", "CREB5", "DIP2B",
  "GABARAPL1", "HSPA6", "IFNAR1", "IGF2R", "IL1R2", "IL6R", "IRF1", "ITPK1",
  "KAT6A", "MAPK1", "NCOA1", "NCOA2", "NDEL1", "NEDD9", "NDST1", "PIK3CG",
  "PLCG2", "RTN3", "PLXNC1", "PREX1", "RAB3D", "SLC6A6", "STAT3", "STAT5B"
)

# Extract the significant genes that are in your gene list
significant_genes_of_interest <- intersect(significant_genes, gene_list)

# Optional: View the filtered genes
print(significant_genes_of_interest)

# Filter the data to include only significant genes
df_long_significant <- df_long %>% filter(gene %in% significant_genes_of_interest)
df_long_significant$UPSIT <- factor(df_long_significant$UPSIT, levels = c("Low", "High"))

# Plot only the significant genes
ggplot(df_long_significant, aes(x = gene, y = value, color = UPSIT)) +
  geom_boxplot() +
  labs(x = "", y = "Normalized Gene Expression Value", 
       title = "Significant change in Gene expression") +
  scale_color_manual(values = c("#F1746B", "#36C3D1"), name = "UPSIT Score") +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  theme_minimal() +
  theme(
    axis.title = element_text(size = 16, color = 'black'),
    axis.text = element_text(size = 14, color = 'grey40'),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = 'grey40'),
    legend.position = c(1.2, 0.2),
    legend.background = element_rect(fill = "azure", color = "grey40"),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    plot.title = element_text(size = 18)) +
  stat_compare_means(aes(group = UPSIT), label = "p.signif", method = "wilcox.test")












#  -----------------------------------------------------------------------------
# Prepare heatmap data with custom ordering
heatmap_data <- filtered_df %>%
  dplyr::select(UPSIT, all_of(significant_genes_of_interest)) %>%
  # Convert UPSIT to factor with desired order: Low first, then High
  dplyr::mutate(UPSIT = factor(UPSIT, levels = c("Low", "High"))) %>%
  dplyr::arrange(UPSIT)

rownames(heatmap_data) <- paste0("Sample_", 1:nrow(heatmap_data))

# Annotation for UPSIT group
annotation <- data.frame(UPSIT = heatmap_data$UPSIT)
rownames(annotation) <- rownames(heatmap_data)

# Define custom colors for UPSIT groups in the annotation bar
ann_colors <- list(
  UPSIT = c("Low" = "#F1746B",  # example: red
            "High" = "#36C3D1") # example: blue
)

# Prepare gene expression matrix
heatmap_matrix <- as.matrix(heatmap_data[, -1])
heatmap_matrix <- t(heatmap_matrix)

# Draw heatmap
pheatmap::pheatmap(
  heatmap_matrix,
  annotation_col = annotation,
  annotation_colors = ann_colors,
  cluster_cols = FALSE,
  cluster_rows = FALSE,
  scale = "row",
  color = colorRampPalette(c("#7D2D4E", "#FFFAE1", "#3A734B"))(100),
  show_rownames = TRUE,
  show_colnames = FALSE,
  fontsize_row = 10,
  main = "Heatmap: Significant Genes"
)
'#9B4C7A'

'#8C3C64'

'#7D2D4E'

'#6F1F3A'

'#601833'


'#5A9C6D'

'#4A875C'

'#3A734B'

'#29613A'

'#19502A'