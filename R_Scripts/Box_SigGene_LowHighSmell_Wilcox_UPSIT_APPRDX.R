############################################################
# Wilcoxon Gene Expression Analysis: UPSIT Low vs High
#
# Aim:
#   - Compare gene expression between Low vs High UPSIT score groups
#   - Perform Wilcoxon rank-sum tests (Mann–Whitney U) across all samples,
#     and separately within healthy controls and PD patients
#   - Adjust p-values using Benjamini–Hochberg (FDR) correction
#   - Identify genes with significant expression changes (More/Less in High vs Low)
#   - Visualize differential expression patterns with boxplots
#   - Focus also on common DEGs overlapping with WGCNA modules
#
# Input:
#   - VST_Norm_RNA_Counts_HavingUPSIT_CommonIn4_Gene_wUPSITScore.csv
#   - Common_Gene_DEGs_UPSIT_and_Diagnosis_pVal_And_Module.csv
#
# Expected Output:
#   - Wilcox_p_values_HighVsLow_All_with_expression_change.csv
#   - Wilcox_p_values_HighVsLow_Healthy_with_expression_change.csv
#   - Wilcox_p_values_HighVsLow_Diseased_with_expression_change.csv
#   - Boxplots of significant genes (UPSIT Low vs High) across groups
#   - Expression plots restricted to overlapping DEGs with WGCNA modules
#
# This script identifies differentially expressed genes linked to smell loss,
# stratifies them by health status, and highlights overlap with WGCNA findings.
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
write.csv(p_values, 'Wilcox_p_values_HighVsLow_All_with_expression_change.csv', row.names = FALSE)

# Filter significant genes based on adjusted p-value (e.g., p-adj < 0.05)
significant_genes <- p_values %>% filter(p_adj < 0.05) %>% pull(gene)

# Filter the data to include only significant genes
df_long_significant <- df_long %>% filter(gene %in% significant_genes)
df_long_significant$UPSIT <- factor(df_long_significant$UPSIT, levels = c("Low", "High"))

# Plot only the significant genes
ggplot(df_long_significant, aes(x = gene, y = value, color = UPSIT)) +
  geom_boxplot() +
  labs(x = "", y = "Normalized Gene Expression Value\n", 
       title = "Low vs High UPSIT Score: Significant change in Gene expression") +
  scale_color_manual(values = c("#F1746B", "#36C3D1"), name = "UPSIT Score") +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 14, color = 'black'),
    axis.title = element_text(size = 16, color = 'grey40'),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, color = 'black'),
    legend.position = c(1.2, 0.2),
    legend.background = element_rect(fill = "azure", color = "grey40"),
    legend.title = element_text(size = 14),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    plot.title = element_text(size = 18)) +
  stat_compare_means(aes(group = UPSIT), label = "p.signif", method = "wilcox.test")








# Only Healthy  ----------------------------------------------------------------
# Filter rows where UPSIT is either below 19 or above 33 as per above categorization criteria
df <- read.csv('VST_Norm_RNA_Counts_HavingUPSIT_CommonIn4_Gene_wUPSITScore.csv')
df_h <- df[df$APPRDX == 2, ]
filtered_df <- df_h[df_h$UPSIT < 19 | df_h$UPSIT > 33, ]
# Change the UPSIT values to categorical labels
filtered_df$UPSIT <- ifelse(filtered_df$UPSIT < 19, "Low", "High")

# Dropping columns (keeping gene expression data)
filtered_df <- filtered_df[ , 5:346]
filtered_df <- filtered_df %>% 
  dplyr::select(0, UPSIT, everything())
head(filtered_df[1:4, 1:6])

table(filtered_df$UPSIT)
filtered_df$UPSIT <- factor(filtered_df$UPSIT, levels = c("High", "Low"))

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
p_values$Expr_Change_wrt_Low <- ifelse(
  mean_expr$High > mean_expr$Low, "More", "Less"
)

# Now save the CSV with the new column
write.csv(p_values, 'Wilcox_p_values_HighVsLow_Healthy_with_expression_change.csv', row.names = FALSE)







# Only Diseased  ---------------------------------------------------------------
# Filter rows where UPSIT is either below 19 or above 33 as per above categorization criteria
df <- read.csv('VST_Norm_RNA_Counts_HavingUPSIT_CommonIn4_Gene_wUPSITScore.csv')
df_h <- df[df$APPRDX == 1, ]
filtered_df <- df_h[df_h$UPSIT < 19 | df_h$UPSIT > 33, ]
# Change the UPSIT values to categorical labels
filtered_df$UPSIT <- ifelse(filtered_df$UPSIT < 19, "Low", "High")

# Dropping columns (keeping gene expression data)
filtered_df <- filtered_df[ , 5:346]
filtered_df <- filtered_df %>% 
  dplyr::select(0, UPSIT, everything())
head(filtered_df[1:4, 1:6])

table(filtered_df$UPSIT)
# High 35, Low 112

filtered_df$UPSIT <- factor(filtered_df$UPSIT, levels = c("High", "Low"))

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
p_values$Expr_Change_wrt_Low <- ifelse(
  mean_expr$High > mean_expr$Low, "More", "Less"
)

# Now save the CSV with the new column
write.csv(p_values, 'Wilcox_p_values_HighVsLow_Diseased_with_expression_change.csv', row.names = FALSE)















# Common in DEGs in all 4 of UPSIT and WGCNA: pVal and Modules

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

com_gene <- read.csv('Common_Gene_DEGs_UPSIT_and_Diagnosis_pVal_And_Module.csv')
filtered_df <- filtered_df[, (colnames(filtered_df) %in% com_gene$Gene) | colnames(filtered_df) == "UPSIT"]

# Reshape data to long format for easier analysis
df_long <- reshape2::melt(filtered_df, id.vars = "UPSIT", variable.name = "gene")
df_long$UPSIT <- factor(df_long$UPSIT, levels = c("Low", "High"))

# Plot only the significant genes
ggplot(df_long, aes(x = gene, y = value, color = UPSIT)) +
  geom_boxplot() +
  labs(x = "", y = "Normalized Gene Expression Value\n", 
       title = "") +
  scale_color_manual(values = c("#F1746B", "#36C3D1"), name = "UPSIT Score") +
  scale_y_continuous(breaks = seq(0, 18, by = 2)) +
  theme_minimal() +
  theme(
    axis.text = element_text(face = "bold", size = 18, color = 'black'),
    axis.title = element_text(face = "bold", size = 18, color = 'black'),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, color = 'black'),
    legend.position = c(1.2, 0.2),
    legend.background = element_rect(fill = "azure", color = "black"),
    legend.title = element_text(face = "bold", size = 14),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_text(face = "bold", size = 18)) +
  stat_compare_means(aes(group = UPSIT), label = "p.signif", method = "wilcox.test")
