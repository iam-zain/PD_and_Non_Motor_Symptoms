############################################################
# CpG Methylation Analysis: UPSIT Low vs High
#
# Aim:
#   - Compare methylation values of CpG sites between Low vs High UPSIT groups
#   - Perform Wilcoxon rank-sum tests (Mann–Whitney U) for each CpG
#   - Adjust p-values using Benjamini–Hochberg (FDR) correction
#   - Identify direction of methylation change (More vs Less in Low compared to High)
#   - Visualize significant CpGs with boxplots
#
# Input:
#   - Signif318_CpG_UPSIT_Data.csv (CpG methylation data with UPSIT scores)
#
# Expected Output:
#   - CpG_Signif_Results_with_Info.csv (significant CpGs with adjusted p-values and direction)
#   - Boxplots of significant CpG methylation differences (UPSIT Low vs High)
#   - Publication-ready figures for CpG methylation analysis
#
# This script identifies CpG sites significantly associated 
# with smell loss categories and highlights their direction of change.
############################################################









# Required libraries
library(dplyr)
library(reshape2)
library(tidyr)
library(ggplot2)
library(ggpubr)

setwd('S:\\PPMI_RNA\\Processed\\WGCNA_UPSIT')

df <- read.csv('Signif_CpG_UPSIT_Data.csv')
df <- df[, -c(1, 2, 3, 5, 6, 8, 9)]
head(df[1:4, 1:4])

# Filter and categorize UPSIT scores
df_filter <- df[df$UPSIT_Total < 19 | df$UPSIT_Total > 33, ]
df_filter$UPSIT_Total <- ifelse(df_filter$UPSIT_Total < 19, "Low", "High")

# Melt data to long format
df_long <- reshape2::melt(df_filter, id.vars = "UPSIT_Total", variable.name = "CpG")

# Calculate Wilcoxon p-values and adjust
p_values <- df_long %>%
  group_by(CpG) %>%
  summarise(p_value = wilcox.test(value ~ UPSIT_Total)$p.value)

# Adjust p-values using Benjamini-Hochberg method
p_values$p_adj <- p.adjust(p_values$p_value, method = "BH")

# Filter for significant CpGs
significant_CpGs <- p_values %>% filter(p_adj < 0.05) %>% pull(CpG)

# Filter long data for significant CpGs
df_long_significant <- df_long %>% filter(CpG %in% significant_CpGs)
df_long_significant$UPSIT_Total <- factor(df_long_significant$UPSIT_Total, levels = c("Low", "High"))

# Create pairwise comparisons
comparisons <- list(c("Low", "High"))

# Plot
ggplot(df_long_significant, aes(x = CpG, y = value, color = UPSIT_Total)) +
  geom_boxplot() +
  labs(x = "\nCpG", y = "Normalized CpG Expression Value", 
       title = "Significant change in CpG expression") +
  scale_color_manual(values = c("#F1746B", "#36C3D1"), name = "UPSIT_Total Score") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.2)) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 18, color = 'grey40'),
    axis.title = element_text(size = 18, color = 'black'),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 0.5, color = 'grey40'),
    legend.position = c(1.2, 0.2),
    legend.background = element_rect(fill = "azure", color = "grey40"),
    legend.title = element_text(size = 18),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    plot.title = element_text(size = 18))
  #stat_compare_means(aes(group = UPSIT_Total), label = "p.signif", method = "wilcox.test")



# Add adjusted p-value
p_values <- p_values %>%
  mutate(p_adj = p.adjust(p_value, method = "BH"))

# Step 2: Compute mean values by group and direction
direction_df <- df_long %>%
  group_by(CpG, UPSIT_Total) %>%
  summarise(mean_value = mean(value), .groups = "drop") %>%
  pivot_wider(names_from = UPSIT_Total, values_from = mean_value) %>%
  mutate(Direction = ifelse(Low > High, "Less", "More"))

# Step 3: Merge and select
results_df <- left_join(p_values, direction_df, by = "CpG") %>%
  dplyr::select(CpG, p_adj, Direction)

# Step 4: Save to CSV
write.csv(results_df, "CpG_Signif_Results_with_Info.csv", row.names = FALSE)
