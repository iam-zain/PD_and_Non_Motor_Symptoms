## We have taken 0.6 Log2FoldChange as 1.0 Log2FoldChange was returning very less up or downregulated genes
 # Female  ---------------------------------------------------------------------
setwd('S:\\PPMI_RNA\\Processed\\Female')
df <- read.csv('DEGs_Result_Female.csv', header = TRUE, sep = ",")

df <- df %>%
  mutate(Description = case_when(
    !is.na(pvalue) & padj < 0.05 & log2FoldChange > 0.6 ~ "Upregulated",
    !is.na(pvalue) & padj < 0.05 & log2FoldChange < -0.6 ~ "Downregulated",
    (padj < 0.05 & log2FoldChange >= -0.6 & log2FoldChange <= 0.6) | 
      (is.na(padj) & (log2FoldChange < -0.6 | log2FoldChange > 0.6)) ~ "Tentative",
    padj > 0.05 & (log2FoldChange < -0.6 | log2FoldChange > 0.6) ~ "Baseline",
    TRUE ~ "Non-Significant"
  ))
write.csv(df, 'DEGs_Result_Female_Description.csv', row.names = F)


 # Male  -----------------------------------------------------------------------
setwd('S:\\PPMI_RNA\\Processed\\Male')
df <- read.csv('DEGs_Result_Male.csv', header = TRUE, sep = ",")

df <- df %>%
  mutate(Description = case_when(
    !is.na(pvalue) & padj < 0.05 & log2FoldChange > 0.6 ~ "Upregulated",
    !is.na(pvalue) & padj < 0.05 & log2FoldChange < -0.6 ~ "Downregulated",
    (padj < 0.05 & log2FoldChange >= -0.6 & log2FoldChange <= 0.6) | 
      (is.na(padj) & (log2FoldChange < -0.6 | log2FoldChange > 0.6)) ~ "Tentative",
    padj > 0.05 & (log2FoldChange < -0.6 | log2FoldChange > 0.6) ~ "Baseline",
    TRUE ~ "Non-Significant"
  ))
write.csv(df, 'DEGs_Result_Male_Description.csv', row.names = F)


 # Combined  -------------------------------------------------------------------
setwd('S:\\PPMI_RNA\\Processed')
df <- read.csv('DEGs_Result.csv', header = TRUE, sep = ",")

df <- df %>%
  mutate(Description = case_when(
    !is.na(pvalue) & padj < 0.05 & log2FoldChange > 0.6 ~ "Upregulated",
    !is.na(pvalue) & padj < 0.05 & log2FoldChange < -0.6 ~ "Downregulated",
    (padj < 0.05 & log2FoldChange >= -0.6 & log2FoldChange <= 0.6) | 
      (is.na(padj) & (log2FoldChange < -0.6 | log2FoldChange > 0.6)) ~ "Tentative",
    padj > 0.05 & (log2FoldChange < -0.6 | log2FoldChange > 0.6) ~ "Baseline",
    TRUE ~ "Non-Significant"
  ))
write.csv(df, 'DEGs_Result_Description.csv', row.names = F)

