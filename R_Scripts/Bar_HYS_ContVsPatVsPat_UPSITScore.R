############################################################
# UPSIT Distribution and Visualization Script
#
# Aim:
#   - Preprocess RNA-seq + clinical dataset (filtering by HYS and diagnosis)
#   - Categorize UPSIT smell scores into Low vs High groups
#   - Compare UPSIT distribution across APPRDX_HYS groups (Control_0, Patient_1, Patient_2)
#   - Perform statistical tests (Chi-square or Fisher’s exact) on group differences
#   - Visualize proportions with annotated significance (asterisks)
#
# Input:
#   - VST_Norm_RNA_Counts_HavingUPSIT_CommonIn4_Gene_wUPSITScore_HYS.csv
#
# Expected Output:
#   - Proportional barplots of UPSIT (Low vs High) by APPRDX_HYS group
#   - p-value significance label (Chi-square/Fisher test) annotated on the plot
#   - Publication-ready ggplot figures showing distribution patterns
#
# This script generates group-wise UPSIT proportion plots with 
# statistical significance testing for smell loss distribution.
############################################################









setwd('S:\\PPMI_RNA\\Processed\\WGCNA_UPSIT')

df <- read.csv('VST_Norm_RNA_Counts_HavingUPSIT_CommonIn4_Gene_wUPSITScore_HYS.csv')
df <- df[!(df$HYS == 3 | df$HYS == 4), ] #Removing HYS4
df <- df[!(df$HYS == 2 & df$APPRDX == 2), ] #Removing HYS4
df <- df  %>% dplyr::select(-c(1, 2)) #Removing PATNO Column
head(df[1:4, 1:6])

# Convert HYS to factor
df$HYS <- as.factor(df$HYS)

df1 <- df[df$UPSIT_Total < 19 | df$UPSIT_Total > 33, ]

# Change the UPSIT values to categorical labels
df1$UPSIT <- ifelse(df1$UPSIT_Total < 19, "Low", "High")
df1 <- df1 %>% 
  dplyr::select(2, UPSIT, everything())
df1 <- df1  %>% dplyr::select(-c(3)) #Removing UPSIT_Total
head(df1[1:4, 1:3])
df1 <- df1  %>% dplyr::select(c(1, 2, 3))
df1$APPRDX[df1$APPRDX == 1] <- 'Patient'
df1$APPRDX[df1$APPRDX == 2] <- 'Control'


df1 <- df1 %>%
  mutate(APPRDX_HYS = paste(APPRDX, HYS, sep = "_")) %>%
  mutate(APPRDX_HYS = factor(APPRDX_HYS, levels = c("Control_0", "Patient_1", "Patient_2")))

plot_data3 <- df1 %>%
  group_by(APPRDX_HYS, UPSIT) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(APPRDX_HYS) %>%
  mutate(prop = count / sum(count))

# Chi-square or Fisher for UPSIT distribution across APPRDX_HYS groups
tbl3 <- table(df1$APPRDX_HYS, df1$UPSIT)
p_val3 <- if(any(tbl3 < 5)) fisher.test(tbl3)$p.value else chisq.test(tbl3)$p.value
p_adj3 <- p.adjust(p_val3, method = "fdr")
sig_label3 <- if(p_adj3 <= 0.001) "***" else if(p_adj3 <= 0.01) "**" else if(p_adj3 <= 0.05) "*" else "ns"

# Plot
ggplot(plot_data3, aes(x = APPRDX_HYS, y = prop, fill = UPSIT)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1),  # Force y-axis to go only up to 100%
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(values = c("Low" = "#F1746B", "High" = "#36C3D1")) +
  labs(
    title = "",
    x = "",
    y = "Proportion of UPSIT",
    fill = "UPSIT Score"
  ) +
  geom_text(
    aes(x = 2, y = 0.98, label = sig_label3),  # Position adjusted just below top
    inherit.aes = FALSE,
    fontface = "bold",
    size = 6
  ) +
  theme_minimal(base_size = 24) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "grey40"),
    axis.title = element_text(),
    legend.position = "top",
    plot.title = element_text(size = 24),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1)
  )



## Moving asterisk mark
asterisk_y <- 1.08  # Above 100%

ggplot(plot_data3, aes(x = APPRDX_HYS, y = prop, fill = UPSIT)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_y_continuous(
    breaks = c(0, 0.25, 0.5, 0.75, 1),
    labels = scales::percent,
    limits = c(0, 1.12),
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(values = c("Low" = "#F1746B", "High" = "#36C3D1")) +
  labs(
    y = "Proportion of UPSIT",
    fill = "UPSIT Score"
  ) +
  geom_text(
    aes(x = 2, y = asterisk_y, label = sig_label3),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 6
  ) +
  theme_minimal(base_size = 24) +
  theme(
    axis.text.x = element_text(color = "black"),
    axis.text.y = element_text(color = "grey40"),
    legend.position = "top",
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1)
  )
