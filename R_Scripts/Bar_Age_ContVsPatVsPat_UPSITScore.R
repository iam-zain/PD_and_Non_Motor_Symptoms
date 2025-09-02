############################################################
# UPSIT Distribution by Age Category Script
#
# Aim:
#   - Preprocess dataset to filter HYS stages and diagnosis
#   - Categorize UPSIT smell scores into Low vs High groups
#   - Classify Age into four categories (Group_01: <45, Group_02: 45–59, 
#     Group_03: 60–74, Group_04: 75+)
#   - Perform statistical tests (Chi-square or Fisher’s exact) within each APPRDX group
#   - Visualize UPSIT proportions across AgeCategories with annotated significance
#
# Input:
#   - VST_Norm_RNA_Counts_HavingUPSIT_CommonIn4_Gene_wUPSITScore_HYS_Age.csv
#
# Expected Output:
#   - Statistical results with adjusted p-values per APPRDX group
#   - Faceted barplot (Control vs Patient) showing UPSIT distribution by AgeCategory
#   - Asterisk labels (ns, *, **, ***) added to indicate statistical significance
#   - Publication-ready ggplot figures of UPSIT vs AgeCategory
#
# This script highlights how smell loss (UPSIT) distributions vary 
# across age groups within patients and controls.
############################################################









setwd('S:\\PPMI_RNA\\Processed\\WGCNA_UPSIT')

df <- read.csv('VST_Norm_RNA_Counts_HavingUPSIT_CommonIn4_Gene_wUPSITScore_HYS_Age.csv')
df <- df[!(df$HYS == 3 | df$HYS == 4), ] #Removing HYS4
df <- df[!(df$HYS == 2 & df$APPRDX == 2), ] #Removing HYS4
df <- df  %>% dplyr::select(c(3, 4, 5)) #Removing PATNO Column
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'
head(df[1:4, 1:3])


df1 <- df[df$UPSIT_Total < 19 | df$UPSIT_Total > 33, ]

# Change the UPSIT values to categorical labels
df1$UPSIT <- ifelse(df1$UPSIT_Total < 19, "Low", "High")
df1 <- df1 %>% 
  dplyr::select(2, UPSIT, everything())
df1 <- df1  %>% dplyr::select(-c(3)) #Removing UPSIT_Total
head(df1[1:4, 1:3])


# Categorize Age into 4 groups
df1$AgeCategory <- NA  # create a new column

df1$AgeCategory[df1$Age < 45] <- 'Group_01'
df1$AgeCategory[df1$Age >= 45 & df1$Age < 60] <- 'Group_02'
df1$AgeCategory[df1$Age >= 60 & df1$Age < 75] <- 'Group_03'
df1$AgeCategory[df1$Age >= 75] <- 'Group_04'
df1 <- df1  %>% dplyr::select(-c(1)) # Removing Age
head(df1[1:4, 1:3])

df1$AgeCategory <- factor(df1$AgeCategory, levels = c("Group_01", "Group_02", "Group_03", "Group_04"))


# Statistical test per APPRDX group
pvals <- df1 %>%
  group_by(APPRDX) %>%
  summarise(
    p_value = {
      tbl <- table(AgeCategory, UPSIT)
      if (any(tbl < 5)) {
        fisher.test(tbl)$p.value
      } else {
        chisq.test(tbl)$p.value
      }
    },
    .groups = "drop"
  ) %>%
  mutate(p_adj = p.adjust(p_value, method = "fdr"),
         signif_label = case_when(
           p_adj <= 0.001 ~ "***",
           p_adj <= 0.01 ~ "**",
           p_adj <= 0.05 ~ "*",
           TRUE ~ "ns"
         ))

# Prepare data for plotting proportions
plot_data <- df1 %>%
  group_by(APPRDX, AgeCategory, UPSIT) %>%
  summarise(count = n(), .groups = "drop") %>%
  group_by(APPRDX, AgeCategory) %>%
  mutate(prop = count / sum(count))


# Set annotation position above middle x-axis position
annot_df <- pvals %>%
  mutate(x = 2.5, y = 1.055)  # Adjust x for center position and y for height


# Create the plot
ggplot(plot_data, aes(x = AgeCategory, y = prop, fill = UPSIT)) +
  geom_bar(stat = "identity", position = "fill") +
  facet_wrap(~ APPRDX) +
  scale_y_continuous(
    labels = scales::percent,
    limits = c(0, 1.1),  # Extend y-axis slightly to allow space for asterisk
    expand = expansion(mult = c(0, 0))
  ) +
  scale_fill_manual(values = c("Low" = "#F1746B", "High" = "#36C3D1")) +
  labs(
    title = "",
    x = "Age Category",
    y = "Proportion of UPSIT",
    fill = "UPSIT Category"
  ) +
  geom_text(
    data = annot_df,
    aes(x = x, y = y, label = signif_label),
    inherit.aes = FALSE,
    fontface = "bold",
    size = 6
  ) +
  theme_minimal(base_size = 24) +
  theme(
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust = 1, color = "black"),
    axis.text.y = element_text(color = "grey40"),
    legend.position = "top",
    strip.text = element_text(size = 24),
    panel.border = element_rect(color = "grey40", fill = NA, linewidth = 1),
    plot.title = element_text(size = 24)
  )
