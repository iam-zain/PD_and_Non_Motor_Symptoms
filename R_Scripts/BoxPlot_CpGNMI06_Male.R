setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Male\\GeneCpG\\Xtras\\TopCpG_NMI_Compare")
df = read.csv("NMI_Methylome_Male06_Data.csv",header = T)
df <- df  %>% select(-c("PATNO")) #Remove PATNO
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'
df_long <- reshape2::melt (df, id.vars = c("APPRDX"), variable.name = "CpG")
df_long$CpG <- reorder(df_long$CpG, df_long$value, median)

ggplot(df_long, aes(x = CpG, y = value, color = APPRDX)) +
  geom_boxplot() +
  labs(x = "\n", y = "Methylation Value\n", title = "Methylation") +
  scale_color_manual(values = c("blue", "red"), name = "APPRDX") +
  theme_minimal() +
  theme(
    axis.text = element_text(face = "bold", size = 14),
    axis.title = element_text(face = "bold", size = 16),
    axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1),
    legend.position = c(0.15, 0.85),
    legend.background = element_rect(fill = "azure", color = "black"),
    legend.title = element_text(face = "bold", size = 14),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
    plot.title = element_text(face = "bold", size = 18))

