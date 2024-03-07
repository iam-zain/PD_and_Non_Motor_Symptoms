setwd ('Z:\\PPMI_Data\\Excels\\NonMotors\\The251\\Male')
df = read.csv('Any2Common_inAll45Feats_Data_Male.csv')
df = df[,2:13]
df1 = df[df$APPRDX==1, ]
df2 = df[df$APPRDX==2, ]

######
counts1 =apply(df1[,2:ncol(df1)],2,table)
counts1_df <- do.call(rbind, counts1)

counts2 =apply(df2[,2:ncol(df2)],2,table)
counts2_df <- do.call(rbind, counts2)

merged_counts <- as.data.frame(cbind(counts1_df, counts2_df))
write.csv(merged_counts, 'Counts_TopCommon11Feats_Male.csv')

########
df = read.csv("Counts_TopCommon11Feats_Male.csv",header = T)
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'

#### 1. Feature ####
columns <- names(df)[3:length(df)]
plot <- list()
for (col in columns) {
  df_1 <- df %>% select(c(1,2,col))
  df_long <- reshape2::melt(df_1, id.vars = c("APPRDX", "Severity"), variable.name = "Feature")
  
  df_summary <- df_long %>% 
    group_by(APPRDX, Severity) %>% 
    summarize(count = sum(value)) %>% 
    mutate(percent = count / sum(count) * 100)
  
  p <- ggplot(df_summary, aes(x = Severity, y = percent, fill = APPRDX)) + 
    geom_bar(position = 'dodge', stat = "identity") + 
    labs(x = "\nSeverity", y = "Percentage\n", 
         title = paste((col), "- Male")) +
    scale_color_manual(values = c("blue", "red"), name = "APPRDX") +
    theme_minimal() +
    theme(
      axis.text = element_text(face = "bold", color = "black", size = 18),
      axis.title = element_text(face = "bold",color = "black", size = 20),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
      #legend.position = c(0.85, 0.15),
      legend.background = element_rect(fill = "azure", color = "black"),
      legend.title = element_text(face = "bold", size = 20),
      #panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, linewidth = 1),
      plot.title = element_text(face = "bold", size = 24))
  print(p)
  ggsave(paste0(col,"_Percentage_Male.svg"))
}


