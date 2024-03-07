setwd ('Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Male\\GeneCpG\\Cognition')
df = read.csv('Cognition_Top50sMerge_Data_Male.csv')
df$APPRDX[df$APPRDX == 1] <- 'Patient'
df$APPRDX[df$APPRDX == 2] <- 'Control'
df1 <- df  %>% dplyr::select(-"PATNO") # Removing a column
df1 <- df1[!(df1$HYS %in% c(3, 4)), ] #Removing HYS 3 and 4

## Selecting HYS and Patient
df_HYS0P <- subset(df1, HYS == 0 & APPRDX == 'Patient')
df_HYS1P <- subset(df1, HYS == 1 & APPRDX == 'Patient')
df_HYS2P <- subset(df1, HYS == 2 & APPRDX == 'Patient')

## Selecting HYS and Control
df_HYS0C <- subset(df1, HYS == 0 & APPRDX == 'Control')
df_HYS1C <- subset(df1, HYS == 1 & APPRDX == 'Control')
df_HYS2C <- subset(df1, HYS == 2 & APPRDX == 'Control')

# calculate median of each column from 3rd column onwards
medians1 <- as.data.frame(apply(df_HYS0P[, 3:length(df_HYS0P)], 2, median))
medians1 <- rownames_to_column(medians1, var = "row_index") # Making CpG names as 1st column names (earlier it is row names)
medians2 <- as.data.frame(apply(df_HYS1P[, 3:length(df_HYS1P)], 2, median))
medians2 <- rownames_to_column(medians2, var = "row_index")
medians3 <- as.data.frame(apply(df_HYS2P[, 3:length(df_HYS2P)], 2, median))
medians3 <- rownames_to_column(medians3, var = "row_index")

medians4 <- as.data.frame(apply(df_HYS0C[, 3:length(df_HYS0C)], 2, median))
medians4 <- rownames_to_column(medians4, var = "row_index")
medians5 <- as.data.frame(apply(df_HYS1C[, 3:length(df_HYS1C)], 2, median))
medians5 <- rownames_to_column(medians5, var = "row_index")
medians6 <- as.data.frame(apply(df_HYS2C[, 3:length(df_HYS2C)], 2, median))
medians6 <- rownames_to_column(medians6, var = "row_index")

df_med = data.frame(matrix(ncol = 0, nrow = 99)) # Creating dataframe and storing values
df_med$CpG = medians1$row_index
df_med$df_HYS0P = medians1$`apply(df_HYS0P[, 3:length(df_HYS0P)], 2, median)`
df_med$df_HYS1P = medians2$`apply(df_HYS1P[, 3:length(df_HYS1P)], 2, median)`
df_med$df_HYS2P = medians3$`apply(df_HYS2P[, 3:length(df_HYS2P)], 2, median)`
df_med$df_HYS0C = medians4$`apply(df_HYS0C[, 3:length(df_HYS0C)], 2, median)`
df_med$df_HYS1C = medians5$`apply(df_HYS1C[, 3:length(df_HYS1C)], 2, median)`
df_med$df_HYS2C = medians6$`apply(df_HYS2C[, 3:length(df_HYS2C)], 2, median)`

# calculate the difference of each cell, HYS wise in Patient & Control
diff_HYS0 <- (df_med$df_HYS0P - df_med$df_HYS0C) # Calculating value of difference
df_med$HYS0 <- diff_HYS0

diff_HYS1 <- (df_med$df_HYS1P - df_med$df_HYS1C)
df_med$HYS1 <- diff_HYS1

diff_HYS2 <- (df_med$df_HYS2P - df_med$df_HYS2C)
df_med$HYS2 <- diff_HYS2

# Only considering those CpGs whose value either increasing or decreasing in Patient & Control
df_filtered <- df_med %>%
  filter(sign(HYS0) == sign(HYS1), sign(HYS0) == sign(HYS2))

sum_HYS <- abs(df_filtered$HYS0) + abs(df_filtered$HYS1) + abs(df_filtered$HYS2) # Taking sum of all differences
df_filtered$diffHYS <- sum_HYS
df_filtered <- as.data.frame(df_filtered[order(-df_filtered$diffHYS),]) # Descending order of difference
df_med10 <- df_filtered[1:9,] # Selecting top 9 CpGs

subset_df1 <- df1[, which( colnames(df1)  %in% df_med10$CpG)]
subset_df1$APPRDX <- df1$APPRDX
subset_df1$HYS <- df1$HYS

df_long <- melt (subset_df1, id.vars = c("HYS", 'APPRDX'), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS) 

library(gridExtra)
plot_list=list()

for(i in 1:9){
  #df_long <- melt (subset_df1, id.vars = c("HYS", 'APPRDX'), variable.name = "CpG")
  #df_long$HYS = as.character(df_long$HYS)
  plot_list[[i]]<-ggplot(df_long[which(df_long$CpG==colnames(subset_df1)[i]),], aes(x=HYS, y=value,color=APPRDX)) +
    geom_boxplot() +
    labs(x = "HYS", y = "Methylation Value", title =  colnames(subset_df1)[i]) +
    scale_color_manual(values = c("blue", "red"), name = "APPRDX") +
    theme_minimal() +
    theme(
      axis.text = element_text(face = "bold", color = "black", size = 12),
      axis.title = element_text(face = "bold",color = "black", size = 14),
      axis.text.x = element_text(angle = 0, vjust = 0.5, hjust=1),
      #legend.position = c(0.85, 0.15),
      legend.background = element_rect(fill = "azure", color = "black"),
      legend.title = element_text(face = "bold", size = 14),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      panel.border = element_rect(color = "black", fill = NA, size = 1),
      plot.title = element_text(face = "bold", size = 16))

}
final_plot <- do.call(grid.arrange,c(plot_list,ncol=3))

#plot_list[[1]]+plot_list[[2]]
