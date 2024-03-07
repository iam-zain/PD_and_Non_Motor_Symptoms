
 # 1. Female  ------------------------------------------------------------------

setwd("S:\\PPMI_RNA\\Processed\\Female")
df1 <- read.csv('metaDataIR3_OnlyPat_Healthy_PATNO_Filter_Female.csv')

df2 <- vroom('All_Counts_PatAndHealthy_Female.csv')
df3 <- t(df2) # transposing df2
df3 <- as.data.frame(df3) # to dataframe
df3 <- df3 %>% rownames_to_column(var= 'new') # naming 1st column
colnames(df3) <- df3[1, ] # naming column names as 1st row names
df3 <- df3[-1, ] # now removing the 1st row, which is equal to the column names

df_merge <- merge(df1, df3, by.x = 'HudAlphaID', by.y = 'Geneid', all = FALSE)
write.csv(df_merge, 'All_Counts_PatAndHealthy_Disease_Female.csv', row.names = F)


 # 2. Male  --------------------------------------------------------------------

setwd("S:\\PPMI_RNA\\Processed\\Male")
df1 <- read.csv('metaDataIR3_OnlyPat_Healthy_PATNO_Filter_Male.csv')

df2 <- vroom('All_Counts_PatAndHealthy_Male.csv')
df3 <- t(df2) # transposing df2
df3 <- as.data.frame(df3) # to dataframe
df3 <- df3 %>% rownames_to_column(var= 'new') # naming 1st column
colnames(df3) <- df3[1, ] # naming column names as 1st row names
df3 <- df3[-1, ] # now removing the 1st row, which is equal to the column names

df_merge <- merge(df1, df3, by.x = 'HudAlphaID', by.y = 'Geneid', all = FALSE)
write.csv(df_merge, 'All_Counts_PatAndHealthy_Disease_Male.csv', row.names = F)


 # 3. Combined  ----------------------------------------------------------------

setwd("S:\\PPMI_RNA\\Processed")
df1 <- read.csv('metaDataIR3_OnlyPat_Healthy.csv')

df2 <- vroom('All_Counts_PatAndHealthy.csv')
df3 <- t(df2) # transposing df2
df3 <- as.data.frame(df3) # to dataframe
df3 <- df3 %>% rownames_to_column(var= 'new') # naming 1st column
colnames(df3) <- df3[1, ] # naming column names as 1st row names
df3 <- df3[-1, ] # now removing the 1st row, which is equal to the column names

df_merge <- merge(df1, df3, by.x = 'HudAlphaID', by.y = 'Geneid', all = FALSE)
write.csv(df_merge, 'All_Counts_PatAndHealthy_Disease.csv', row.names = F)
