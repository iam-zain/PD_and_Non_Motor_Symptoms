setwd ("Z:\\PPMI_Data\\Excel_Data\\NonMotors\\Feature_CpG_Gene_Association\\UPSIT_Processed_Files")

### HYS only
df = read.csv("UPSIT3_HYS_Score.csv",header = T)

df_long <- melt (df, id.vars = c("HYS"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS)

ggplot(df_long, aes(x= HYS, y=value, color = HYS))+
  geom_boxplot()+ ylab("UPSIT Score") +
  theme(legend.position=c(0.92, 0.9), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("UPSIT Score HYS wise in All")



### With APPRDX
df = read.csv("UPSIT3_HYS_APPRDX_Score.csv",header = T)

df_long <- melt (df, id.vars = c("HYS", "APPRDX"), variable.name = "CpG")
df_long$HYS = as.character(df_long$HYS)
df_long$APPRDX = as.character(df_long$APPRDX)

ggplot(df_long, aes(x= APPRDX, y=value, color = HYS))+
  geom_boxplot()+ ylab("UPSIT Score") +
  theme(legend.position=c(0.94, 0.14), legend.background = element_rect(fill="azure",size=0.1,
  linetype="solid", colour ="black"),panel.border = element_rect(colour = "black", fill=NA, size=1)) +
  ggtitle ("UPSIT Score HYS wise in Patient & Control")
