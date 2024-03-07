setwd ("Z:\\PPMI_Data\\Excels\\Only_Male\\GeneCpG\\Apathy")
df = read.csv("ApathyCateg_Methylome_Male.csv") #With226 Variance removed
head(df[1:8,1:8])
df <- df [which(df$APPRDX != 3),]
df1 <- df  %>% select(-c(1,2,3,5,6,7,8)) #Remove PATNO

#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = df1, doTrace = 2, maxRuns = 999)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
# sel_bor <- TentativeRoughFix(res_bor)
# CpG_attributes <- attStats(res_bor)
# imp_Conf_CpG <- getConfirmedFormula(res_bor) #Shows only confirmed important

imp_CpG_Data <- df %>% select(PATNO, APPRDX, cg05903298,cg11865360,cg24845267,cg07777378,cg02157463)

write.csv(imp_CpG_Data, "BorutaR_Apathy_CpG_Male.csv", row.names = F)
