setwd ("Z:\\PPMI_Data\\Excels\\Only_Female\\GeneCpG\\Urine")
df = vroom("UrineCateg_Methylome_Female.csv") 
head(df[1:8,1:8])
df <- df [which(df$APPRDX != 3),]
df1 <- df  %>% select(-c(1,2,3,6,5,7,8)) #Remove PATNO

#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = df1, doTrace = 2, maxRuns = 999)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
# sel_bor <- TentativeRoughFix(res_bor)
# CpG_attributes <- attStats(res_bor)
# imp_Conf_CpG <- getConfirmedFormula(res_bor) #Shows only confirmed important

imp_CpG_Data <- df %>% select(PATNO, APPRDX,cg26856497,cg21909650,cg22597771,cg25079847,
                                cg06996696,cg01686420,cg03894242)

write.csv(imp_CpG_Data, "BorutaR_Urine_CpG_Female.csv", row.names = F)

### NMI
df2 <- read.csv("Urine_NMI_Top50_Female.csv")
mylist <- df2$CpG
nmidf <- df[,mylist]
nmidf$PATNO <- df$PATNO
nmidf$APPRDX <- df$APPRDX
write.csv(nmidf, "NMI_Top50_Urine_Methylome_Female.csv", row.names = F)


### boruta
df2 <- read.csv("Urine_BorutaFemale_Top50.csv")
mylist <- df2$CpG
nmidf <- df[,mylist]
nmidf$PATNO <- df$PATNO
nmidf$APPRDX <- df$APPRDX
write.csv(nmidf, "Boruta_Top50_Urine_Methylome_Female.csv", row.names = F)

