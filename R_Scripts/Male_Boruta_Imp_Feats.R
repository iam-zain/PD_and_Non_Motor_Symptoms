setwd("/home/system-02/zain")
df = read.csv("NonMotor_Categorized_Male.csv")
df = df [,-c(1)] # Removing PATNO

#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = df, doTrace = 2, maxRuns = 999)
sel_bor <- TentativeRoughFix(res_bor)
CpG_attributes <- attStats(res_bor)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
imp_Conf_CpG <- getConfirmedFormula(res_bor) #Shows only confirmed important

write.csv(imp_Conf_CpG, "ImpFeats_Boruta.csv")

#LetterNumber_Category + MDS_Depress_Category + MDS_Apathy_Category + 
#MDSP_Fatigue_Category + Montreal_Cognitive_Category + SCOPA_Sex_Category + 
#Symbol_Digit_Category + Trail_Making_B_Category + UPSIT_3_Category