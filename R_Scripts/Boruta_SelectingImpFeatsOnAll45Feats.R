setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\NonMot_Files")
df = read.csv("NonMotor_SocioBehavior_Categorized.csv") #With226 Variance removed
df <- df [which(df$APPRDX != 3),]
df <- df  %>% select(-c(1)) #Remove PATNO
#df <- df  %>% select(-c("REM_Neuro_Category")) #Remove REM Neuro

#Boruta package for feature selection
set.seed(1)
res_bor <- Boruta (APPRDX ~ ., data = df, doTrace = 2, maxRuns = 999)
sel_bor <- TentativeRoughFix(res_bor)
CpG_attributes <- attStats(res_bor)
imp_CpG <- getNonRejectedFormula(res_bor) #Shows confirmed + tentative
imp_Conf_CpG <- getConfirmedFormula(res_bor) #Shows only confirmed important

imp_CpG_Data <- df %>% select(PATNO, APPRDX, LetterNumber_Category , MDS_Depress_Category , MDS_Apathy_Category , 
                                MDSP_Constipate_Category , MDSP_Fatigue_Category , Montreal_Cognitive_Category , 
                                SCOPA_Gastro_Category , SCOPA_Sex_Category , Symbol_Digit_Category , 
                                UPSIT_3_Category)

write.csv(imp_CpG_Data, "BorutaFeatsUnrejected_OnAll45Feats.csv", row.names = F)
