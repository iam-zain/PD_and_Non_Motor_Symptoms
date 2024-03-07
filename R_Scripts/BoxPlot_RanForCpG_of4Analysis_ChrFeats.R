setwd ("Z:\\PPMI_Data\\Excel_Data\\NonMotors\\Feature_CpG_Gene_Association\\Xtra_Files\\CommonAll_9Features")
df <- read.csv("BoxPlot_ChrNMI_CommonFeats_RanFor.csv", header = T)

par(font.axis = 2) #To make x axis values bold #cex for increasing font size
boxplot(df$NMI..05,df$Random.58.CpG,df$NMI..04,df$Random.619.CpG, df$Common.CpG.in.Features,df$Random.47.CpG,
        df$Common.NMI...Features,df$Random.6.CpG, cex.main=2,
        main = "Random Forest Model Comparison- Top CpG in Chromosome & Features",  
         names = c("NMI>.05", "Random 58 CpG", "NMI>.04", "Random 619 CpG", "Common CpG in Features",
                   "Random 47 CpG", "Common NMI & Features", "Random 6 CpG"),
         col = c("cyan2","deeppink1"),yaxt="n")
axis(2, at=seq(0.3,0.85,0.05),font=2) #To make y axis values bold
title(ylab = "Accuracy", font.lab = 2) # To create y label and bolden it

