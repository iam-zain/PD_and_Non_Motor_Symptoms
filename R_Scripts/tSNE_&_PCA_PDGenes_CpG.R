
setwd("Z:\\PPMI_Data\\Excel_Data")
getwd()

library(ggplot2)
library(Rtsne)

chrd= read.csv("CpG_PDGenes_List_All_PatientData_Apprdx.csv",header = T)
chr <- data.matrix(chrd)

IR_data <- chrd[ ,1:2893]
IR_target <- chrd[ ,2894]

tsne_results <- Rtsne(IR_data, perplexity=100, max_iter = 6000, pca = T, check_duplicates = T)

par(mfrow=c(1,2)) # To plot two images side-by-side

data <- as.data.frame(cbind(tsne_results$Y,as.factor(chrd[ ,2894])))

data[,"V3"] <- as.character(data$V3)  
plot(V2~V1, data)
ggplot(data, aes(V1,V2, colour=V3)) + geom_point() + 
  scale_color_discrete(name = 'Patient ID', labels = c("PD Patient", "Healthy Control", "SWEDD")) + 
  labs(title = waiver(), x="Dim_1" ,y="Dim_2") + ggtitle("tSNE_PDGenes_List_All_PatientID")+
  geom_smooth(method = "lm", formula = y~x, se = F)



ggplot(data, aes(V1,V2, colour=V3)) + geom_point() + 
  scale_color_discrete(name = 'Patient ID', labels = c("PD Patient", "Healthy Control", "SWEDD")) + 
  labs(title = waiver(), x="Dim_1" ,y="Dim_2") + ggtitle("tSNE_PDGenes_List_All_PatientID_Separate") + 
  facet_wrap(~V3)








  #scale_color_discrete(name = 'Patient ID', labels = c("PD Patient", "Healthy Control", "SWEDD")) + 
  labs(title = waiver(), x="Dim_1" ,y="Dim_2") + ggtitle("tSNE_PDGenes_List_All_PatientID")+
  geom_smooth(method = "lm", formula = y~x, se = F) +
  scale_fill_manual(values = c(1="yellow"))

  saveRDS(data,"Z:\\PPMI_Data\\Excel_Data\\data.rds" )



# #which(is.na(chrpca), arr.ind=TRUE)
# row col



chrpca = read.csv("CpG_PDGenes_List_All_PatientData_Apprdx.csv",header = T)
head(chrpca)
nrow(chrpca)
ncol(chrpca1)
chrpca1 <- chrpca[-2894]

merapca = prcomp(chrpca1, retx = T, scale = F)
View(merapca)

df <- merapca$rotation

merapca1 = prcomp(chrpca1)
View(merapca1)

imp <-summary(merapca)
imp$sdev

merapca$sdev
library(ggplot2)

meradata = data.frame(cbind(chrpca, merapca$x[,1:2]))

meradata[,"APPRDX"] <- as.character(meradata$APPRDX)  
plot(V2~V1, data)
ggplot(meradata, aes(PC1, PC2,  color = APPRDX)) +
  geom_point(position = position_jitter()) +
  scale_color_discrete(name = 'Patient ID', labels = c("PD Patient", "Healthy Control", "SWEDD")) +
  labs(x = "PC1 (13.1%)", y= "PC2 (8.6%)") + 
  ggtitle("PCA_PDGenes_List_All_PatientID")+
  geom_smooth(method = "lm", formula = y~x, se=F)

dd1 <- summary(merapca)
View(dd1$importance)
summary(prcomp(chrpca, scale. = TRUE))
View(dd)
View(merapca)

PCA_all_PDGenes_List_All_PatientID = dd$importance[3,]
write.csv(PCA_all_PDGenes_List_1_PatientID, 'PCA_all_PDGenes_List_All_PatientID.csv')

library(ggbiplot)
print(ggbiplot(merapca, obs.scale = 1, var.scale = 1, groups = meradata$APPRDX, ellipse = TRUE, circle = TRUE))


