library(ggplot2)

upsit = read.csv("UPSIT_Total_Methylome_5IranCateg.csv")
dim(upsit)
head(upsit[,1:10])

upsit$APPRDX = as.factor(upsit$APPRDX)
upsit$Gender = as.factor(upsit$Gender)

boxplot(UPSIT_Total~HYS, data = upsit)

boxplot(cg06067658~HYS, data = upsit)
cor(upsit$HYS, upsit$cg10291990)

head(upsit[,7:8])
highCorUPSIT = c()
for (i in 8:ncol(upsit)) {
   if(abs(cor(upsit$HYS, upsit[,i]))>0.4){
      print(names(upsit)[i])
      highCorUPSIT = c(highCorUPSIT,names(upsit)[i])
   }
}
upsit2 = upsit[,highCorUPSIT]

boxplot(cg18581963~HYS, data = upsit)
boxplot(cg18581963~Smell_Category, data = upsit)

boxplot(Smell_Category~HYS, data = upsit)
boxplot(HYS~Smell_Category, data = upsit)

plot(upsit$Smell_Category, upsit$HYS)
plot(upsit$HYS, upsit$cg18581963)

upsit1 = upsit[,-c(1,2,4,5,6,7)]
head(upsit1[,1:10])
table(upsit1$HYS)
upsit1$HYS = as.factor(upsit1$HYS)

library(caret)
rf1 = train(HYS~., data = upsit1, method = "rf")
rf1
vImp = (varImp(rf1))
vImp$
write.csv(vImp$importance, "impCpGs.csv")
boxplot(cg04674736~HYS, data = upsit)
boxplot(cg04674736~APPRDX, data = upsit)
boxplot(cg04674736~Smell_Category, data = upsit)
boxplot(cg18581963~UPSIT_Total, data = upsit)
boxplot(cg04674736~UPSIT_Total, data = upsit)

boxplot(cg04674736~Gender, data = upsit)



bp = ggplot(data = upsit, aes(x = APPRDX, y = HYS, group=APPRDX)) +
  geom_boxplot(aes(fill=APPRDX))+
  labs(x="Hoehn and Yahr Staging Scale of PD", y = "UPSIT Score")
bp + facet_grid(. ~ APPRDX)

bp = ggplot(data = upsit, aes(x = HYS, y = UPSIT_Total, group=HYS)) +
   geom_boxplot(aes(fill=HYS))+
  labs(x="Hoehn and Yahr Staging Scale of PD", y = "UPSIT Score")
bp + facet_grid(. ~ APPRDX)

ggplot(data = upsit, aes(x = UPSIT_Total, y = cg04674736)) +
   geom_boxplot()
bp <- ggplot(upsit, aes(x=UPSIT_Total, y=cg03659012, group=UPSIT_Total)) + 
   geom_boxplot(aes(fill=UPSIT_Total))+
  labs(x="Smell dysfunction", y = "Methylation level")
bp
bp + facet_grid(. ~ Gender)

bp <- ggplot(upsit, aes(x=HYS, y=cg04674736, group=HYS)) + 
   geom_boxplot(aes(fill=HYS))+
  labs(x="Desieasae Severity", y = "Methylation level") 
bp
bp + facet_grid(. ~ Gender)

bp <- ggplot(upsit, aes(x=UPSIT_Total, y=cg04674736, group=UPSIT_Total)) + 
   geom_boxplot(aes(fill=UPSIT_Total))
bp + labs(x="UPSIT Score") 
bp + facet_grid(. ~ APPRDX)


bp <- ggplot(upsit, aes(x=APPRDX, y=cg04674736, group=APPRDX)) + 
   geom_boxplot(aes(fill=APPRDX))
bp+ facet_grid(. ~ Gender)

bp <- ggplot(upsit, aes(x=APPRDX, y=cg04674736, group=APPRDX)) + 
   geom_boxplot(aes(fill=APPRDX))
bp+ facet_grid(. ~ HYS)

bp <- ggplot(upsit, aes(x=HYS, y=cg04674736, group=HYS)) + 
   geom_boxplot(aes(fill=HYS))
bp+ facet_grid(. ~ APPRDX)


bp <- ggplot(upsit, aes(x=APPRDX, y=cg18581963, group=APPRDX)) + 
   geom_boxplot(aes(fill=APPRDX))
bp+ facet_grid(. ~ HYS)



bp <- ggplot(upsit, aes(x=Gender, y=cg18581963, group=Gender)) + 
   geom_boxplot(aes(fill=Gender))
bp+ facet_grid(. ~ HYS)



bp <- ggplot(upsit, aes(x=APPRDX,y=UPSIT_Total, group=APPRDX)) + 
   geom_boxplot(aes(fill=APPRDX))
bp+ facet_grid(. ~ HYS)


head(upsit2)
dim(upsit1)
dim(upsit2)
# write.csv(upsit1, "upsit1.csv", row.names = F)
# write.csv(upsit2, "upsit2.csv", row.names = F)
dim(upsit1)
upsitPCA = prcomp(upsit1[,-1])
upsitPCA2 = prcomp(upsit2[,-61])
upsitPCA$x
upsitPCA$x[,1]
sum(apply(upsit1, 2, var))
sum(apply(upsitPCA$x[,1:100], 2, var))
biplot(upsitPCA)
screeplot(upsitPCA)

table(upsit1$HYS)
table(upsit$Smell_Category)
plot(upsitPCA$x[,1], upsitPCA$x[,2], col = c(upsit$Smell_Category))
plot(upsitPCA$x[,1], upsitPCA$x[,2], col = c(upsit$APPRDX)+1)

pal <- colorRamp(c("yellow", "orange", "red"))    # 1) choose colors
col <- rgb(pal((upsit$cg04674736 - min(upsit$cg04674736)) / diff(range(0,1))), max=255)  # 2) interpolate numbers
plot(upsitPCA$x[,1], upsitPCA$x[,2], col = col, pch=19)


library(ggfortify)
table(upsit$UPSIT_Total)
autoplot(upsitPCA,data = upsit, colour = 'UPSIT_Total')
autoplot(upsitPCA,data = upsit, colour = 'HYS')

autoplot(upsitPCA2,data = upsit, colour = 'UPSIT_Total')
autoplot(upsitPCA2,data = upsit, colour = 'HYS')

#autoplot(upsitPCA,data = upsit, colour = 'Smell_Category', frame = TRUE, frame.type = "norm")
#autoplot(upsitPCA,data = upsit, colour = 'UPSIT_Total', label = TRUE, label.size = 3)
plot(upsitPCA, type="l")
screeplot(upsitPCA, npcs = 15, type = c("lines"))
screeplot(upsitPCA2, npcs = 10, type = c("lines"))

scores = as.data.frame(upsitPCA$x)
scores$APPRDX = upsit$APPRDX
head(scores)
boxplot(PC1~APPRDX, data = scores)

colrs = c("red", "yellow", "green", "blue", "black")

plot(upsitPCA$x[,1], upsitPCA$x[,2], col = colrs[c(upsit$Smell_Category)])
plot(upsitPCA$x[,1], upsitPCA$x[,2], col = colrs[c(upsit$HYS)])


biplot(upsitPCA)
########################################################
library(caret)
trctl1 = trainControl(method = "repeatedcv", repeats = 5)
trctl2 = trainControl(method = "cv", number = 10)
dim(upsit2)
class(upsit$APPRDX) = as.factor(upsit$APPRDX)
upsit1
upsit2$APPRDX =  as.factor(upsit$APPRDX)
upsit1$APPRDX =  as.factor(upsit$APPRDX)
rf1 = train(APPRDX~., data = upsit2, method="rf", trControl = trctl)
rf2 = train(APPRDX~., data = upsit1[,-1], method="rf", trControl = trctl2)
mean(rf1$resample[,1])
mean(rf2$resample[,1])


impCpG = read.csv("impCpGs.csv")
head(impCpG)
dim(impCpG)
upsit3 = upsit[,impCpG$CpGs[1:60]]
upsit3$APPRDX = as.factor(upsit$APPRDX)
head(upsit1)

rf3 = train(APPRDX~., data = upsit3, method="rf", trControl = trctl2)
rf3$resample
boxplot(rf1$resample[,1], rf2$resample[,1], rf3$resample[,1])

glm1 <- glm(APPRDX~., family="binomial", data=upsit1[,-1])
#use model to make predictions on test set
predicted <- predict(glm1, upsit1[,-1], type="response")

glm3 <- glm(APPRDX~., family="binomial", data=upsit3)
predicted <- predict(glm3, upsit3, type="response")

glm2 <- glm(APPRDX~., family="binomial", data=upsit1[,-1])
predicted <- predict(glm2, upsit1[,-1], type="response")
predicted <- predict(glm3, upsit3, type="response")

glm4 = glm(APPRDX~HYS, family="binomial", data = upsit, )
predicted <- predict(glm4, upsit[,c("APPRDX","HYS")], type="response")
library(pROC)
rocobj <- roc(upsit$APPRDX, predicted)
ggroc(rocobj, colour = 'steelblue', size = 1)

rocobj$auc

########################################################
library(readxl)
library(reshape)
library(ggplot2)
data1 = read_excel("BoxPlot_RanFor_Diff_Analyses_Categorized.xlsx", sheet = 1)
head(data1)
class(data1)
boxplot(data1)
boxplot(data1[,-1], 
        boxwex = 0.5, col = "bisque", notch = T)

meltData1 <- melt(as.data.frame(data1[,-1]))
head(meltData1)

bp <- ggplot(meltData1, aes(x=variable, y=value, group=variable)) + 
   geom_boxplot(aes(fill=variable))+
   labs(x="Selected Features", y = "Accuracy")+
   theme(axis.text.x = element_text(angle = 90, hjust=1), 
         axis.text=element_text(size=15),
         axis.title=element_text(size=14,face="bold")
         )
bp



data2 = read_excel("NonMot_Top10_Common4_CategoryCount_APPRDX.xlsx", sheet = 1)
head(data2)
data2 = as.data.frame(data2[,2:5])
meltData2.N = melt(data2[,c(1,2)])
data2$APPRDX = as.factor(data2$APPRDX)

bp <- ggplot(data2[,c(1,4)], aes(x=APPRDX, y=Severe, group=APPRDX)) + 
   geom_boxplot(aes(fill=APPRDX))+
   labs(x="Participants", y = "Count")
bp

pat  = data2[which(data2$APPRDX == 1),]

hc  = data2[which(data2$APPRDX == 2),]
counts = data.frame(rbind(
   (apply(pat[,-1], 2, sum)/sum(apply(pat[,-1], 2, sum)))*100,
   (apply(hc[,-1], 2, sum)/sum(apply(hc[,-1], 2, sum)))*100
   
))
counts$Cat = c("Disease", "Healthy")
counts = melt(counts)

ggplot(data=counts, aes(x=Cat, y=value, fill=variable)) +
  geom_bar(stat="identity") +
  labs(x=" ", y = "% Cases")+
  scale_fill_manual(values=c("#086A87","#B18904","#6E6E6E"))

ggplot(data=counts, aes(x=variable, y=value, fill=Cat)) +
  geom_bar(stat="identity", position=position_dodge()) 


data3 = read_excel("BoxPlot_Individual_AllMerge_RanFor_EqualSample.xlsx", sheet = 1)
head(data3)
meltData3 <- melt(as.data.frame(data3))
head(meltData3)
bp <- ggplot(meltData3, aes(x=variable, y=value, group=variable)) + 
   geom_boxplot(aes(fill=variable))+
   labs(x="Features", y = "Accuracy")+
   theme(axis.text.x = element_text(angle = 90, hjust=1), 
         axis.text=element_text(size=12),
         axis.title=element_text(size=14,face="bold")
   )
bp
