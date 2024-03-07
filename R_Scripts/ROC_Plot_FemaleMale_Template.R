library(MLeval)

################ Female ##############
##########  45 Feats  ###########
setwd("Z:\\PPMI_Data\\Excels\\Only_Female")
NonMot = read.csv('NonMotor_Categorized_Female.csv')
NonMot1 = NonMot [,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv", number = 10, repeats=10, savePredictions = T, classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf', trControl = control)
x = evalm(fit.cv,cols = "blue", gnames = '', fsize = 18, rlinethick = 1.5, plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - Female"))))

##########  Top 6 Feats  ###########
NonMot = read.csv('ImpFeats_CommonIn2_DataFemale.csv')
NonMot1 = NonMot [,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv", number = 10, repeats=10, savePredictions = T, classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf', trControl = control)
x = evalm(fit.cv,cols = "blue", gnames = '', fsize = 18, rlinethick = 1.5, plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 6 Common Features - Female"))))



################ Male ##############
##########  45 Feats  ###########
setwd("Z:\\PPMI_Data\\Excels\\Only_Male")
NonMot = read.csv('NonMotor_Categorized_Male.csv')
NonMot1 = NonMot [,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv", number = 10, repeats=10, savePredictions = T, classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf', trControl = control)
x = evalm(fit.cv,cols = "blue", gnames = '', fsize = 18, rlinethick = 1.5, plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - Male"))))

##########  Top 9 Feats  ###########
setwd("Z:\\PPMI_Data\\Excels\\Only_Male")
NonMot = read.csv('ImpFeats_CommonIn2_DataMale.csv')
NonMot1 = NonMot [,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv", number = 10, repeats=10, savePredictions = T, classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf', trControl = control)
x = evalm(fit.cv,cols = "blue", gnames = '', fsize = 18, rlinethick = 1.5, plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 9 Common Features - Male"))))
