library(MLeval)

##########################  Uncategorized 
########## 21 Feats
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\NonMot_Files_RAW_PreProcess\\Feats21_UnCategorized")
NonMot = read.csv('Feats21_unCateg_APPRDX.csv')
rownames(NonMot)=NonMot$PATNO
NonMot1= NonMot[,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv",number = 10,savePredictions = T,classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf',trControl = control)
x = evalm(fit.cv,cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 21 UnCategorized Features"))))


##########################  Categorized 
########## 21 Feats
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Main21")
NonMot = read.csv('NonMotor_MainFeatures_Categorized.csv')
rownames(NonMot)=NonMot$PATNO
NonMot1= NonMot[,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv",number = 10,savePredictions = T,classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf',trControl = control)
x = evalm(fit.cv,cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 21 Main Features"))))

########## 10 Feats
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Main21")
NonMot = read.csv('Top10_from22Feats_Data.csv')
rownames(NonMot)=NonMot$PATNO
NonMot1= NonMot[,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv",number = 10,savePredictions = T,classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf',trControl = control)
x = evalm(fit.cv,cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 10 Main Features"))))




######### 45 Feats
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")
NonMot = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
rownames(NonMot)=NonMot$PATNO
NonMot1= NonMot[,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv",number = 10,savePredictions = T,classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf',trControl = control)
x = evalm(fit.cv,cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 sub Features"))))

######### 12 Feats
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")
NonMot = read.csv('Any2Common_inAll45Feats_Data.csv')
rownames(NonMot)=NonMot$PATNO
NonMot1= NonMot[,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv",number = 10,savePredictions = T,classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf',trControl = control)
x = evalm(fit.cv,cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: Top 12 sub Features"))))





################ Female ##############
##########  45 Feats  ###########
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Female")
NonMot = read.csv('NonMotor_Categorized_Female.csv')
NonMot1 = NonMot [,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv", number = 10, savePredictions = T, classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf', trControl = control)
x = evalm(fit.cv,cols = "blue", gnames = '', fsize = 18, rlinethick = 1.5, plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - Female"))))

##########  Top 8 Feats  ###########
NonMot = read.csv('Any2Common_inAll45Feats_Data_Female.csv')
NonMot1 = NonMot [,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv", number = 10, savePredictions = T, classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf', trControl = control)
x = evalm(fit.cv,cols = "blue", gnames = '', fsize = 18, rlinethick = 1.5, plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 8 Common Features - Female"))))



################ Male ##############
##########  45 Feats  ###########
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Male")
NonMot = read.csv('NonMotor_Categorized_Male.csv')
NonMot1 = NonMot [,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv", number = 10, savePredictions = T, classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf', trControl = control)
x = evalm(fit.cv,cols = "blue", gnames = '', fsize = 18, rlinethick = 1.5, plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - Male"))))

##########  Top 8 Feats  ###########
NonMot = read.csv('Any2Common_inAll45Feats_Data_Male.csv')
NonMot1 = NonMot [,-c(1)]
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

control <- trainControl(method = "repeatedcv", number = 10, savePredictions = T, classProbs = T)
fit.cv <- train( APPRDX~., data = NonMot1, method = 'rf', trControl = control)
x = evalm(fit.cv,cols = "blue", gnames = '', fsize = 18, rlinethick = 1.5, plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 8 Common Features - Male"))))

