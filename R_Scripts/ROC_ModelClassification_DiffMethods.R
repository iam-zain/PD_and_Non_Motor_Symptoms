setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")

###############  Logistic Regression  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'
control <- trainControl(method="cv",number=10,savePredictions="all",classProbs=TRUE)
  
  log_model <- train(APPRDX ~., data=NonMot1, method="glm", family=binomial, trControl=control)
  x = evalm(log_model,cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
            title = substitute(paste(bold("AUC-ROC curve: 45 Features - LogReg"))))



###############  Random Forest  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'
control <- trainControl(method="cv",number=10,savePredictions="all",classProbs=TRUE)
rf_model <- train(APPRDX ~., data=train_data, method="rf", trControl=control)
x = evalm(rf_model,cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - RanFor"))))

  

###############  glmnet / Elastic Net  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

enet_hits_int = train(APPRDX ~ . ^ 2, data = train_data,method = "glmnet",trControl = control,tuneLength = 10)
x = evalm(enet_hits_int, cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - glmnet"))))



###############  Ada Boost  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

fitGrid <- expand.grid(mfinal = (1:3)*3, maxdepth = c(1, 3), coeflearn = c("Breiman"))
ada_model <- train(APPRDX ~.,data = train_data, method ="AdaBoost.M1", trControl = control, tuneGrid = fitGrid, verbose = TRUE)
x = evalm(ada_model, cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
            title = substitute(paste(bold("AUC-ROC curve: 45 Features - AdaBoost"))))
  


###############  XG Boost  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

fitGrid <- expand.grid(nrounds = 200,max_depth = 5,eta = 0.05,gamma = 0.01,
                         colsample_bytree = 0.75,min_child_weight = 0,subsample = 0.5)
XG_model <- train(APPRDX ~.,data = train_data, method ="xgbTree", trControl = control, tuneGrid = fitGrid, verbose = TRUE)
x = evalm(XG_model, cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - XGBoost"))))




###############  Neural Network  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

NeuralNet <- train(APPRDX ~.,data = train_data, method ="nnet", trControl = control, na.action = na.omit, trace = F)
x = evalm(NeuralNet, cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - NeuralNet"))))



###############  LDA  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

LDA_model <- train(APPRDX ~.,data = train_data, method ="lda", trControl = control, metric = 'Accuracy')
x = evalm(LDA_model, cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - LDA"))))




###############  knn  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'

knn_model = train(APPRDX ~ ., data = train_data, method = "knn", trControl = control, 
                    preProcess = c("center", "scale"), tuneLength = 10)
x = evalm(knn_model, cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - knn"))))




###############  svm  ##############
NonMot1 = read.csv('NonMotor_SocioBehavior_Categorized_Edit.csv')
NonMot1$APPRDX[NonMot1$APPRDX == 1] <- 'Patient'
NonMot1$APPRDX[NonMot1$APPRDX == 2] <- 'Control'

svm_model <- train(APPRDX ~.,data = train_data, method ="svmLinear", trControl = control)
x = evalm(svm_model, cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - svmLinear"))))


### radial ###
svm_radial <- train(APPRDX ~.,data = train_data, method ="svmRadial", trControl = control)
x = evalm(svm_radial, cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - svmRadial"))))

### poly ###
svm_poly <- train(APPRDX ~.,data = train_data, method ="svmPoly", trControl = control)
x = evalm(svm_poly, cols = "blue",gnames = '', fsize = 18,rlinethick = 1.5,plots = c("r"),
          title = substitute(paste(bold("AUC-ROC curve: 45 Features - svmPoly"))))

