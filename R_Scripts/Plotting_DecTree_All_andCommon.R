########## 1. All ##########
setwd ("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250")
options(scipen = 999) #Viewing output values as decimal values

Non_Mot_All = read.csv("NonMotor_SocioBehavior_Categorized_Edit.csv",header = T)
Non_Mot = Non_Mot_All [,-c(1)] # Removing PATNO
Non_Mot$APPRDX = as.factor(Non_Mot$APPRDX)

set.seed(2)
train <- createDataPartition(Non_Mot [,"APPRDX"], p=0.8, list = F)
data_train <- Non_Mot[train,]
data_test <- Non_Mot[-train,]

#Decision Tree 
# DecTree <- train(APPRDX ~ ., data_train, method="rpart", trControl = trainControl(method = "cv"))
# Feats_DecTree <- varImp(DecTree)

decTreMod <- rpart(APPRDX~., data_train, method = "class") #plotting tree
vari <- decTreMod$variable.importance 
#Visualizing tree
rpart.plot(decTreMod, type = 3, extra = 102, main="Decision Tree of All Features", tweak = 1.6)



########## 2. Common 12 from Common 4 Analysis ##########

Non_Mot_All = read.csv("Any2Common_inAll45Feats_Data.csv",header = T)
Non_Mot = Non_Mot_All [,-c(1)] # Removing PATNO
Non_Mot$APPRDX = as.factor(Non_Mot$APPRDX)

set.seed(11451) 
train <- createDataPartition(Non_Mot [,"APPRDX"], p=0.8, list = F)
data_train <- Non_Mot[train,]
data_test <- Non_Mot[-train,]

#Decision Tree 
# DecTree <- train(APPRDX ~ ., data_train, method="rpart", trControl = trainControl(method = "cv"))
# Feats_DecTree <- varImp(DecTree)

decTreMod <- rpart(APPRDX~., data_train, method = "class") #plotting tree
vari <- decTreMod$variable.importance 
#Visualizing tree
rpart.plot(decTreMod, type = 3, extra = 102, main="Decision Tree of Common 12 Features", tweak = 1.6)


