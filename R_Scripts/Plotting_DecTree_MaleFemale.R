########## 1. Male ##########
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Male")
options(scipen = 999) #Viewing output values as decimal values

Non_Mot_All = read.csv("NonMotor_Categorized_Male.csv",header = T)
Non_Mot = Non_Mot_All [,-c(1)] # Removing PATNO
Non_Mot$APPRDX = as.factor(Non_Mot$APPRDX)

set.seed(1)
train <- createDataPartition(Non_Mot [,"APPRDX"], p=0.8, list = F)
data_train <- Non_Mot[train,]
data_test <- Non_Mot[-train,]

#Decision Tree 
# DecTree <- train(APPRDX ~ ., data_train, method="rpart", trControl = trainControl(method = "cv"))
# Feats_DecTree <- varImp(DecTree)

decTreMod <- rpart(APPRDX~., data_train, method = "class") #plotting tree
vari <- decTreMod$variable.importance 
#Visualizing tree
rpart.plot(decTreMod, type = 3, extra = 102, main="Decision Tree of All Features - Male", tweak = 1.6) 

########  Common Features  ########
Non_Mot_All = read.csv("Any2Common_inAll45Feats_Data_Male.csv",header = T)
Non_Mot = Non_Mot_All [,-c(1)] # Removing PATNO
Non_Mot$APPRDX = as.factor(Non_Mot$APPRDX)

set.seed(32) 
train <- createDataPartition(Non_Mot [,"APPRDX"], p=0.8, list = F)
data_train <- Non_Mot[train,]
data_test <- Non_Mot[-train,]

decTreMod <- rpart(APPRDX~., data_train, method = "class") #plotting tree
vari <- decTreMod$variable.importance 
#Visualizing tree
rpart.plot(decTreMod, type = 3, extra = 102, main="Decision Tree of Common 8 Features - Male", tweak = 1.6)






########## 2. Female ##########
setwd("Z:\\PPMI_Data\\Excels\\NonMotors\\Derived250\\Female")

Non_Mot_All = read.csv("NonMotor_Categorized_Female.csv",header = T)
Non_Mot = Non_Mot_All [,-c(1)] # Removing PATNO
Non_Mot$APPRDX = as.factor(Non_Mot$APPRDX)

set.seed(1)
train <- createDataPartition(Non_Mot [,"APPRDX"], p=0.8, list = F)
data_train <- Non_Mot[train,]
data_test <- Non_Mot[-train,]

decTreMod <- rpart(APPRDX~., data_train, method = "class") #plotting tree
vari <- decTreMod$variable.importance 
#Visualizing tree
rpart.plot(decTreMod, type = 3, extra = 102, main="Decision Tree of All Features - Female", tweak = 1.6) 


########  Common Features  ########
Non_Mot_All = read.csv("Any2Common_inAll45Feats_Data_Female.csv",header = T)
Non_Mot = Non_Mot_All [,-c(1)] # Removing PATNO
Non_Mot$APPRDX = as.factor(Non_Mot$APPRDX)

set.seed(2) 
train <- createDataPartition(Non_Mot [,"APPRDX"], p=0.8, list = F)
data_train <- Non_Mot[train,]
data_test <- Non_Mot[-train,]

decTreMod <- rpart(APPRDX~., data_train, method = "class") #plotting tree
vari <- decTreMod$variable.importance 
#Visualizing tree
rpart.plot(decTreMod, type = 3, extra = 102, main="Decision Tree of Common 8 Features - Female", tweak = 1.6)
