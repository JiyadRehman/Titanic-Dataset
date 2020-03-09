#Loading Data ----------------------------

library(readr)

library(ggplot2)

train <- read_csv("D:/MASON/Spring 2019/OR 568/Assignments/Assignment 4/train.csv")
test <- read_csv("D:/MASON/Spring 2019/OR 568/Assignments/Assignment 4/test.csv")

#Loading Data ----------------------------


#Data Preparation

datatrain <- train
datatest <- test

summary(train)

# Drop PassengerID and Ticket column
train$PassengerId <- NULL
train$Ticket <- NULL

# check for Null values

length(which(is.na(train$Survived))) # 0
length(which(is.na(train$Pclass)))   #0
length(which(is.na(train$Name)))     #0
length(which(is.na(train$Sex)))      #0
length(which(is.na(train$Age)))      #177
length(which(is.na(train$SibSp)))    #0
length(which(is.na(train$Parch)))    #0
length(which(is.na(train$Fare)))     #0
length(which(is.na(train$Cabin)))    #687
length(which(is.na(train$Embarked))) #2

# cabin has alot of Null values it will create issue so lets drop it

train$Cabin <- NULL

# From Name we extracted titles

train$Title <- gsub('(.*, )|(\\..*)','',train$Name)

# Making dataset

train <- as.data.frame(train)

train$Survived <- as.factor(train$Survived)
train$Pclass <- as.factor(train$Pclass)
train$Sex <- as.factor(train$Sex)
train$Embarked <- as.factor(train$Embarked)
train$Title <- as.factor(train$Title)


# Now Data imputation for Age and Embarked is required

# Working on Age -----------------------------------------------------

temp <- train[(which(is.na(train$Age))),]
nrow(temp[(which(temp$Title == "Mr")),])    #119
nrow(temp[(which(temp$Title == "Mrs")),])   #17
nrow(temp[(which(temp$Title == "Miss")),])  #36
# so most are covered under these categories so we can have these filled with there medians
# rest can be filled with overall median

trainmed <- median(train$Age, na.rm = TRUE)  # overall median

tempmiss <- train[(which(train$Title == "Miss")),]
tempmiss <- median(tempmiss$Age, na.rm = TRUE)       #median for Miss

tempmr <- train[(which(train$Title == "Mr")),]
tempmr <- median(tempmr$Age, na.rm = TRUE)           #median for Mr

tempmrs <- train[(which(train$Title == "Mrs")),]
tempmrs <- median(tempmrs$Age, na.rm = TRUE)         #median for Mrs


for(i in 1:nrow(train))
{
  if(train$Title[i] == 'Mr' && is.na(train$Age[i])){
    train$Age[i] = tempmr
  }else if(train$Title[i] == 'Mrs' && is.na(train$Age[i])) {
    train$Age[i] = tempmrs
  } else if(train$Title[i] == 'Miss' && is.na(train$Age[i])) {
    train$Age[i] = tempmiss
  } else if(train$Title[i] != 'Miss' && train$Title[i] != 'Mrs'  && train$Title[i] != 'Mr'  && is.na(train$Age[i])) {
    train$Age[i] = trainmed
  }    else
    train$Age[i] = train$Age[i]
}


rm(trainmed,tempmrs,tempmr,tempmiss, temp, i)


# Working on Embarked -----------------------------------------------------

nrow(train[(which(train$Embarked == "S")),])  # 664
nrow(train[(which(train$Embarked == "Q")),])  #77
nrow(train[(which(train$Embarked == "C")),])  #168

# so lets fill the null values with mode

train$Embarked[is.na(train$Embarked)] <- "S" # Filled null values with S


# Now removing columns that i believe not useful

# train$Title <- NULL  # Lets keep this and make 5 categories
train$Name <- NULL

# Preparing Title column according to our need -------------------------------------

# train$Title <- NULL  # Lets keep this and make 5 categories

length(which(train$Title == 'Mr')) # 517
length(which(train$Title == 'Mrs')) # 125
length(which(train$Title == 'Miss')) # 182
length(which(train$Title == 'Master')) # 40

train$test <- 'x'
# convert Title into a string

for(i in 1:nrow(train)){
  if(train$Title[i] == 'Mr' | train$Title[i] == 'Mrs'| train$Title[i] == 'Miss'| train$Title[i] == 'Master')
  {
    train$test[i] <- as.character(train$Title[i])
  } else 
    train$test[i] <- 'Other'
  
}

train$Title <- train$test
train$test <- NULL

typeof(train$temp)
train$Title <- as.factor(train$Title)

# Family Size

train$temp <- 'X'
train$temp <- train$SibSp + train$Parch
train$temp <- as.factor(train$temp)

train$Alone <- ifelse(train$temp == 0, 0, 1)

colnames(train)[colnames(train)=="temp"] <- "Family"



#----- PREPARING TEST DATASET ---------------------------------------------

# Drop PassengerID and Ticket column
test$PassengerId <- NULL
test$Ticket <- NULL

# check for Null values

length(which(is.na(test$Pclass)))   #0
length(which(is.na(test$Name)))     #0
length(which(is.na(test$Sex)))      #0
length(which(is.na(test$Age)))      #86
length(which(is.na(test$SibSp)))    #0
length(which(is.na(test$Parch)))    #0
length(which(is.na(test$Fare)))     #1
length(which(is.na(test$Cabin)))    #327
length(which(is.na(test$Embarked))) #0

# cabin has alot of Null values it will create issue so lets drop it

test$Cabin <- NULL

# From Name we extracted titles

test$Title <- gsub('(.*, )|(\\..*)','',test$Name)

# Making dataset

test <- as.data.frame(test)

test$Survived <- as.factor(test$Survived)
test$Pclass <- as.factor(test$Pclass)
test$Sex <- as.factor(test$Sex)
test$Embarked <- as.factor(test$Embarked)
test$Title <- as.factor(test$Title)


# Now Data imputation for Age and Embarked is required

# Working on Age -----------------------------------------------------

temp <- test[(which(is.na(test$Age))),]
nrow(temp[(which(temp$Title == "Mr")),])    #57
nrow(temp[(which(temp$Title == "Mrs")),])   #10
nrow(temp[(which(temp$Title == "Miss")),])  #14
# so most are covered under these categories so we can have these filled with there medians
# rest can be filled with overall median

trainmed <- median(test$Age, na.rm = TRUE)  # overall median

tempmiss <- test[(which(test$Title == "Miss")),]
tempmiss <- median(tempmiss$Age, na.rm = TRUE)       #median for Miss

tempmr <- test[(which(test$Title == "Mr")),]
tempmr <- median(tempmr$Age, na.rm = TRUE)           #median for Mr

tempmrs <- test[(which(test$Title == "Mrs")),]
tempmrs <- median(tempmrs$Age, na.rm = TRUE)         #median for Mrs


for(i in 1:nrow(test))
{
  if(test$Title[i] == 'Mr' && is.na(test$Age[i])){
    test$Age[i] = tempmr
  }else if(test$Title[i] == 'Mrs' && is.na(test$Age[i])) {
    test$Age[i] = tempmrs
  } else if(test$Title[i] == 'Miss' && is.na(test$Age[i])) {
    test$Age[i] = tempmiss
  } else if(test$Title[i] != 'Miss' && test$Title[i] != 'Mrs'  && test$Title[i] != 'Mr'  && is.na(test$Age[i])) {
    test$Age[i] = trainmed
  }    else
    test$Age[i] = test$Age[i]
}


rm(trainmed,tempmrs,tempmr,tempmiss, temp, i)


# Working on Embarked -----------------------------------------------------

nrow(test[(which(test$Embarked == "S")),])  # 270
nrow(test[(which(test$Embarked == "Q")),])  #46
nrow(test[(which(test$Embarked == "C")),])  #102

# so lets fill the null values with mode

test$Embarked[is.na(test$Embarked)] <- "S" # Filled null values with S


# Now removing columns that i believe not useful

# train$Title <- NULL  # Lets keep this and make 5 categories
test$Name <- NULL

# Preparing Title column according to our need -------------------------------------

# train$Title <- NULL  # Lets keep this and make 5 categories

length(which(test$Title == 'Mr')) # 240
length(which(test$Title == 'Mrs')) # 72
length(which(test$Title == 'Miss')) # 78
length(which(test$Title == 'Master')) # 21

test$test <- 'x'
# convert Title into a string

for(i in 1:nrow(test)){
  if(test$Title[i] == 'Mr' | test$Title[i] == 'Mrs'| test$Title[i] == 'Miss'| test$Title[i] == 'Master')
  {
    test$test[i] <- as.character(test$Title[i])
  } else 
    test$test[i] <- 'Other'
  
}

test$Title <- test$test
test$test <- NULL

typeof(test$temp)
test$Title <- as.factor(test$Title)

# Family Size

test$temp <- 'X'
test$temp <- test$SibSp + test$Parch
test$temp <- as.factor(test$temp)

test$Alone <- ifelse(test$temp == 0, 0, 1)

colnames(test)[colnames(test)=="temp"] <- "Family"

#*********************************************************************

# Working on Fare -----------------------------------------------------

test[(which(is.na(test$Fare))),] #Pclass is 3 for null fare so we will impute median of it

temp <- test[(which(test$Pclass == "3")),]
temp <- median(temp$Fare, na.rm = TRUE)  

test$Fare[is.na(test$Fare)] <- temp # Filled null values with S

rm(temp)

# Now removing columns that i believe not useful

test$Name <- NULL



# *************************************************************************


# RANDOM FOREST

library(randomForest)

typeof(train$Sex)
typeof(test$Sex)

rf <- randomForest(train[,c(-1)], as.factor(train[,1]), ntree = 300)

varImpPlot(rf) # based on on this we choose only imp variables

#set.seed(22)
#rf <- randomForest(train[,c(2,3,4,7,9)], as.factor(train[,1]), ntree = 300)



df <- data.frame(PID = datatest$PassengerId)

#df$Sur <- predict(rf,test[,c(1,2,3,6,8)]) # this the file that will be submitted

df$Sur <- predict(rf,test) # this the file that will be submitted

write.csv(df,'TitanicSub3RF.csv')

# GRADIENT BOOSTING


library(caret)

objControl <- trainControl(method='cv', number=3, returnResamp='none', summaryFunction = twoClassSummary, classProbs = TRUE)

train2 <- train

train2$Survived <- ifelse(train2$Survived==1,'yes','no')
train2$Survived <- as.factor(train2$Survived)

#i <- c(2,3,4,7,9)

i<- -1

objModel <- train(train2[,i], train2[,1], 
                  method='gbm', 
                  trControl=objControl,  
                  metric = "ROC",
                  preProc = c("center", "scale"))

#predictions <- predict(object=objModel, test[,c(1,2,3,6,8)], type='raw')

predictions <- predict(object=objModel, test, type='raw')


df2 <- data.frame(PID = datatest$PassengerId)

df2$Sur <- predictions

df2$Sur <- ifelse(df2$Sur == 'yes', 1, 0) # Final for submission

write.csv(df2,'TitanicGradientBoosting.csv')


#REMOVE FROM HERE *****************************************************
#predictions2 <- predict(object=objModel, train2[,-1], type='raw')

#df$Sur4 <- predictions
#df$Sur4 <- ifelse(df$Sur4 == 'yes', 1, 0) # CHK
#df$sur5 <- ifelse(df$sur2 == df$Sur4,1,0)

#length(which(df$sur5 == 0))  
#REMOVE TILL HERE *****************************************************



# Data frame for train
chk <- data.frame(train2$Survived, predictions2)

chk$true <- ifelse(chk$train2.Survived == chk$predictions2, 1, 0)

length(which(chk$true == 1))


# SVM

library(e1071)

#svmfit=svm(Survived~Title+Fare+Sex+Age+Pclass, data=train2, kernel="linear", cost=10,scale=FALSE)

svmfit=svm(Survived~., data=train2, kernel="linear", cost=10,scale=FALSE)


summary(svmfit)

#ypred=predict(svmfit,test[c(-4,-5,-7,-9,-10)])

ypred=predict(svmfit,test)


df3 <- data.frame(PID = datatest$PassengerId)

df3$svm <- ypred

df3$svm <- ifelse(df3$svm == 'yes', 1, 0) # Final for submission

write.csv(df3,'TitanicSVM.csv')


# REMOVE FROM HERE ****************************************************
#df$sur6 <- df3$svm
#df$sur7 <- ifelse(df$sur2 == df$sur6,1,0)

#length(which(df$sur7 == 0))  
# REMOVE TILL HERE ****************************************************


#--------------------------------------------------------------------------

#---------------------------------------------------------------------------

#FURTHER WORKING FOR RANDOM FOREST


chk <- data.frame(sex = train$Sex, title = train$Title, fare = train$Fare, pclass = train$Pclass)

chk$sex <- ifelse(chk$sex == 'male', 0, 1)

chk$title1 <- ifelse(chk$title == 'Mr',1, ifelse(chk$title=='Mrs', 4, ifelse(chk$title=='Miss',5,ifelse(chk$title=='Master',2,3))))

chk$title <- NULL

chk$pclass <- as.numeric(chk$pclass)

cor(chk)

# Based on corplots it can be identified that columns are correlated so we dropped on of them

rf <- randomForest(train[,c(-1,-7,-3)], as.factor(train[,1]), ntree = 300)
rf
varImpPlot(rf) # based on on this we choose only imp variables

#set.seed(22)
#rf <- randomForest(train[,c(2,3,4,7,9)], as.factor(train[,1]), ntree = 300)


df <- data.frame(PID = datatest$PassengerId)

df$Sur <- predict(rf,test[,c(-2,-6)]) # this the file that will be submitted

#df$Sur <- predict(rf,test) # this the file that will be submitted


write.csv(df,'TitanicRandomForest.csv')
