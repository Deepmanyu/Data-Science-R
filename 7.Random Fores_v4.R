##################################################################
##################################################################
###########Bagging Example

#Importing Boston  house pricing data. 
library(MASS) 
data(Boston)
head(Boston)
dim(Boston)
#Boston <- read.csv("/Datasets/Housing/Boston.csv")

##Training and holdout sample
library(caret)
set.seed(200)
sampleseed <- createDataPartition(Boston$medv, p=0.9, list=FALSE)

train_boston<-Boston[sampleseed,]
test_boston<-Boston[-sampleseed,]

dim(train_boston)
dim(test_boston)

###Regression Model
reg_model<- lm(medv ~ ., data=train_boston)
summary(reg_model)

###Accuracy testing on holdout data
pred_reg<-predict(reg_model, newdata=test_boston[,-14])
reg_err<-sum((test_boston$medv-pred_reg)^2)
reg_err

###Bagging Ensemble Model
library(ipred)
bagg_model<- bagging(medv ~ ., data=train_boston , nbagg=30)

###Accuracy testing on holout data
pred_bagg<-predict(bagg_model, newdata=test_boston[,-14])
bgg_err<-sum((test_boston$medv-pred_bagg)^2)
bgg_err


###Overall Improvement
reg_err
bgg_err
(reg_err-bgg_err)/reg_err

#############################################################################
#############################################################################
###########Random Forest Example
#Data Import
train<- read.csv("./Car Accidents IOT/Train.csv")
test<- read.csv("./Car Accidents IOT/Test.csv")


###Decision Tree

library(rpart)
crash_model_ds<-rpart(Fatal ~ ., method="class", control=rpart.control(minsplit=30, cp=0.03),   data=train)

#Training accuarcy
predicted_y<-predict(crash_model_ds, type="class")
table(predicted_y)
confusionMatrix(predicted_y,factor(train$Fatal))

#Accuaracy on Test data
predicted_test_ds<-predict(crash_model_ds, test, type="class")
confusionMatrix(predicted_test_ds,factor(test$Fatal))


###Random Forest
library(randomForest)

rf_model <- randomForest(as.factor(train$Fatal) ~ ., ntree=200,   mtry=ncol(train)/3, data=train)



#Training accuaracy
predicted_y<-predict(rf_model)
table(predicted_y)
confusionMatrix(predicted_y,factor(train$Fatal))

#Accuaracy on Test data
predicted_test_rf<-predict(rf_model,test, type="class")
confusionMatrix(predicted_test_rf,factor(test$Fatal))

#Variable Importance
varImp(rf_model)
varImpPlot(rf_model)

#Variable Importance
partialPlot(rf_model, pred.data=train,x.var=S22)
partialPlot(rf_model, pred.data=train,x.var=S14)
partialPlot(rf_model, pred.data=train,x.var=S3)


###############################################################
#######Market  Response Model
#######Complete Project on Raw Data
###############################################################

#train_all <- read.csv("D:/3. Big Data D/2.BigDataSets/Springleaf Data/train.csv",  stringsAsFactors=FALSE )
#dim(train_all)

#The below statement loads training data faster
load("D:/Google Drive/Training/Datasets/Spring Leaf Marketing Response Data/Spring_leaf_market_response.RData")
dim(train_all)

train <- train_all[sample(1:nrow(train_all),nrow(train_all)/3, replace=F, set.seed(155)), ]
dim(train)

test <- train_all[sample(1:nrow(train_all),nrow(train_all)/3, replace=F, set.seed(175)), ]
dim(test)

#Response Variable freq and proportion
table(train$target)
table(test$target)

table(train$target)/nrow(train)
table(test$target)/nrow(test)

#Take all the variables in one vector
variable_names <- names(train)[2:(ncol(train)-1)]
variable_names

# If there are any charecter variables, we will convert them to factors 
# For fatser computation  and easy interpratation 

for (f in variable_names) {
  if (class(train[[f]])=="character") {
    levels <- unique(c(train[[f]], test[[f]]))
    train[[f]] <- as.integer(factor(train[[f]], levels=levels))
    test[[f]]  <- as.integer(factor(test[[f]],  levels=levels))
  }
}

#Are there any missing values?
sum(is.na(train))
sum(is.na(test))

#Missing values for each variable
i=1
Miss_val_by_var <- data.frame(var_name=0, miss_val_count=0)

for (f in variable_names) {
  Miss_val_by_var[i,1]= f
  Miss_val_by_var[i,2]= sum(is.na(train[[f]]))
  i=i+1
}

Miss_val_by_var[order(-Miss_val_by_var$miss_val_count),]
head(Miss_val_by_var[order(-Miss_val_by_var$miss_val_count),], n=20)

#Verification
sum(is.na(train))
sum(Miss_val_by_var$miss_val_count)

#Percentiles of missing value 
quantile(Miss_val_by_var$miss_val_count, c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8, 0.9, 1)) 
quantile(Miss_val_by_var$miss_val_count, c(0, 0.1, 0.25,  0.5, 0.75, 0.8, 0.9,0.93, 0.95, 0.97, 0.98, 0.99, 1)) 

#Percentage missing vs divide by nrow to know the percentage missing 
quantile(Miss_val_by_var$miss_val_count, c(0, 0.1, 0.25,  0.5, 0.75, 0.8, 0.9,0.93, 0.95, 0.97, 0.98, 0.99, 1))/nrow(train) 
#round off
round(quantile(Miss_val_by_var$miss_val_count, c(0, 0.1, 0.25,  0.5, 0.75, 0.8, 0.9,0.93, 0.95, 0.97, 0.98, 0.99,0.993,0.995,0.997,0.998, 0.999, 1))/nrow(train),2) 

# All missing values treatment in one go. Replace with -1
# You should spend  more time on missing value treatment
train[is.na(train)] <- -1
test[is.na(test)] <- -1

#Are there any missing values?
sum(is.na(train))
sum(is.na(test))

########Building RF model
library(randomForest)
#rf_m1 <- randomForest(train[,variable_names], factor(train$target), ntree=10, sampsize=5000,  nodesize=200)
#rf_m1 <- randomForest(train[,variable_names], factor(train$target), ntree=15, sampsize=5000,  nodesize=200)
#rf_m1 <- randomForest(train[,variable_names], factor(train$target), ntree=15, sampsize=5000,  nodesize=100)

rf_m1 <- randomForest(train[,variable_names], factor(train$target), ntree=20, sampsize=5000,  replace=F, nodesize=50)

##Confusion Matrix and Accuracy
###Training data
predicted_rf_m1_class<-predict(rf_m1, data.matrix(train[ ,variable_names]))
#predicted_rf_m1_class
library(caret)
conf_matrix_rf<-confusionMatrix(predicted_rf_m1_class,factor(train$target))
conf_matrix_rf

###############Testing results
predicted_rf_m1_test_class<-predict(rf_m1, data.matrix(test[ ,variable_names]))
#predicted_rf_m1_test_class

conf_matrix_rf_test<-confusionMatrix(predicted_rf_m1_test_class,factor(test$target))
conf_matrix_rf_test

###################################################
#### Model-2 for increasing specificity 
### Try Changing the class weight for class imbalance, it works well

rf_m2 <- randomForest(train[,variable_names], factor(train$target), classwt=c(0.3,0.7),ntree=20, sampsize=5000,  replace=F, nodesize=50)

##Confusion Matrix and Accuracy
###Training data
predicted_rf_m2_class<-predict(rf_m2, data.matrix(train[ ,variable_names]))
#predicted_rf_m1_class

conf_matrix_rf<-confusionMatrix(predicted_rf_m2_class,factor(train$target))
conf_matrix_rf

###############Testing results
predicted_rf_m2_test_class<-predict(rf_m2, data.matrix(test[ ,variable_names]))
#predicted_rf_m1_test_class

conf_matrix_rf_test<-confusionMatrix(predicted_rf_m2_test_class,factor(test$target))
conf_matrix_rf_test
