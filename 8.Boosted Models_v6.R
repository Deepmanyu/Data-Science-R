#########################################################
#########################################################
###Boosting
######################################################### 
#########################################################

#########################################################
###GBM Parameters
######################################################### 


library(gbm)
############################################
#interaction.depth = 1
############################################
train <- read.csv("./BParameters Data/Ecom_Cust_Relationship_Management/Ecom_Cust_Survey.csv")


#Lets try interaction.depth =1,2,3,4 with one tree
gm<-gbm(Overall_Satisfaction~Region+Age+Order.Quantity+Customer_Type+Improvement.Area, data=train,  
        n.trees = 1,interaction.depth = 1)
summary(gm)

gm<-gbm(Overall_Satisfaction~Region+Age+Order.Quantity+Customer_Type+Improvement.Area, data=train, 
        n.trees = 1,interaction.depth = 2)
summary(gm)

gm<-gbm(Overall_Satisfaction~Region+Age+Order.Quantity+Customer_Type+Improvement.Area, data=train, 
        n.trees = 1,interaction.depth = 3)
summary(gm)


gm<-gbm(Overall_Satisfaction~Region+Age+Order.Quantity+Customer_Type+Improvement.Area, data=train, 
        n.trees = 1,interaction.depth = 4)
summary(gm)


################################################################
########shrinkage 

train <- read.csv("./BParameters Data/Shrinkage/Shrink.csv")
###80% traiing and rest 20% testing 
sample_index<-sample(1:nrow(train),nrow(train)*0.8)
train1 <- train[sample_index, ]
test1<- train[-sample_index, ]

#Shrinkage Example
n=1000
gm<-gbm(Buy~., data=train1,  
        distribution="bernoulli", 
        verbose=T, 
        interaction.depth = 2 , 
        n.trees = n,
        n.minobsinnode=5,
        bag.fraction=1,set.seed(125),
        shrinkage = 1)

#Accuracy on Training and test data
conf_matrix<-confusionMatrix(factor(ifelse(predict(gm,  n.trees = n, type="response")<0.2,0,1)),factor(train1$Buy))
conf_matrix$overall[1]
conf_matrix$byClass[2]

conf_matrix<-confusionMatrix(factor(ifelse(predict(gm,  n.trees = n, newdata=test1["Age"], type="response")<0.2,0,1)),factor(test1$Buy))
conf_matrix$overall[1]
conf_matrix$byClass[2]

######### Shrinkage Finetuning

shrink_ntree <-data.frame()
shrink_ntree1 <-data.frame()
n=0
s=0
j=1
i=1

for(n in c(10,50, 100, 300, 500, 700, 1000)){
  for(s in c(0.001,0.002, 0.005, 0.01, 0.05, 0.07, 0.1, 0.3, 0.6, 0.8, 1)){
    gm<-gbm(Buy~., data=train1,  
            distribution="bernoulli", 
            verbose=T, 
            interaction.depth = 2 , 
            n.trees = n,
            n.minobsinnode=5,
            bag.fraction=1,set.seed(125),
            shrinkage = s)
    
    #Accuracy on Training and test data
    conf_matrix<-confusionMatrix(factor(ifelse(predict(gm,  n.trees = n, type="response")<0.2,0,1)),factor(train1$Buy))
    shrink_ntree[i,j]=conf_matrix$byClass[2]
    
    #conf_matrix<-confusionMatrix(ifelse(predict(gm,  n.trees = n, newdata=test1["Age"], type="response")<0.2,0,1),test1$Buy)
    #shrink_ntree1[i,j]=conf_matrix$byClass[2]
    i=i+1
  }
  i=1
  j=j+1
}

names(shrink_ntree)<-c(10,50, 100, 300, 500, 700, 1000)
row.names(shrink_ntree)<-c(0.001,0.002, 0.005, 0.01, 0.05, 0.07, 0.1, 0.3, 0.6, 0.8, 1)
shrink_ntree

##########################################
## bag.fraction
train <- read.csv("./BParameters Data/Ecom_Cust_Relationship_Management/Ecom_Cust_Survey.csv")

###80% traiing and rest 20% testing 
sample_index<-sample(1:nrow(train),nrow(train)*0.8)
train1 <- train[sample_index, ]
test1<- train[-sample_index, ]

n=1000
gm<-gbm(Overall_Satisfaction~., data=train1,  
        distribution="bernoulli", 
        verbose=T, 
        interaction.depth = 2 , 
        n.trees = n,
        n.minobsinnode=5,
        bag.fraction=0.6,set.seed(125),
        shrinkage = 0.1)

#Accuracy on Training and test data
conf_matrix<-confusionMatrix(factor(ifelse(predict(gm,  n.trees = n, type="response")<0.5,0,1)),factor(train1$Overall_Satisfaction))
conf_matrix$overall[1]

##########################################
##train.fraction

train <- read.csv("./BParameters Data/Ecom_Cust_Relationship_Management/Ecom_Cust_Survey.csv")

###80% traiing and rest 20% testing 
sample_index<-sample(1:nrow(train),nrow(train)*0.8)
train1 <- train[sample_index, ]
test1<- train[-sample_index, ]

n=1000
gm<-gbm(Overall_Satisfaction~., data=train1,  
        distribution="bernoulli", 
        verbose=T, 
        interaction.depth = 2 , 
        n.trees = n,
        n.minobsinnode=5,
        bag.fraction=1,set.seed(125),
        shrinkage = 0.1,
        train.fraction=0.5)

#Accuracy on Training and test data
conf_matrix<-confusionMatrix(factor(ifelse(predict(gm,  n.trees = n, type="response")<0.5,0,1)),factor(train1$Overall_Satisfaction))
conf_matrix$overall[1]

#############
#If data is sorted then train.fraction is an issue. Try building same model for ordered data
train1<-train[order(train$Overall_Satisfaction),]

n=1000
gm<-gbm(Overall_Satisfaction~., data=train1,  
        distribution="bernoulli", 
        verbose=T, 
        interaction.depth = 2 , 
        n.trees = n,
        n.minobsinnode=5,
        bag.fraction=1,set.seed(125),
        shrinkage = 0.1,
        train.fraction=0.5)

############################################

train <- read.csv("./BParameters Data/Ecom_Cust_Relationship_Management/Ecom_Cust_Survey.csv")
###80% traiing and rest 20% testing 
sample_index<-sample(1:nrow(train),nrow(train)*0.8)
train1 <- train[sample_index, ]
test1<- train[-sample_index, ]

n=300
gm<-gbm(Overall_Satisfaction~., data=train1,  
        distribution="bernoulli", 
        verbose=T, 
        interaction.depth = 2 , 
        n.trees = n,
        n.minobsinnode=5,
        bag.fraction=0.5,set.seed(125),
        shrinkage = 0.1,
        train.fraction=0.5)

#Accuracy on Training and test data
conf_matrix<-confusionMatrix(factor(ifelse(predict(gm,  n.trees = n, type="response")<0.5,0,1)),factor(train1$Overall_Satisfaction))
conf_matrix$overall[1]

############Finding best number of iterations

# check performance using an out-of-bag estimator(from bag.fraction)
# OOB underestimates the optimal number of iterations
best.iter <- gbm.perf(gm,method="OOB")
print(best.iter)

# check performance using a 50% heldout test set (from train.fraction)
best.iter <- gbm.perf(gm,method="test")
print(best.iter)

# plot the performance # plot variable influence
summary(gm,n.trees=1)         # based on the first tree
summary(gm,n.trees=best.iter) # based on the estimated best number of trees

##################################################################
##################GBM Models

Credit_risk_data <- read.csv("D:/Google Drive/Training/Datasets/Credit Risk Data/Credit_risk_data.csv")

#Response variable
table(Credit_risk_data$SeriousDlqin2yrs)

#There is class imbalance, lets create a balanced dataset.
Credit_risk_data_1<-Credit_risk_data[Credit_risk_data$SeriousDlqin2yrs==0,]
dim(Credit_risk_data_1)

########Take a small percentage of zeros and all one
risk_20pct_zero<-Credit_risk_data_1[sample(1:nrow(Credit_risk_data_1),15000), ]
dim(risk_20pct_zero)

risk_All_one<-Credit_risk_data[Credit_risk_data$SeriousDlqin2yrs==1,]
dim(risk_All_one)

#Final  Balanced data
Credit_risk_data_bal<-rbind(risk_20pct_zero,risk_All_one)
names(Credit_risk_data_bal)
#Shuffle the 1 and 0
Credit_risk_data_bal<-Credit_risk_data_bal[sample(1:nrow(Credit_risk_data_bal),nrow(Credit_risk_data_bal)),]
dim(Credit_risk_data_bal)
table(Credit_risk_data_bal$SeriousDlqin2yrs)


n=400
gm<-gbm(SeriousDlqin2yrs~., data=Credit_risk_data_bal,  
        distribution="bernoulli", 
        verbose=T, 
        interaction.depth = 3, 
        n.trees = n,
        n.minobsinnode=5,
        bag.fraction=0.5,set.seed(125),
        shrinkage = 0.07,
        train.fraction=0.7)

#Accuracy on Training and test data
conf_matrix<-confusionMatrix(factor(ifelse(predict(gm,  n.trees = n, type="response")<0.5,0,1)),factor(Credit_risk_data_bal$SeriousDlqin2yrs))
conf_matrix
conf_matrix$overall[1]

# plot the performance # plot variable influence
summary(gm,n.trees=n) # based on the estimated best number of trees

###Partial dependence plots
names(Credit_risk_data_bal)
plot.gbm(gm,i.var =2 , lwd = 2, col = "red")
plot.gbm(gm,i.var =1 , lwd = 2, col = "red")


###############################################################
#######Market  Response Model
#######Complete Project on Raw Data
###############################################################
library(gbm)
library(data.table)
library(magrittr)

#train_all <- read.csv("D:/3. Big Data D/2.BigDataSets/Springleaf Data/train.csv", stringsAsFactors=FALSE )
#dim(train_all)

load("D:/Google Drive/Training/Datasets/Spring Leaf Marketing Response Data/Spring_leaf_market_response.RData")
dim(train_all)

train <- train_all[sample(1:nrow(train_all),nrow(train_all)/3, replace=F, set.seed(55)), ]
dim(train)

test <- train_all[sample(1:nrow(train_all),nrow(train_all)/3, replace=F, set.seed(75)), ]
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

##################################
###Boosting model
library(gbm)

n=100
gbm_m1<-gbm(target~., data=train[,-1],  
        distribution="bernoulli", 
        verbose=T, 
        interaction.depth = 2 , 
        n.trees = n,
        n.minobsinnode=5,
        bag.fraction=0.5,set.seed(125),
        shrinkage = 0.1,
        train.fraction = 0.5)


##Confusion Matrix and Accuracy
###Training data
predicted_gbm_m1<-predict(gbm_m1, n.trees=n)
predicted_gbm_m1_cl<- ifelse(predicted_gbm_m1>0.5,1,0)
conf_matrix_gbm_m1<-confusionMatrix(factor(predicted_gbm_m1_cl),factor(train$target))
conf_matrix_gbm_m1

###############Testing results
predicted_gbm_m1_test<-predict(gbm_m1, n.trees=n, test[ ,variable_names])
predicted_gbm_m1_test_class<- ifelse(predicted_gbm_m1_test>0.5,1,0)
conf_matrix_gbm_test<-confusionMatrix(factor(predicted_gbm_m1_test_class),factor(test$target))
conf_matrix_gbm_test

##################################
###Boosting model-2 with scale_pos_weight For Class imbalance 

#Create model weights first
table(train$target)/nrow(train)
model_weights <- ifelse(train$target ==0, 0.2,0.8)
table(model_weights)                   

n=300
gbm_m1<-gbm(target~., data=train[,-1],  
            distribution="bernoulli", 
            verbose=T, 
            interaction.depth = 2 , 
            n.trees = n,
            n.minobsinnode=5,
            bag.fraction=0.5,set.seed(125),
            shrinkage = 0.1,
            train.fraction = 0.5,
            weights = model_weights)

##Confusion Matrix and Accuracy
###Training data
predicted_gbm_m1<-predict(gbm_m1, n.trees=n)
predicted_gbm_m1_cl<- ifelse(predicted_gbm_m1>0.3,1,0)
conf_matrix_gbm_m1<-confusionMatrix(factor(predicted_gbm_m1_cl),factor(train$target))
conf_matrix_gbm_m1

###############Testing results
predicted_gbm_m1_test<-predict(gbm_m1, n.trees=n, test[ ,variable_names])
predicted_gbm_m1_test_class<- ifelse(predicted_gbm_m1_test>0.3,1,0)
conf_matrix_gbm_test<-confusionMatrix(factor(predicted_gbm_m1_test_class),factor(test$target))
conf_matrix_gbm_test
