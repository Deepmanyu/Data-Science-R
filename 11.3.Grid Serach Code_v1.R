####Neural Net Grid Search

train<- read.csv("./Car Accidents IOT/Train.csv")
dim(train)
test<- read.csv("./Car Accidents IOT/Test.csv")
dim(test)

#Creating intime train and test data
sample_index<-sample(1:nrow(train),nrow(train)*0.8, replace = F, set.seed(23))
length(sample_index)

train1<-train[sample_index,]
dim(train1)
names(train1)
test1<-train[-sample_index,]
dim(test1)

#Standardise the data
library(clusterSim)
train_strd<-data.Normalization (train1[,-1],type="n1",normalization="column")
head(train_strd)
train_strd$Fatal<-train1$Fatal

# x vector, matrix or dataset type ;type of normalization: n0 - without normalization
# n1 - standardization ((x-mean)/sd)
# n2 - positional standardization ((x-median)/MAD)
# n3 - unitization ((x-mean)/range)

test_strd<-data.Normalization (test1[,-1],type="n1",normalization="column")
head(test_strd)
test_strd$Fatal<-test1$Fatal

train1<-train_strd
test1<-test_strd
names(test1)
grid_results <-data.frame()

decay_d=0
hidden_h=0
j=1
i=1

library(nnet)
library(caret)

for(decay_d in c(0,0.1,0.5,10,50)){
  for(hidden_h in c(2,5,10)){
    
    mod1<-nnet(as.factor(Fatal)~., data=train1, 
               size=hidden_h, 
               maxit=500,
               decay = decay_d)
    
    #Accuracy on test data
    
    actual_values_test<-as.factor(test1$Fatal)
    Predicted_test<-predict(mod1,test1[,-23], type="class")
    cm_test<-confusionMatrix(factor(actual_values_test),factor(Predicted_test))
    cm_test$overall[1]
    
    grid_results[i,j]=cm_test$overall[1]
    
    i=i+1
  }
  i=1
  j=j+1
}

grid_results

names(grid_results)<-c(0,0.1,0.5,10,50)
row.names(grid_results)<-c(2,5,10)
grid_results


############################################################
####Random Forest Grid Search

train<- read.csv("./Car Accidents IOT/Train.csv")
dim(train)
test<- read.csv("./Car Accidents IOT/Test.csv")
dim(test)

#Creating intime train and test data
sample_index<-sample(1:nrow(train),nrow(train)*0.8, replace = F, set.seed(23))
length(sample_index)

train1<-train[sample_index,]
dim(train1)
names(train1)
test1<-train[-sample_index,]
dim(test1)

grid_results <-data.frame()

ntree_n=0
mtry_m=0
j=1
i=1

library(randomForest)
for(ntree_n in c(5,10,50,70,100,150)){
  for(mtry_m in c(2,3,4,5)){
 
    rf_model <- randomForest(as.factor(train1$Fatal) ~ ., ntree=ntree_n,   mtry=mtry_m, data=train1)
    
    #Accuracy on test data
    
    actual_values_test<-test1$Fatal
    Predicted_test<-predict(rf_model,test1,type="class")
    cm_test<-table(actual_values_test,Predicted_test)
    cm_test
    acc_test<-(cm_test[1,1]+cm_test[2,2])/(cm_test[1,1]+cm_test[1,2]+cm_test[2,1]+cm_test[2,2])
    acc_test
 
    grid_results[i,j]=acc_test
    
    i=i+1
  }
  i=1
  j=j+1
}

grid_results

names(grid_results)<-c(5,10,50,70,100,150)
row.names(grid_results)<-c(2,3,4,5)
grid_results

#############################################################################
###Grid Serach in GBM


train<- read.csv("./Car Accidents IOT/Train.csv")
dim(train)
test<- read.csv("./Car Accidents IOT/Test.csv")
dim(test)

#Creating intime train and test data
sample_index<-sample(1:nrow(train),nrow(train)*0.8, replace = F, set.seed(23))
length(sample_index)

train1<-train[sample_index,]
dim(train1)
names(train1)
test1<-train[-sample_index,]
dim(test1)

grid_results <-data.frame()

n=0
s=0
j=1
i=1
library(gbm)
for(n in c(10,50, 100,200,500)){
  for(s in c(0.001,0.05, 0.07, 0.1, 0.5,1)){
    gm<-gbm(Fatal~., data=train1,  
            distribution="bernoulli", 
            verbose=T, 
            interaction.depth = 2 , 
            n.trees = n,
            n.minobsinnode=5,
            bag.fraction=1,set.seed(125),
            shrinkage = s)
    
    conf_matrix<-confusionMatrix(factor(ifelse(predict(gm,  n.trees = n, newdata=test1[,-1], type="response")<0.5,0,1)),factor(test1$Fatal))
    grid_results[i,j]=conf_matrix$overall[1]
    i=i+1
  }
  i=1
  j=j+1
}
grid_results

names(grid_results)<-c(10,50, 100,200,500)
row.names(grid_results)<- c(0.001,0.05, 0.07, 0.1, 0.5,1)
grid_results


#############################################################################
###Grid Serach in SVM


train<- read.csv("./Car Accidents IOT/Train.csv")
dim(train)
test<- read.csv("./Car Accidents IOT/Test.csv")
dim(test)

#Creating intime train and test data
sample_index<-sample(1:nrow(train),nrow(train)*0.5, replace = F, set.seed(23))
length(sample_index)

train1<-train[sample_index,]
dim(train1)
names(train1)
test1<-train[-sample_index,]
dim(test1)

grid_results <-data.frame()

n=0
s=0
j=1
i=1
library(e1071)
library(caret)

for(C in c(1/8,1/4,1)){
  for(G in c(1/64,1/16,1/4,2)){
    svm_m1<- svm(as.factor(Fatal)~., data=train1 ,cost = C, gamma=G)
    conf_matrix<-confusionMatrix(factor(predict(svm_m1, type = "class", newdata=test1[,-1])),factor(test1$Fatal))
    grid_results[i,j]=conf_matrix$overall[1]
    i=i+1
  }
  i=1
  j=j+1
}
grid_results

names(grid_results)<-c(1/8,1/4,1)
row.names(grid_results)<-  c(1/64,1/16,1/4,2)
grid_results

###################################################################
###### Model Implementation PMML 

library(randomForest)
set.seed(71)
iris.rf <- randomForest(Species ~ ., data=iris,ntree=200 )
print(iris.rf)

?randomForest
require(pmml)
saveXML(pmml(iris.rf),file = 'iris_rf.pmml')



