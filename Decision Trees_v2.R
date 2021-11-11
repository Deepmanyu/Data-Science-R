#Import Data 
Ecom_Cust_Survey <- read.csv("D:\\Google Drive\\Training\\Datasets\\Ecom_Cust_Relationship_Management\\Ecom_Cust_Survey.csv")

dim(Ecom_Cust_Survey)
names(Ecom_Cust_Survey)

#Need the library rpart
library(rpart)

#Building Tree Model
Ecom_Tree<-rpart(Overall_Satisfaction~Region+ Age+ Order.Quantity+Customer_Type+Improvement.Area, method="class", data=Ecom_Cust_Survey, control=rpart.control(minsplit=30))
Ecom_Tree

#Plotting the trees
plot(Ecom_Tree, uniform=TRUE)
text(Ecom_Tree, use.n=TRUE, all=TRUE)

#A better looking tree
library(rattle)
fancyRpartPlot(Ecom_Tree,palettes=c("Greys", "Oranges"))

########################################
##########Tree Validation
Ecom_pred<-predict(Ecom_Tree, type="class")

###To predict the probabailities insted of class 
#Ecom_pred_prob<-predict(Ecom_Tree, type="prob")

####Calculation of Accuracy and Confusion Matrix
conf_matrix<-table(Ecom_pred,Ecom_Cust_Survey$Overall_Satisfaction)
conf_matrix
accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy


############################################################################ 
##The problem of overfitting
#Choosing Cp and Pruning
train <- read.csv("D:\\Google Drive\\Training\\Datasets\\Buyers Profiles\\Train_data.csv")
test<-read.csv("D:\\Google Drive\\Training\\Datasets\\Buyers Profiles\\Test_data.csv")

library(rpart)
#Sample_tree<-rpart(Bought~Gender+Age, method="class", data=Prune_Sample, control=rpart.control(minsplit=2, cp=0.001))
Sample_tree<-rpart(Bought~Gender+Age, method="class", data=train, control=rpart.control(minsplit=2))

Sample_tree

#Plotting
library(rattle)
fancyRpartPlot(Sample_tree,palettes=c("Greys", "Oranges"))

##########Tree Validation
###Accuracy On the training data
sample_pred<-predict(Sample_tree, type="class")

####Calculation of Accuracy and Confusion Matrix
conf_matrix<-table(sample_pred,train$Bought)
conf_matrix

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy


###Accuracy On the test data
test_pred<- predict(Sample_tree, test,type="class")


####Calculation of Accuracy and Confusion Matrix
conf_matrix<-table(test_pred,test$Bought)
conf_matrix

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy
 
############################################################################ 
#Pruning the tree and choosing Cp

#Changing Cp
Sample_tree_1<-rpart(Bought~Gender+Age, method="class", data=train, control=rpart.control(minsplit=2, cp=0.1))
Sample_tree_1

#Plotting
library(rattle)
fancyRpartPlot(Sample_tree_1,palettes=c("Greys", "Oranges"))

##########Tree Validation
###Accuracy On the training data
sample_pred<-predict(Sample_tree_1, type="class")

####Calculation of Accuracy and Confusion Matrix
conf_matrix<-table(sample_pred,train$Bought)
conf_matrix

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy


###Accuracy On the test data
test_pred<- predict(Sample_tree_1, test,type="class")


####Calculation of Accuracy and Confusion Matrix
conf_matrix<-table(test_pred,test$Bought)
conf_matrix

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy

# Cp display the results
printcp(Sample_tree)
# cross-validation results
plotcp(Sample_tree)

Sample_tree_2<-rpart(Bought~Gender+Age, method="class", data=train, control=rpart.control(minsplit=2, cp=0.23))
Sample_tree_2

#Plotting
fancyRpartPlot(Sample_tree_2,palettes=c("Greys", "Oranges"))

########We can either build a new tree or Prune the old tree 
Sample_tree<-rpart(Bought~Gender+Age, method="class", data=train, control=rpart.control(minsplit=2))
Sample_tree

#Plotting
library(rattle)
fancyRpartPlot(Sample_tree,palettes=c("Greys", "Oranges"))


Pruned_tree<-prune(Sample_tree,cp=0.23)
Pruned_tree

fancyRpartPlot(Pruned_tree,palettes=c("Greys", "Oranges"))


############################################################################ 
#Building the tree model for Fiberbits data
Fiberbits <- read.csv("D:\\Google Drive\\Training\\Datasets\\Fiberbits\\Fiberbits.csv")
names(Fiberbits)

library(rpart)
Fiber_bits_tree<-rpart(active_cust~., method="class", control=rpart.control(minsplit=30, cp=0.001), data=Fiberbits)
Fiber_bits_tree
fancyRpartPlot(Fiber_bits_tree)


#Analyzing the tree
printcp(Fiber_bits_tree) 
plotcp(Fiber_bits_tree) 

#Pruning
Fiber_bits_tree_1<-prune(Fiber_bits_tree, cp=0.0081631)
Fiber_bits_tree_1
fancyRpartPlot(Fiber_bits_tree_1)

#Analyzing the tree
printcp(Fiber_bits_tree_1) 
plotcp(Fiber_bits_tree_1) 

#Pruning further
Fiber_bits_tree_2<-prune(Fiber_bits_tree, cp=0.0239316)
Fiber_bits_tree_2
fancyRpartPlot(Fiber_bits_tree_2)

#Analyzing the tree
printcp(Fiber_bits_tree_2) 
plotcp(Fiber_bits_tree_2) 

#####Prediction using the model
conf_matrix<-table(predict(Fiber_bits_tree_2, type="class"),Fiberbits$active_cust)
conf_matrix
accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy







