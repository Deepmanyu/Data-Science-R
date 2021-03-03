###########################################################################
###############Neural Network Code

####LAB Logistic Regression######
###Data
Emp_Productivity_raw <- read.csv(".\\Emp_Productivity\\Emp_Productivity.csv")

############################################
####Sample-1
############################################
Emp_Productivity1<-Emp_Productivity_raw[Emp_Productivity_raw$Sample_Set<3,]


dim(Emp_Productivity1)
names(Emp_Productivity1)
head(Emp_Productivity1)

table(Emp_Productivity1$Productivity)

####The clasification graph Sample-1
library(ggplot2)
ggplot(Emp_Productivity1)+geom_point(aes(x=Age,y=Experience,color=factor(Productivity),shape=factor(Productivity)),size=5)

###Logistic Regerssion model1
Emp_Productivity_logit<-glm(Productivity~Age+Experience,data=Emp_Productivity1, family=binomial())
Emp_Productivity_logit
coef(Emp_Productivity_logit)

####Accuracy of the model1
predicted_values<-round(predict(Emp_Productivity_logit,type="response"),0)
conf_matrix<-table(predicted_values,Emp_Productivity_logit$y)
conf_matrix

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy
accuracy_Sample1<-accuracy

###########LAB: Decision Boundary #####################
####Drawing the Decision boundary for model1

Emp_Productivity_logit
coef(Emp_Productivity_logit)

slope1 <- coef(Emp_Productivity_logit)[2]/(-coef(Emp_Productivity_logit)[3])
intercept1 <- coef(Emp_Productivity_logit)[1]/(-coef(Emp_Productivity_logit)[3]) 

library(ggplot2)
base<-ggplot(Emp_Productivity1)+geom_point(aes(x=Age,y=Experience,color=factor(Productivity),shape=factor(Productivity)),size=5)
base+geom_abline(intercept = intercept1 , slope = slope1, color = "red", size = 2) #Base is the scatter plot. Then we are adding the decision boundary


############################################
#######Overall Data
#######LAB: Non-Linear Decision Boundaries

####The clasification graph on overall data
library(ggplot2)
ggplot(Emp_Productivity_raw)+geom_point(aes(x=Age,y=Experience,color=factor(Productivity),shape=factor(Productivity)),size=5)

###Logistic Regerssion model for overall data
Emp_Productivity_logit_overall<-glm(Productivity~Age+Experience,data=Emp_Productivity_raw, family=binomial())
Emp_Productivity_logit_overall


slope2 <- coef(Emp_Productivity_logit_overall)[2]/(-coef(Emp_Productivity_logit_overall)[3])
intercept2 <- coef(Emp_Productivity_logit_overall)[1]/(-coef(Emp_Productivity_logit_overall)[3]) 


####Drawing the Decision boundary

library(ggplot2)
base<-ggplot(Emp_Productivity_raw)+geom_point(aes(x=Age,y=Experience,color=factor(Productivity),shape=factor(Productivity)),size=5)
base+geom_abline(intercept = intercept2 , slope = slope2, colour = "blue", size = 2) 

####Accuracy of the overall model
predicted_values<-round(predict(Emp_Productivity_logit_overall,type="response"),0)
conf_matrix<-table(predicted_values,Emp_Productivity_logit_overall$y)
conf_matrix

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy

accuracy_all_data<-accuracy

############################################
############################################
####Intermediate Output

####The clasification graph on overall data
library(ggplot2)
ggplot(Emp_Productivity_raw)+geom_point(aes(x=Age,y=Experience,color=factor(Productivity),shape=factor(Productivity)),size=5)


####The clasification graph Sample-1
library(ggplot2)
ggplot(Emp_Productivity1)+geom_point(aes(x=Age,y=Experience,color=factor(Productivity),shape=factor(Productivity)),size=5)

####Decision boundary for model1 built on Sample-1
library(ggplot2)
base<-ggplot(Emp_Productivity1)+geom_point(aes(x=Age,y=Experience,color=factor(Productivity),shape=factor(Productivity)),size=5)
base+geom_abline(intercept = intercept1 , slope = slope1, color = "red", size = 2) #Base is the scatter plot. Then we are adding the decision boundary

#### sample-2
############################################

Emp_Productivity2<-Emp_Productivity_raw[Emp_Productivity_raw$Sample_Set>1,]

####The clasification graph
library(ggplot2)
ggplot(Emp_Productivity2)+geom_point(aes(x=Age,y=Experience,color=factor(Productivity),shape=factor(Productivity)),size=5)


###Logistic Regerssion model2 built on Sample2
Emp_Productivity_logit2<-glm(Productivity~Age+Experience,data=Emp_Productivity2, family=binomial())
Emp_Productivity_logit2

coef(Emp_Productivity_logit2)
slope3 <- coef(Emp_Productivity_logit2)[2]/(-coef(Emp_Productivity_logit2)[3])
intercept3 <- coef(Emp_Productivity_logit2)[1]/(-coef(Emp_Productivity_logit2)[3]) 


####Drawing the Decison boundry
library(ggplot2)
base<-ggplot(Emp_Productivity2)+geom_point(aes(x=Age,y=Experience,color=factor(Productivity),shape=factor(Productivity)),size=5)
base+geom_abline(intercept = intercept3 , slope = slope3, color = "red", size = 2) 

####Accuracy of the model2
predicted_values<-round(predict(Emp_Productivity_logit2,type="response"),0)
conf_matrix<-table(predicted_values,Emp_Productivity_logit2$y) 
conf_matrix

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy
accuracy_Sample2<-accuracy 

###############################################
#### The Intermediate output and combined model
###############################################

#The Two models
Emp_Productivity_logit
Emp_Productivity_logit2

#The two new coloumns
Emp_Productivity_raw$inter1<-predict(Emp_Productivity_logit,type="response", newdata=Emp_Productivity_raw)
Emp_Productivity_raw$inter2<-predict(Emp_Productivity_logit2,type="response", newdata=Emp_Productivity_raw)

head(Emp_Productivity_raw)

####Clasification graph with the two new coloumns
library(ggplot2)
ggplot(Emp_Productivity_raw)+geom_point(aes(x=inter1,y=inter2,color=factor(Productivity),shape=factor(Productivity)),size=5)

###Logistic Regerssion model with Intermediate outputs as input
Emp_Productivity_logit_combined<-glm(Productivity~inter1+inter2,data=Emp_Productivity_raw, family=binomial())
Emp_Productivity_logit_combined

####Drawing the Decison boundry
slope4 <- coef(Emp_Productivity_logit_combined)[2]/(-coef(Emp_Productivity_logit_combined)[3])
intercept4<- coef(Emp_Productivity_logit_combined)[1]/(-coef(Emp_Productivity_logit_combined)[3]) 


library(ggplot2)
base<-ggplot(Emp_Productivity_raw)+geom_point(aes(x=inter1,y=inter2,color=factor(Productivity),shape=factor(Productivity)),size=5)
base+geom_abline(intercept = intercept4 , slope = slope4, colour = "red", size = 2) 

####Accuracy of the combined
predicted_values<-round(predict(Emp_Productivity_logit_combined,type="response"),0)
conf_matrix<-table(predicted_values,Emp_Productivity_logit_combined$y)
conf_matrix

accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
accuracy
accuracy_intermediate_Step<-accuracy

#The Final three models
Emp_Productivity_logit

Emp_Productivity_logit2

Emp_Productivity_logit_combined

#Acccuracies of all the models till now
accuracy_all_data
accuracy_Sample1
accuracy_Sample2
accuracy_intermediate_Step


###############################################
#### Building Neuralnetork on Employee productivity data
###############################################

####Building neural net
library(neuralnet)

Emp_Productivity_nn_model1<-neuralnet(Productivity~Age+Experience,data=Emp_Productivity_raw, hidden=3,linear.output = FALSE)
plot(Emp_Productivity_nn_model1)


####Results and Intime validation
actual_values<-Emp_Productivity_raw$Productivity
Predicted<-Emp_Productivity_nn_model1$net.result[[1]]
Predicted

#The root mean square error
sqr_err<-(actual_values-Predicted)^2
sum(sqr_err)
mean(sqr_err)
sqrt(mean(sqr_err))

#Plottig Actual and Predicted
plot(actual_values)
points(Predicted, col=2)

#Plottig Actual and Predicted using ggplot
library(ggplot2)
library(reshape2)
act_pred_df<-data.frame(actual_values,Predicted)
act_pred_df$id<-rownames(act_pred_df)
act_pred_df_melt = melt(act_pred_df, id.vars ="id")
ggplot(act_pred_df_melt,aes(id, value, colour = variable)) + geom_point() 

##Plotting Actual and Predicted using ggplot on classification graph

Emp_Productivity_pred_act<-data.frame(Emp_Productivity_raw,Predicted=round(Predicted,0))
library(ggplot2)
#Graph without predictions
ggplot(Emp_Productivity_pred_act)+geom_point(aes(x=Age,y=Experience,color=factor(Productivity)),size=5)

#Graph with predictions
ggplot(Emp_Productivity_pred_act)+geom_point(aes(x=Age,y=Experience,color=factor(Productivity),shape=factor(Predicted)),size=5)

###########
#There is an issue with the local minimum. if you see the error is high, then you can rebuild the model
Emp_Productivity_nn_model1<-neuralnet(Productivity~Age+Experience,data=Emp_Productivity_raw, hidden=3,linear.output = FALSE)
plot(Emp_Productivity_nn_model1)


#########Further increasing hidden layers
Emp_Productivity_nn_model3<-neuralnet(Productivity~Age+Experience,data=Emp_Productivity_raw, hidden=4,threshold=0.001, stepmax = 1e+07, linear.output = FALSE)
plot(Emp_Productivity_nn_model3)


################ R options in neuralnet


########Stepmax
#Alogorithm didn't converge with the default  step max; stepmax = 1e+05; The algorith needs more steps than 100,000; 
#Lets Increse the steps to 10,000,000
Emp_Productivity_nn_model2<-neuralnet(Productivity~Age+Experience,data=Emp_Productivity_raw, hidden=2, stepmax = 1e+07, linear.output = FALSE)
plot(Emp_Productivity_nn_model2)


########Save the model
#save(Emp_Productivity_nn_model2, file = "C:\\Users\\venk\\Google Drive\\Training\\Machine Learning\\3.References\\5. Neural Networks\\NN_model_2H_Err.rda")
#load("C:\\Users\\venk\\Google Drive\\Training\\Machine Learning\\3.References\\5. Neural Networks\\NN_model_2H_Err.rda")


############################################
####Digit Recognition Example-USPS Data

##Data unzipping

#gunzip("D:\\Google Drive\\Training\\Datasets\\Digit Recognizer\\Original Numbers Data\\t10k-images-idx3-ubyte.gz")
#gunzip("D:\Google Drive\Training\Datasets\\Digit Recognizer\\Original Numbers Data\\t10k-labels-idx1-ubyte.gz")
#gunzip("D:\Google Drive\Training\Datasets\\Digit Recognizer\\Original Numbers Data\\train-images-idx3-ubyte.gz")
#gunzip("D:\Google Drive\Training\Datasets\\Training\\Datasets\\Digit Recognizer\\Original Numbers Data\\train-labels-idx1-ubyte.gz")


#train_images<-file("D:\\Google Drive\\Training\\Datasets\\Digit Recognizer\\MNIST\\train-images-idx3-ubyte", "rb")
#train_labels<-file("D:\\Google Drive\\Training\\Datasets\\Digit Recognizer\\MNIST\\train-labels-idx1-ubyte", "rb")
#test_images<-file("D:\\Google Drive\\Training\\Datasets\\Digit Recognizer\\MNIST\\t10k-images-idx3-ubyte.txt", "rb")
#test_labels<-file("D:\\Google Drive\\Training\\Datasets\\Digit Recognizer\\MNIST\\t10k-labels-idx1-ubyte", "rb")

#number_pixels_train <- readBin( train_images, integer(), n=1, endian="big")
#number_labels_train <- readBin( train_labels, integer(), n=1, endian="big")
#number_pixels_test <- readBin( train_images, integer(), n=1, endian="big")
#number_labels_test<- readBin( train_labels, integer(), n=1, endian="big")



###################################################################################

#Importing test and training data - USPS Data
digits_train <- read.table("D:\\Google Drive\\Training\\Datasets\\Digit Recognizer\\USPS\\zip.train.txt", quote="\"", comment.char="")
digits_test <- read.table("D:\\Google Drive\\Training\\Datasets\\Digit Recognizer\\USPS\\zip.test.txt", quote="\"", comment.char="")

dim(digits_train)

col_names <- names(digits_train[,-1])
label_levels<-names(table(digits_train$V1))

#Lets see some images. 
for(i in 1:10)
{
data_row<-digits_train[i,-1]
pixels = matrix(as.numeric(data_row),16,16,byrow=TRUE)
image(pixels, axes = FALSE)
title(main = paste("Label is" , digits_train[i,1]), font.main = 4)
}


#####Creating multiple columns for multiple outputs
#####We need these variables while building the model
digit_labels<-data.frame(label=digits_train[,1])
for (i in 1:10)
	{
	digit_labels<-cbind(digit_labels, digit_labels$label==i-1)
	names(digit_labels)[i+1]<-paste("l",i-1,sep="")
	}

label_names<-names(digit_labels[,-1])

#Update the training dataset
digits_train1<-cbind(digits_train,digit_labels)
names(digits_train1)


#formula y~. doesn't work in neuralnet function
model_form <- as.formula(paste(paste(label_names, collapse = " + "), "~", paste(col_names, collapse = " + ")))

######################The Model
pc <- proc.time()#Lets keep an eye on runtime

library(neuralnet)
Digit_model<-neuralnet(model_form, data=digits_train1, hidden=15,linear.output=FALSE)
summary(Digit_model)

proc.time() - pc

#######Prediction  on holdout data
test_predicted<-data.frame(compute(Digit_model,digits_test[,-1])$net.result)

########Collating all labels into a single column
pred_label<-0
for(i in 1:nrow(test_predicted))
	{
	pred_label[i]<-which.max(apply(test_predicted[i,],MARGIN=2,min))-1
    }	
test_predicted$pred_label<-pred_label

###Confusion Matrix and Accuracy
library(caret)
table(test_predicted$pred_label)
table(digits_test$V1)
confuse<-table(test_predicted$pred_label,digits_test$V1)
diag(confuse)
sum(diag(confuse))


accuracy_test_data=sum(diag(confuse))/sum(confuse)
accuracy_test_data









