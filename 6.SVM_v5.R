Transactions_sample <- read.csv("./Fraud Transaction/Transactions_sample.csv")
head(Transactions_sample)
names(Transactions_sample)

#####Logit model
logit_model<-glm(Fraud_id~Total_Amount+Tr_Count_week,data=Transactions_sample,family=binomial())
logit_model 

###The classifier slope & intercept
logit_slope <- coef(logit_model)[2]/(-coef(logit_model)[3]) 
logit_intercept<- coef(logit_model)[1]/(-coef(logit_model)[3]) 


###The classifier diagram 
library(ggplot2)
base<-ggplot(Transactions_sample)+geom_point(aes(x=Total_Amount,y=Tr_Count_week,color=factor(Fraud_id),shape=factor(Fraud_id)),size=5)
base+geom_abline(intercept = logit_intercept , slope = logit_slope, color = "red", size = 2) 


######################################
######SVM Models
#####################################

#SVM Building needs e1071 package
library(e1071)

#Converting the output into factor, otherwise SVM will fit a regression model
Transactions_sample$Fraud_id<-factor(Transactions_sample$Fraud_id) 

#SVM Model building
svm_model <- svm(Fraud_id~Total_Amount+Tr_Count_week, data=Transactions_sample)
summary(svm_model)

#Plotting SVM Clasification graph
plot(svm_model, Transactions_sample,Tr_Count_week~Total_Amount ) #x2~x1

#Prediction in SVM
new_data1<-data.frame(Total_Amount=11000, Tr_Count_week=15)
new_data2<-data.frame(Total_Amount=2000, Tr_Count_week=4)

p1<-predict(svm_model, new_data1)
p1
p2<-predict(svm_model, new_data2)
p2

#SVM on overall data
Transactions<- read.csv("Fraud Transaction/Transaction.csv")

#Converting the output into factor, otherwise SVM will fit a regression model

svm_model_1 <- svm(Fraud_id~Total_Amount+Tr_Count_week, type="C", data=Transactions)
summary(svm_model_1)

#Plotting SVM Clasification graph
plot(svm_model_1, Transactions,Tr_Count_week~Total_Amount ) 


############################################################################
###### Kernels

sw_user_profile <- read.csv("./Software users/sw_user_profile.csv")
plot(sw_user_profile$Age,sw_user_profile$Id,  col=as.integer(sw_user_profile$Active+1))


#Model Building 
library(e1071)
svm_model_nl <- svm(Active~Age,  type="C",  data=sw_user_profile)
summary(svm_model_nl)

#Making the kernel to linear
svm_model_nl <- svm(Active~Age,  type="C", kernel="linear", data=sw_user_profile)
summary(svm_model_nl)

#Confusion Matrix
library(caret)
Age_predicted<-predict(svm_model_nl)
confusionMatrix(Age_predicted,factor(sw_user_profile$Active))

#######################################
#New variable derivation. Mapping to higher dimensions

#Creating the new variable
sw_user_profile$new<-(sw_user_profile$Age)^2
plot(sw_user_profile$Age,sw_user_profile$new,  col=as.integer(sw_user_profile$Active+1))

#Model Building with new variable
library(e1071)
svm_model_2 <- svm(Active~Age+new,  type="C", kernel="linear", data=sw_user_profile)
summary(svm_model_2)

#Confusion Matrix
library(caret)
Age_predicted<-predict(svm_model_2)
confusionMatrix(Age_predicted,factor(sw_user_profile$Active))

##Plotting SVM Classification graph
plot(svm_model_2, sw_user_profile,new~Age ) 



########Model Building with radial kernel function
library(e1071)
svm_model_3 <- svm(Active~Age,  type="C", data=sw_user_profile)
summary(svm_model_3)


#Confusion Matrix
library(caret)
Age_predicted<-predict(svm_model_3)
confusionMatrix(Age_predicted,factor(sw_user_profile$Active))


################################################
###SVM for Hand Written Digit Recognition Example

#Importing test and training data
digits_train <- read.table("Digit Recognizer/USPS/zip.train.txt", quote="\"", comment.char="")
digits_test <- read.table("Digit Recognizer/USPS/zip.test.txt", quote="\"", comment.char="")

dim(digits_train)
dim(digits_test)

#Lets see some images. 
for(i in 1:10 )
{
data_row<-digits_train[i,-1]
pixels = matrix(as.numeric(data_row),16,16,byrow=TRUE)
image(pixels, axes = FALSE)
title(main = paste("Label is" , digits_train[i,1]), font.main = 4)
}

#Are there any missing values?

sum(is.na(digits_train))
sum(is.na(digits_test))

#The first variable is label
table(digits_train$V1)
table(digits_test$V1)


########SVM Model Building 
library(e1071)

###################################
#####Model on Full Data 
pc <- proc.time()
number.svm <- svm(V1 ~. , type="C", data = digits_train)
proc.time() - pc
summary(number.svm)


###Out of time validation with test data
test_label_predicted<-predict(number.svm, newdata =digits_test[,-1] , type = "class")
confusionMatrix(test_label_predicted,factor(digits_test[,1]))

#Lets see some predictions. 
digits_test$predicted<-test_label_predicted

for(i in 1:10 )
{
data_row<-digits_test[i,c(-1,-ncol(digits_test))]
pixels = matrix(as.numeric(data_row),16,16,byrow=TRUE)
image(pixels, axes = FALSE)
title(main = paste("Label is" , digits_test[i,1] ,"  Prediction is" , digits_test[i,ncol(digits_test)]))
}


#Lets see some errors in predictions images. 
# Wrong predictions
digits_test$predicted<-test_label_predicted
wrong_predictions<-digits_test[!(digits_test$predicted==digits_test$V1),]
nrow(wrong_predictions)


for(i in 1:10 )
{
data_row<-wrong_predictions[i,c(-1,-ncol(wrong_predictions))]
pixels = matrix(as.numeric(data_row),16,16,byrow=TRUE)
image(pixels, axes = FALSE)
title(main = paste("Label is" , wrong_predictions[i,1] ,"  Prediction is" , wrong_predictions[i,ncol(wrong_predictions)]))
}

###########################################################################
##Probability prediction in SVM

Transactions_sample <- read.csv("./Fraud Transaction/Transactions_sample.csv")
head(Transactions_sample)
names(Transactions_sample)

svm_model_Fraud <- svm(as.factor(Fraud_id)~Total_Amount+Tr_Count_week, data=Transactions_sample,probability = TRUE)
summary(svm_model_Fraud)

# compute decision values and probabilites
pred <- predict(svm_model_Fraud,Transactions_sample[,c(2,3)], probability = TRUE)
pred 
