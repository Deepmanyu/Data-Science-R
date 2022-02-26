################################################################################################################
############################               Regression                           ################################
################################################################################################################

#Dataset: Air Travel Data\Air_travel.csv

#Importing Air passengers data
air <- read.csv("D:\\Google Drive\\Training\\Datasets\\AirPassengers\\AirPassengers.csv")
dim(air) 
names(air)
head(air)

#Correlation between promotion and passengers count
cor(air$Promotion_Budget,air$Passengers)


#Regression Model  promotion and passengers count
model1<-lm(Passengers~Promotion_Budget, data=air)
summary(model1)

#Potting the Regression line
plot(air$Promotion_Budget,air$Passengers,col = "blue", xlab="Age", ylab="Buy")
abline(model1, lwd = 5, col="red")

#Prediction 
newdata = data.frame(Promotion_Budget=650000)
predict(model1, newdata)

#Prediction 
newdata = data.frame(Promotion_Budget=517356)
predict(model1, newdata)

#Prediction 
newdata = data.frame(Promotion_Budget=700000) 
predict(model1, newdata)

#Regression Model inter_metro_flight_ratio and passengers count
plot(air$Inter_metro_flight_ratio,air$Passengers)
model2<-lm(Passengers~Inter_metro_flight_ratio, data=air) 
summary(model2)

#No intercept model
#model_noint<-lm(Passengers~0+Promotion_Budget, data=air) 
#summary(model_noint)

################################################
#Multiple Regerssion Model
multi_model<-lm(Passengers~Promotion_Budget+Inter_metro_flight_ratio+Service_Quality_Score , data=air)
summary(multi_model)


###############################################
##Adjusted R-Square
adj_sample<-read.csv("D:\\Google Drive\\Training\\Datasets\\Adjusted RSquare\\Adj_Sample.csv")

m1<-lm(Y~x1+x2+x3,data=adj_sample)
summary(m1)

m2<-lm(Y~x1+x2+x3+x4+x5+x6, data=adj_sample)
summary(m2)

m3<-lm(Y~x1+x2+x3+x4+x5+x6+x7+x8, data=adj_sample) 
summary(m3)

################################################
#####Multiple Regression- issues
final_exam<- read.csv("Final Exam/Final Exam Score.csv")

cat("The size of data")
dim(final_exam)

cat("The names of the variables")
names(final_exam)

cat("First few observations")
head(final_exam)

exam_model<-lm(Final_exam_marks~Sem1_Science+Sem2_Science+Sem1_Math+Sem2_Math, data=final_exam)
summary(exam_model)   

#After dropping Sem1_Math 
exam_model2<-lm(Final_exam_marks~Sem1_Science+Sem2_Science+Sem2_Math, data=final_exam) 
summary(exam_model2)  

#Sctter Plot between the predictor variables 
plot(final_exam$Sem1_Math,final_exam$Sem2_Math) 
cor(final_exam$Sem1_Math,final_exam$Sem2_Math) 


###############################################
##Multicollinearity detection
##Testing Multicollinearity
###Need car package (Companion to Applied Regression)
library(car)
exam_model<-lm(Final_exam_marks~Sem1_Science+Sem2_Science+Sem1_Math+Sem2_Math, data=final_exam)
summary(exam_model)
vif(exam_model)

exam_model1<-lm(Final_exam_marks~Sem1_Science+Sem2_Science+Sem2_Math, data=final_exam)
summary(exam_model1)
vif(exam_model1)

exam_model2<-lm(Final_exam_marks~Sem2_Science+Sem2_Math, data=final_exam)
summary(exam_model2)

cat("VIF Values of each variable")
vif(exam_model2)

################################################################################################################
############################  Logistic Regression                     ################################
################################################################################################################

#Import Product Sales Data 
Product_sales <- read.csv("D:\\Google Drive\\Training\\Datasets\\Product Sales Data\\Product_sales.csv")

dim(Product_sales)
names(Product_sales)

table(Product_sales$Bought)

#Building a linear regression line
prod_sales_model<-lm(Bought~Age,data=Product_sales)
summary(prod_sales_model)

#Prediction for Age=4
new_data<-data.frame(Age=4)
predict(prod_sales_model,new_data)

#Prediction for Age=105
new_data<-data.frame(Age=105)
predict(prod_sales_model,new_data)

#Plotting data and Linear Regression line
plot(Product_sales$Age,Product_sales$Bought,col = "blue", xlab="Age", ylab="Buy")
abline(prod_sales_model, lwd = 5, col="red")


############################################################
##Building logistic Regression Line

prod_sales_Logit_model <- glm(Bought ~ Age,family=binomial(),data=Product_sales)
summary(prod_sales_Logit_model)

#Prediction for Age=3
new_data<-data.frame(Age=4)
predict(prod_sales_Logit_model,new_data,type="response")

#Prediction for Age=105
new_data<-data.frame(Age=105)
predict(prod_sales_Logit_model,new_data,type="response")

#Plotting the regression lines
plot(Product_sales$Age,Product_sales$Bought,col = "blue")
curve(predict(prod_sales_Logit_model,data.frame(Age=x),type="resp"),add=TRUE, lwd = 5, col = "blue")
#Adding linear Regression Line
abline(prod_sales_model, lwd = 5, col="red")


############################################################
##Multiple logistic Regression Line

Fiberbits <- read.csv("D:\\Google Drive\\Training\\Datasets\\Fiberbits\\Fiberbits.csv")


Fiberbits_model_1<-glm(active_cust~.,family=binomial(),data=Fiberbits)
summary(Fiberbits_model_1)


###########Classification Table
predicted_values<-predict(prod_sales_Logit_model,type="response")
cat("Predcited Values")
predicted_values[1:10]

cat("Lets convert them to classes using a threshold")
threshold=0.5
threshold

predicted_class<-ifelse(predict(prod_sales_Logit_model,type="response")>threshold,1,0)
cat("Predcited Classes")
predicted_class[1:10]

actual_values<-prod_sales_Logit_model$y
conf_matrix<-table(predicted_class,actual_values)
cat("Confusion Matrix")
conf_matrix


accuracy<-(conf_matrix[1,1]+conf_matrix[2,2])/(sum(conf_matrix))
cat("Accuracy")
accuracy


##############Multicollinearity 
cat("Need car package")
library(car)

summary(Fiberbits_model_1)

cat("use VIF for identifying the Multicollinearity")
vif(Fiberbits_model_1)


##############Individual Impact of Variables
library(caret)
summary(Fiberbits_model_1)
varImp(Fiberbits_model_1, scale = FALSE)

