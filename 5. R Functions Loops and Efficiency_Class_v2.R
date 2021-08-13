
######################################################
###For Loop in R

#Finding Squares of first twenty numbers
square<-1 # Any random value for initialization 
for (i in 1 : 20 )
{
  square[i]<-i^2
}
square


#Creating cumulative units sold.
Sales<- read.csv("./Superstore Sales Data/Sales_sample.csv")
Sales$Cumulative_units[1]= Sales$unitsSold[1] 
for (i in 2:nrow(Sales))
{
  Sales$Cumulative_units[i]= Sales$Cumulative_units[i-1]+ Sales$unitsSold[i] 
}

Sales

################################################# 
###########User Defined Function 

#Distancie function
mydistance<-function(x1,y1,x2,y2)
{
  sqrt((x1-x2)^2+(y1-y2)^2) 
}
mydistance(0,0, 2,2)
mydistance(2,2, 9,9)

#Sum of squares function
mysumsquare<-function(x)
{ 
  sum=0 
  for (j in 1 : length(x))
  {
    sum=sum+(x[j])^2
  }
  return (sum) 
}
mysumsquare(c(1,2,3,4,5,6,7))

#########################################
#Function for missing values in all variables 

my_miss_values<-function(input_data=data.frame()){
  Miss_vals=0
  for (i in 1:ncol(input_data)) {
    Miss_vals[i]= sum(is.na(input_data[,i]))
    }
  print(Miss_vals)
}

AutoDataset <- read.csv("./Automobile Data Set/AutoDataset.csv")
my_miss_values(AutoDataset)


##Generic Function for missing values in all variables 
my_miss_values_1<-function(input_data=data.frame()){
  
  Miss_val_by_var <- data.frame(var_name=0, miss_val_count=0)
  i=1
  
  for (f in names(input_data)) {
  Miss_val_by_var[i,1]= f
  Miss_val_by_var[i,2]= sum(is.na(input_data[[f]]))
  i=i+1
  }
  final_result<-Miss_val_by_var[order(-Miss_val_by_var$miss_val_count),]
  print(final_result)
}

AutoDataset <- read.csv("./Automobile Data Set/AutoDataset.csv")
my_miss_values_1(AutoDataset)

#########################################################################
##Generic Summary function for missing values in all variables 
my_summary<-function(input_data=data.frame())
{
  i=1
  summary_data<-data.frame(var_name=0,miss_val_count=0,miss_per=0,mean=0,median=0,sd=0,P10=0,P20=0,P30=0,P50=0,P75=0,P90=0,P95=0,P97=0,maximum=0,range=0,variance=0)
  
  for(f in names(input_data))
  {
    ifelse(class(input_data[[f]]) %in% c("character","factor"),
           {
             summary_data[i,1]=f
             summary_data[i,2:17]=0
           },
           {
             summary_data[i,1]=f
             summary_data[i,2]=sum(is.na(input_data[[f]]))
             summary_data[i,3]=round((summary_data[i,2]/nrow(input_data))*100,2)
             summary_data[i,4]=round(mean(input_data[[f]],na.rm=TRUE),2)
             summary_data[i,5]=round(median(input_data[[f]],na.rm=TRUE),2)
             summary_data[i,6]=round(sd(input_data[[f]],na.rm=TRUE),2)
             summary_data[i,7]=round(quantile(input_data[[f]],0.1,na.rm=TRUE),2)
             summary_data[i,8]=round(quantile(input_data[[f]],0.2,na.rm=TRUE),2)
             summary_data[i,9]=round(quantile(input_data[[f]],0.3,na.rm=TRUE),2)
             summary_data[i,10]=round(quantile(input_data[[f]],0.5,na.rm=TRUE),2)
             summary_data[i,11]=round(quantile(input_data[[f]],0.75,na.rm=TRUE),2)
             summary_data[i,12]=round(quantile(input_data[[f]],0.9,na.rm=TRUE),2)
             summary_data[i,13]=round(quantile(input_data[[f]],0.95,na.rm=TRUE),2)
             summary_data[i,14]=round(quantile(input_data[[f]],0.97,na.rm=TRUE),2)
             summary_data[i,15]=max(input_data[[f]],na.rm=TRUE)
             summary_data[i,16]=max(input_data[[f]],na.rm=TRUE)-min(input_data[[f]],na.rm=TRUE)
             summary_data[i,17]=round(var(input_data[[f]],na.rm=TRUE),2)
             i = i + 1
           })
  }
  print(summary_data)
}

credit <- read.csv("./Give me some Credit/cs-training.csv")
my_summary(credit)
my_summary(AutoDataset)
my_summary(iris)


##########################################################
### Group By 

bank <- read.csv("./Bank Tele Marketing/bank_market.csv")

#Group by using for loop

#########married group
sum1<-0
count1<-0

for (i in 1 : nrow(bank))
{
  if(bank$marital[i]=="married") {
                                 sum1=sum1+bank$balance[i]
                                 count1=count1+1
  }

}
Married_avg_balance<-sum1/count1
Married_avg_balance  

#########married group
sum1<-0
count1<-0

for (i in 1 : nrow(bank))
{
  if(bank$marital[i]=="single") {
    sum1=sum1+bank$balance[i]
    count1=count1+1
  }
  
}
single_avg_balance<-sum1/count1
single_avg_balance  

#######################################
# Group by and summary is easy using dplyr 
library(dplyr)

###Summary by marital status
Groups <- group_by(bank, marital)
summarise(Groups, avgs=mean(balance))

###Summary by  job
Groups <- group_by(bank, job)
summarise(Groups, avgs=mean(balance))

#############################################
###: Group By and Summarise
online<- read.csv("./Online_Retail_Sales_Data/Online Retail.csv")

###Summary by marital status
Groups <- group_by(online, Country)
summarise(Groups, avgs=sum(Quantity))
summarise(Groups, avgs=mean(UnitPrice))

####################################
##tidyverse contains all dependent packages including dplyr  
library(tidyverse)

####################################
sales<- read.csv("./Online_Retail_Sales_Data/Online Retail.csv")

########Filter
filter_dplyr <- filter(sales, UnitPrice>10 & Country=="Germany")
dim(filter_dplyr)

########Select
select_dplyr <- select(sales, InvoiceNo, CustomerID, Country)
head(select_dplyr)

select_dplyr1 <- select(sales, 2:4)
head(select_dplyr1)

select_dplyr2 <- select(sales, contains("Invoice")) 
head(select_dplyr2)

#for two conditions
#select_dplyr_1 <- select(sales,  contains("ID"), contains("Date")) 
#head(select_dplyr_1)

#########################################
##"Chaining" or "Pipelining"
sales_3 <- sales %>%
  select(InvoiceDate, UnitPrice) %>%
  filter(UnitPrice > 20)

head(sales_3)

####arrage
sales_4 <- sales %>%
  select(InvoiceDate, UnitPrice, Quantity) %>%
  filter(UnitPrice > 20) %>%
  arrange(Quantity)

head(sales_4, 30)
#For descending 
#arrange(desc(Quantity))

#######mutate
sales_5 <- sales %>%
  select(InvoiceDate, UnitPrice, Quantity) %>%
  filter(UnitPrice > 20) %>%
  mutate(net_price = UnitPrice*Quantity) %>%
  arrange(desc(Quantity))

head(sales_5, 10)

#############

sales_6 <- sales %>%
  select(InvoiceDate, UnitPrice, Quantity, Country) %>%
  filter(UnitPrice > 20) %>%
  mutate(net_price = UnitPrice*Quantity) %>%
  group_by(Country) %>%
  summarise(AveragePriceCountry = mean(net_price))%>%
  arrange(AveragePriceCountry)

head(sales_6, 10)

###########################################
##LAB ##dplyr
Sales_by_country <- read.csv("./Superstore Sales Data/Sales_by_country_v1.csv")
names(Sales_by_country)

#Example-1
new_data1 <- Sales_by_country %>%
  select(custId , custName, custCountry,salesChannel) %>%
  filter(salesChannel=="Online")

head(new_data1)

#Example-2
new_data2 <- Sales_by_country %>%
  select(custId , custName, custCountry,salesChannel, unitsSold) %>%
  filter(salesChannel=="Online")%>%
  group_by(custCountry)%>%
  summarise(units_sold_by_country= sum(unitsSold))%>%
  arrange(units_sold_by_country)

head(new_data2,10)

#########################################################
###Feature re-engineering 

bank<-read.csv("./Bank Tele Marketing/bank_market.csv")
dim(bank)
names(bank)

###Creating dummy variables
table(bank$marital)

####Creating dummy variables
bank1 <- bank %>%
  mutate(marital1 = as.numeric(marital=="married")) %>%
  mutate(marital2 = as.numeric(marital=="single")) %>%
  mutate(marital3 = as.numeric(marital=="divorced")) 

head(bank1)







