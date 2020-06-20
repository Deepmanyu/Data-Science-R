####### HYPOTHESIS TESTING IN R ############
#determine whether there is any significant difference in the diameter of the cutlet between two units. 


#########################################
#### Reading the csv file and EDA #######
#########################################
cutlets <- read.csv("/Users/deepmanyusuhag/Desktop/Analytics/excelR - 2/Assignments/Hypothesis Testing/Cutlets.csv")
names(cutlets)
summary(cutlets)
boxplot(cutlets, horizontal = TRUE, axes = TRUE)

#strip charts 
stripchart(cutlets$Unit.A,method="jitter", #methods - "stack" and "fitter"
           vertical = FALSE, 
           main='StripChart - Unit.A',
           xlab='Diameter')
#histograms
hist(cutlets$Unit.A, 
     main="Histogram for Unit.A", # title of histogram
     xlab="Cutlet diameter", # x-axis label
     border="blue", # border of the histogram bars
     col="green", # color fill fo the histogram bars
     xlim=c(min(cutlets$Unit.A),7.6), # change range of x axis
     las=1, # for rotating labels. Values are 0,1,2,3.
     breaks=5) # to control number of breaks
lines(density(cutlets$Unit.A))
stripchart(cutlets$Unit.A, add = TRUE, at = 10)

#histogram using ggplot
ggplot(data=cutlets, aes(x=cutlets$Unit.B)) + 
  geom_histogram(breaks=seq(6, max(cutlets$Unit.B), by=.2), 
                 col="red", 
                 fill="green", 
                 alpha = .2) + # alpha is transparency - 0 (fully transparent) and 1 (opaque)
  labs(title="Histogram for Unit.B", x="Cutlet Diameter", y="Frequency") + 
  xlim(c(min(cutlets$Unit.B),max(cutlets$Unit.B))) + 
  ylim(c(0,15))



#Scatter Plots
plot(cutlets$Unit.A)

#Normal QQ Plots
qqnorm(cutlets$Unit.A)





#########################################
#### Test for normality #######
#########################################
library("car")
#visual inspection
qqPlot(cutlets$Unit.A)
qqPlot(cutlets$Unit.B)
#shapiro wilk test
shapiro.test(cutlets$Unit.A)
shapiro.test(cutlets$Unit.B)

#########################################
#### Test for same variances  #######
#########################################

res.ftest <- var.test(cutlets$Unit.A, cutlets$Unit.B, alternative = "two.sided")
res.ftest

#########################################
#### Two sample t-test #######
#########################################

res_ttest <- t.test(cutlets$Unit.A, cutlets$Unit.B, var.equal = TRUE)
res_ttest
