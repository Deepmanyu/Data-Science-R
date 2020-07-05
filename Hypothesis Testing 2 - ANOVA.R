####### HYPOTHESIS TESTING IN R ############
#determine whether there is any significant difference in the diameter of the cutlet between two units. 

#installing packages
install.packages("dplyr")
library(dplyr)
install.packages("ggpubr")
library(ggpubr)
library(car)


#########################################
#### Reading the csv file and EDA #######
#########################################
lab_test <- read.csv("/Users/deepmanyusuhag/Desktop/Analytics/excelR - 2/Assignments/Hypothesis Testing/LabTAT.csv")
lab_test
names(lab_test)
summary(lab_test)
boxplot(lab_test, horizontal = TRUE, axes = TRUE)

# manuplating the dataset
lab_test2 <- stack(lab_test) #stacking all columns into one
lab_test2
names(lab_test2)
colnames(lab_test2)[2] = "lab_indicator" #changing column name 
names(lab_test2)

#########################################
#### Anova hypothesis test #######
#########################################
# Null hypothesis: the means of the different groups are the same
# Alternative hypothesis: At least one sample mean is not equal to the others.

# show a random sample of the data
set.seed(1234)
sample_n(lab_test2, 10)

#show levels
levels(lab_test2$lab_indicator)

#summary statistics by lab_indicator - compute count, mean, sd
group_by(lab_test2, lab_indicator) %>% 
  summarise(
    count = n(),
    mean = mean(values, na.rm = TRUE), 
    sd = sd(values, na.rm = TRUE)
  )

# Box plots again
boxplot(values ~ lab_indicator, data = lab_test2,
        xlab = "Labs", ylab = "Values",
        frame = FALSE, col = c("#00AFBB", "#E7B800", "#FC4E07", "#BB05DE"))

#Box plots using ggpubr
# Plot weight by group and color by group
ggboxplot(lab_test2, x = "lab_indicator", y = "values", 
          color = "lab_indicator", palette = c("#00AFBB", "#E7B800", "#FC4E07", "#BB04E5"),
          order = c("Laboratory.1", "Laboratory.2", "Laboratory.3", "Laboratory.4"),
          ylab = "Values", xlab = "Labs")

# Mean plots
# ++++++++++++++++++++
# Plot weight by group
# Add error bars: mean_se
# (other values include: mean_sd, mean_ci, median_iqr, ....)
ggline(lab_test2, x = "lab_indicator", y = "values", 
       add = c("mean_se", "jitter"), 
       order = c("Laboratory.1", "Laboratory.2", "Laboratory.3", "Laboratory.4"),
       ylab = "Values", xlab = "Labs")

# compute the analysis of variance
result.labs <- aov(values ~ lab_indicator, data = lab_test2)

# summary of results - 
# p-value is less than the significance level 0.05 
# we can conclude that there are significant differences between the groups
# cannot reject the null hypothesis : means of groups are same
summary(result.labs)


# to determine difference of mean of labs in pairs
# we use Tukeys multiple pair wise comparisions
# Tukey HSD (Tukey Honest Significant Differences)
# difference between all groups is significant except lab2-lab1

TukeyHSD(result.labs)



#########################################
#### Checking assumptions of anova #######
#########################################

# 1. Homogeneity of variances - plots and levene test
plot(result.labs , 1)

leveneTest(values ~ lab_indicator, data = lab_test2)

# 2. Normality - plots and Shapiro-WilK test
plot(result.labs, 2)
aov_residuals <- residuals(object = result.labs)
shapiro.test(x = aov_residuals)







