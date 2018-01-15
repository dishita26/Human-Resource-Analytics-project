# Importing the dataset #
HR_analytics <- read_excel("C:/Users/dishita/Desktop/Data Mining/project/HR_comma_sep.xlsx")

#Checking the overall summary#
summary(HR_analytics)

#Renaming dataset
library(plyr)
HR_analytics <- rename(HR_analytics, c("sales" = "roles"))
HR_analytics <- rename(HR_analytics, c("time_spend_company" = "exp_in_company"))
names(HR_analytics)[10]<-"salary"
head(HR_analytics)

#Check the type of dataset#
str(HR_analytics)

## NOTE: When performing cross validation, its important to maintain this turnover ratio
attrition<-as.factor(HR_analytics$left)
summary(attrition)

perc_attrition_rate<-sum(HR_analytics$left/length(HR_analytics$left))*100
#percentage of attrition
print(perc_attrition_rate) # 24% of the employee left, 73% of the employee stayed 

#Overview of summary (turnover vs non-turnover)
cor_vars<-HR_analytics[,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","exp_in_company","Work_accident","left"
                          ,"promotion_last_5years")]

aggregate(cor_vars[,c("satisfaction_level","last_evaluation","number_project","average_montly_hours","exp_in_company","Work_accident","promotion_last_5years")], 
          by=list(Category=cor_vars$left), FUN=mean)

# Correlation Matrix & Heatmap#
# Correlation Matrix 
library(reshape2)
library(ggplot2)
trans <- cor(cor_vars) 
#Heatmap 
melted_cormat <- melt(trans)
ggplot(data = melted_cormat, aes(x=Var1, y=Var2, fill=value)) + 
  geom_tile() +theme(axis.text.x = element_text(angle = 90, hjust = 1))


