library(ranger)
library(Boruta)

HR_analytics$left<-as.factor(HR_analytics$left)
boruta.train <- Boruta(left~., data = HR_analytics, doTrace = 2)

print(boruta.train)
plot(boruta.train, xlab = "", xaxt = "n")

lz<-lapply(1:ncol(boruta.train$ImpHistory),function(i)
  boruta.train$ImpHistory[is.finite(boruta.train$ImpHistory[,i]),i])
names(lz) <- colnames(boruta.train$ImpHistory)
Labels <- sort(sapply(lz,median))
axis(side = 1,las=2,labels = names(Labels),
     at = 1:ncol(boruta.train$ImpHistory), cex.axis = 0.7)

#intepretation #
# Summary: With all of this information, this is what Bob should know about his company and why his employees probably left:
#   
#   Employees generally left when they are underworked (less than 150hr/month or 6hr/day)
# Employees generally left when they are overworked (more than 250hr/month or 10hr/day)
# Employees with either really high or low evaluations should be taken into consideration for high turnover rate
# Employees with low to medium salaries are the bulk of employee turnover
# Employees that had 2,6, or 7 project count was at risk of leaving the company
# Employee satisfaction is the highest indicator for employee turnover.
# Employee that had 4 and 5 yearsAtCompany should be taken into consideration for high turnover rate