# Importing the dataset #
HR_analytics <-  read_excel("C:/Users/dishita/Desktop/Data Mining/project/HR_comma_sep.xlsx")

#converting categorical variables into factors#
HR_analytics$salary <- factor(HR_analytics$salary,
                              levels = c('low','medium','high'),
                              labels = c(1,2,3) )
HR_analytics$salary <- as.numeric(HR_analytics$salary)
HR_analytics$sales <- factor(HR_analytics$sales,
                             levels = c('accounting', 'hr','IT','management','marketing',
                                        'product_mng','RandD','sales','support','technical'),
                             labels = c(1,2,3,4,5,6,7,8,9,10))
HR_analytics$sales <- as.numeric(HR_analytics$sales)

#Splitting the dataset into training and test set #
library('caTools')
set.seed(123)
split <- sample.split(HR_analytics$left,SplitRatio = 0.8)
training_set <- subset(HR_analytics, split==TRUE)
test_set <- subset(HR_analytics, split==FALSE)

#Feature Scaling #
training_set[-7] = scale(training_set[-7])
test_set [-7] = scale(test_set[-7])

classifier = rpart(formula = left ~ . ,
                   data = training_set)

y_pred = predict(classifier, newdata=test_set[-7])
prediction <- ifelse(y_pred>0.5,1,0)
cm = table(test_set$left,prediction)

#K-fold Cross Validation#
library(caret)
library(rpart)
folds = createFolds(training_set$left, k = 5)
cv = lapply(folds, function(x){
   training_fold = training_set[-x, ]
   test_fold = training_set[x, ]
   classifier = rpart(formula = left ~ . ,
                      data = training_fold)
   y_pred = predict(classifier, newdata=test_fold[-7])
   prediction <- ifelse(y_pred>0.5,1,0)
   cm = table(test_fold$left,prediction)
   accuracy = ((cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2]))
   return(accuracy)
})
accuracy = mean(as.numeric(cv)) 

