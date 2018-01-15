################################### KNN_model ############################################################## 

# Importing the dataset #
HR_analytics <- read_excel("C:/Users/dishita/Desktop/Data Mining/project/HR_comma_sep.xlsx")


#Exploring Dataset#
str(HR_analytics)
summary(HR_analytics)
hist(HR_analytics$sales)

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

 
# #Fitting KNN to the Train set and Predicting the test set results#
library(class)
y_pred = knn(train = training_set[, -7,drop=FALSE],
             test = test_set[,-7,drop=FALSE],
             cl = training_set$left,
             k = 5)
cm = table(test_set$left,y_pred)

#Fitting the KNN model & k-fold cross validation #
library(caret)
library(class)
folds = createFolds(training_set$left, k = 5)
cv = lapply(folds, function(x){
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  y_pred = knn(train = training_fold[, -7,drop=FALSE],
           test = test_fold[,-7,drop=FALSE],
           cl = training_fold$left,
           k = 5)
  cm = table(test_fold$left,y_pred)
  accuracy = ((cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2]))
  return(accuracy)
})
accuracy = mean(as.numeric(cv))

# #confusion matrix#
# library('caret')
# cm = confusionMatrix(test_set$left, reference = y_pred)
# accuracy = (cm[1,1]+cm[2,2])/14999
# Sensitivity = sensitivity(test_set$left, reference = y_pred)
# Specificity = specificity(test_set$left, reference = y_pred)
# 
# y_pred = as.numeric(y_pred)


            
