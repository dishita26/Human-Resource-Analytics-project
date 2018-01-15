# Importing the dataset #
HR_analytics <- read_excel("C:/Users/dishita/Desktop/Data Mining/project/HR_comma_sep.xlsx")


#Exploring Dataset#
str(HR_analytics)
summary(HR_analytics)

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


#Fitting SVM to training set #
library('e1071')
classifier = svm(formula = left ~ ., 
                 data = training_set,
                 type = 'C-classification',
                 kernel = 'radial')

#Predicting the Test set results #
y_pred = predict(classifier, newdata=test_set[-7])

#Making the confusion matrix #
library('caret')
cm = table(test_set$left, reference = y_pred)
accuracy = ((cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2]))

