# Importing the dataset #
HR_imp_features <- read_excel("C:/Users/dishita/Desktop/Data Mining/project/HR_imp_features.xlsx")

#Splitting the dataset into training and test set #
library('caTools')
set.seed(123)
split <- sample.split(HR_imp_features$left,SplitRatio = 0.8)
training_set <- subset(HR_imp_features, split==TRUE)
test_set <- subset(HR_imp_features, split==FALSE)

#Feature Scaling #
training_set[-6] = scale(training_set[-6])
test_set [-6] = scale(test_set[-6])

#K-fold Cross Validation#
library(caret)
library(rpart)
folds = createFolds(training_set$left, k = 5)
cv = lapply(folds, function(x){
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  classifier = rpart(formula = left ~ . ,
                     data = training_fold)
  y_pred = predict(classifier, newdata=test_fold[-6])
  prediction <- ifelse(y_pred>0.5,1,0)
  cm = table(test_fold$left,prediction)
  accuracy = ((cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2]))
  return(accuracy)
})
accuracy = mean(as.numeric(cv)) 