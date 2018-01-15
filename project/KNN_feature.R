################################### KNN_model ############################################################## 

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

#Fitting the KNN model & k-fold cross validation #
library(caret)
library(class)
folds = createFolds(training_set$left, k = 5)
cv = lapply(folds, function(x){
  training_fold = training_set[-x, ]
  test_fold = training_set[x, ]
  y_pred = knn(train = training_fold[, -6,drop=FALSE],
               test = test_fold[,-6,drop=FALSE],
               cl = training_fold$left,
               k = 5)
  cm = table(test_fold$left,y_pred)
  accuracy = ((cm[1,1]+cm[2,2])/(cm[1,1]+cm[1,2]+cm[2,1]+cm[2,2]))
  return(accuracy)
})
accuracy = mean(as.numeric(cv))

