## Data pre-processing ##

# Importing the dataset##
library(readxl)
HR_analytics<- read_excel("C:/Users/dishita/Desktop/Data Mining/project/HR_comma_sep.xlsx")
HR_analytics$sales<- NULL

##Encoding the categorical variables as factors##
HR_analytics$salary <- as.numeric(factor(HR_analytics$salary,
                                  levels = c('low','medium','high'),
                                  labels = c(1,2,3)))

##Splitting the dataset into training set and test set ##
library(caTools)
set.seed(123)
split <- sample.split(HR_analytics$left, SplitRatio = 0.8)
training_set <- subset(HR_analytics, split == TRUE)
test_set <- subset (HR_analytics, split == FALSE)

##Feature scaling ##
training_set[-7] <- scale(training_set[-7]) 
test_set[-7] <- scale (test_set[-7])

##Fitting ANN to training set ###
library(h2o)
h2o.init(nthreads = -1, max_mem_size = "2g", ip = "127.0.0.1", port= 50001) 
