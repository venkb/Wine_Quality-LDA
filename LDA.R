library(caTools) #to split into training and test data sets
library(MASS) #to build linear disriminants

#load data
setwd('C:\\Venkat\\Github\\Wine_Quality-LDA')
dataset = read.csv('Wine.csv')

#split into training and test data sets
set.seed(123)
split = sample.split(Y = dataset$Customer_Segment,
                     SplitRatio = 0.8)
training_set = subset(dataset, split == TRUE)
test_set = subset(dataset, split==FALSE)

#scale the continuous variables
training_set[,-14] = scale(training_set[,-14])
test_set[,-14] =  scale(test_set[,-14])

#applying LDA
lda = lda(formula = Customer_Segment ~ .,
          data = training_set)

#when you predict using lda you will get a matrix. so we need to convert it into data frame
training_set_lda = as.data.frame(predict(lda, training_set))
training_set_lda = training_set_lda[,c(5,6,1)]

test_set_lda = as.data.frame(predict(lda, test_set))
test_set_lda = test_set_lda[,c(5,6,1)]

#build the linear svm classifier using training set
classifier_svm = svm(formula = class ~ .,
                     data = training_set_lda,
                     type = 'C-classification',
                     kernel = 'linear')

#predict the class in test set using linear svm classifier
y_pred = predict(classifier_svm,
                 newdata = test_set_lda[,-3])

#build the confusion matrix
cm = table(test_set_lda[,3], y_pred)
cm
