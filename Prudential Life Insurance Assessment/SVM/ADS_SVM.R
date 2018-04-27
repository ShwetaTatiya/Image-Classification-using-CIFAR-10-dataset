#Installing all needed packages

install.packages("hydroGOF")
library(hydroGOF)
library(e1071)
library(caret)
library(forecast)
library(ggplot2)


#Loading dataset
final_data <- read.csv(file="/Users/shwetatatiya/Desktop/SEMESTER_2_SHWETA/ADS/Prudential Life Insurance Assessment/CleanedData.csv", header=TRUE, sep= ',')
train<-final_data[1:500,]
test<-final_data[501:800,]


#SVM REGRESSION
############################################Radial Kernel#######################################################

#Radial Kernel
radial<-tune(svm, Response~.,kernel ="radial", data= train, ranges=list(cost=10^(-2:2),
                                                                        gamma=10^(-2:2)))
#gamma=10^(-2:2), cost=10^(-2:2)))
summary(radial)


#Applying SVM with the best parameters we found after tuning. 
radial_model = svm(Response ~ ., kernel = "radial", cost =1 ,gamma=0.01, data = train, scale = F)
radial_predictions <-  predict(radial_model, test[-39])
accuracy(radial_predictions,test$Response)

############################################Linear Kernel#######################################################

#Linear Kernel
linear<-tune(svm, Response~.,kernel ="linear", data= train, ranges=list(
  gamma=10^(-2:2),cost=10^(-2:2)))
summary(linear)


#Applying SVM with the best parameters we found after tuning. 
linear_model = svm(Response ~ ., kernel = "linear", cost =0.01 ,gamma=0.01, data = train, scale = F)
linear_predictions <-  predict(linear_model, test[-39])
accuracy(linear_predictions,test$Response)

############################################Sigmoid Kernel#######################################################

sigmoid<-tune(svm, Response~.,kernel ="sigmoid", data= train, ranges=list(gamma=10^(-2:2),cost=10^(-2:2)))
#gamma=10^(-2:2), cost=10^(-2:2)))
summary(sigmoid)


#Applying SVM with the best parameters we found after tuning. 
sigmoid_model = svm(Response ~ ., kernel = "sigmoid", cost =1 ,gamma=0.01, data = train, scale = F)
sigmoid_predictions <-  predict(sigmoid_model, test[-39])
accuracy(sigmoid_predictions,test$Response)

############################################Polynomial Kernel#######################################################

#Polynomial kernel
polynomial<-tune(svm, Response~.,kernel ="polynomial", data= train, ranges=list(gamma=10^(-1:1),cost=10^(-1:1)))
#gamma=10^(-2:2), cost=10^(-2:2)))
summary(polynomial)

#Applying SVM with the best parameters we found after tuning. 
polynomial_model = svm(Response ~ ., kernel = "polynomial", cost =0.1 ,gamma=0.1, data = train, scale = F)
polynomial_predictions <-  predict(polynomial_model, test[-39])
accuracy(predictions3,test$Response)


