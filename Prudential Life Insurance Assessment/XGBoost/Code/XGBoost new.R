install.packages("readr")
install.packages("xgboost")

library(readr)
library(xgboost)
library(forecast)

trainData <- read.csv("/Users/shwetatatiya/Desktop/SEMESTER_2_SHWETA/ADS/Prudential Life Insurance Assessment/XGBoost/Data/CleanedData (1).csv")

#Divinding our dataset into train(80%) and test(20%) 
set.seed(123)
index = sample(seq_len(nrow(trainData)), size = floor(0.8*nrow(trainData)))
train <- trainData[index, ]
test <- trainData[-index, ]


#Removing the ID column from the dataframe- in train

Colum_names <- names(train)[2:(ncol(train)-1)]
Colum_names

#Through manual observation, its being observed that all days, 
#assuming text variables are categorical & replacing them with numeric values in 
#both the train and test dataframe

for (i in Colum_names) {
  if (class(train[[i]])=="character") {
    levels <- unique(c(train[[i]], test[[i]]))
    train[[i]] <- as.integer(factor(train[[i]], levels=levels))
  }
}

#Converting Categorized features to numeric

for (i in Colum_names) {
  if (class(train[[i]])=="character") {
    levels <- unique(c(train[[i]], test[[i]]))
    test[[i]]  <- as.integer(factor(test[[i]],  levels=levels))
    
  }
}

#Eliminating Response variable
Colum_names1 <- Colum_names[-38]

#Starting XGBoost for the train data


XgData <- xgboost(data = data.matrix(train[,Colum_names1]), 
                  label = train$Response,
                  eta = 0.9,
                  num_class=10,
                  nrounds = 15,
                  objective = "multi:softmax",
                  eval_metric = "merror")

#Test Accuracy
test
test1 <- test[-39]
pred <- predict(XgData, newdata = data.matrix(test1))
acc<-which(test$Response-pred==0)
length(acc)/length(test$Response)
re <- test$Response
rmse <- function(matpred, resp)
{
  sqrt(mean((matpred-resp)^2))
}
rmse(pred, re)


##accuracy(PredictData$Response2,test$Response)
###############ME     RMSE      MAE       MPE     MAPE
#Test set 0.01660166 1.892765 1.353535 -36.39322 57.41654
#The function accuracy gives you multiple measures of accuracy of the model fit:
#mean error (ME), root mean squared error (RMSE), mean absolute error (MAE), 
#mean percentage error (MPE), mean absolute percentage error (MAPE),
#mean absolute scaled error (MASE) and the first-order autocorrelation coefficient (ACF1).
#It is up to you to decide, based on the accuracy measures, whether you consider this a
#good fit or not. For example, mean percentage error of nearly -70% does not look good to
#me in general, but that may depend on what your series are and how much predictability you 
#may realistically expect.

#Plotting the model
model <- xgb.dump(XgData, with_stats = T)
model[1:10] #This statement prints top 10 nodes of the model
# Compute feature importance matrix
importance_matrix <- xgb.importance(Colum_names1, model = XgData)
# Nice graph
xgb.plot.importance(importance_matrix[1:20,])

#cc<-importance_matrix[,2]
#In case last step does not work for you because of a version issue, you can try following :
#barplot(cc)

#Testing our model
test<- chisq.test(test$Response,pred)
print(test)


