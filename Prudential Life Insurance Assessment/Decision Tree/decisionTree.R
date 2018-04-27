library(readr)
library(tree)

myDataCleainingFuntion<-function(link){
  myData <- read.csv( file = link,
                      colClasses=c(Product_Info_1='factor', Product_Info_2='factor', Product_Info_3='factor', Product_Info_5='factor', 
                                   Product_Info_6='factor', Product_Info_7='factor', Employment_Info_2='factor', Employment_Info_3='factor', 
                                   Employment_Info_5='factor', InsuredInfo_1='factor', InsuredInfo_2='factor', InsuredInfo_3='factor', InsuredInfo_4='factor', 
                                   InsuredInfo_5='factor', InsuredInfo_6='factor', InsuredInfo_7='factor', Insurance_History_1='factor', 
                                   Insurance_History_2='factor', Insurance_History_3='factor', Insurance_History_4='factor', 
                                   Insurance_History_7='factor', Insurance_History_8='factor', Insurance_History_9='factor', Family_Hist_1='factor', 
                                   Medical_History_2='factor', Medical_History_3='factor', Medical_History_4='factor', Medical_History_5='factor', Medical_History_6='factor',
                                   Medical_History_7='factor', Medical_History_8='factor', Medical_History_9='factor', Medical_History_11='factor', 
                                   Medical_History_12='factor', Medical_History_13='factor', Medical_History_14='factor', Medical_History_16='factor', Medical_History_17='factor', 
                                   Medical_History_18='factor', Medical_History_19='factor', Medical_History_20='factor', Medical_History_21='factor', Medical_History_22='factor', 
                                   Medical_History_23='factor', Medical_History_25='factor', Medical_History_26='factor', Medical_History_27='factor', Medical_History_28='factor', 
                                   Medical_History_29='factor', Medical_History_30='factor', Medical_History_31='factor', Medical_History_33='factor', Medical_History_34='factor', 
                                   Medical_History_35='factor', Medical_History_36='factor', Medical_History_37='factor', Medical_History_38='factor', Medical_History_39='factor', 
                                   Product_Info_4='numeric', Ins_Age='numeric', Ht='numeric', Wt='numeric', BMI='numeric', Employment_Info_1='numeric', Employment_Info_4='numeric', Employment_Info_6='numeric', 
                                   Insurance_History_5='numeric', Family_Hist_2='numeric', Family_Hist_3='numeric', Family_Hist_4='numeric', Family_Hist_5='numeric',
                                   Response='factor',
                                   Medical_History_1='integer', Medical_History_10='integer', Medical_History_15='integer', Medical_History_24='integer', Medical_History_32='integer'))
  
  myData <- transform( myData,
                       count = Medical_Keyword_1+Medical_Keyword_2+Medical_Keyword_3+Medical_Keyword_4+
                         Medical_Keyword_5+Medical_Keyword_6+Medical_Keyword_7+Medical_Keyword_8+
                         Medical_Keyword_9+Medical_Keyword_10+Medical_Keyword_11+Medical_Keyword_12+
                         Medical_Keyword_13+Medical_Keyword_14+Medical_Keyword_15+Medical_Keyword_16+Medical_Keyword_17+
                         Medical_Keyword_18+Medical_Keyword_19+Medical_Keyword_20+Medical_Keyword_21+
                         Medical_Keyword_22+Medical_Keyword_23+Medical_Keyword_24+Medical_Keyword_25+
                         Medical_Keyword_26+Medical_Keyword_27+Medical_Keyword_28+Medical_Keyword_29+
                         Medical_Keyword_30+Medical_Keyword_31+Medical_Keyword_32+Medical_Keyword_33+
                         Medical_Keyword_34+Medical_Keyword_35+Medical_Keyword_36+Medical_Keyword_37+
                         Medical_Keyword_38+Medical_Keyword_39+Medical_Keyword_40+Medical_Keyword_41+
                         Medical_Keyword_42+Medical_Keyword_43+Medical_Keyword_44+Medical_Keyword_45+
                         Medical_Keyword_46+Medical_Keyword_47+Medical_Keyword_48)
  
  myData[,"Product_Info_2_char"]<-as.factor(substr(myData[,3],0,1))
  myData[,"Product_Info_2_num"]<-as.factor(substr(myData[,3],2,2))
  myData[,"BMI_Age"]<-myData[,"BMI"]*myData[,"Ins_Age"]
  for ( i in 1:nrow(myData))
    if(is.na(myData[i,13]))
      myData[i,13]=0
  summary(myData$Employment_Info_1)
  for ( i in 1:nrow(myData))
    if(is.na(myData[i,16]))
      myData[i,16]=0
  summary(myData$Employment_Info_1)
  myZeroAdjustement <- function(Data,value,index){
    for ( i in 1:nrow(Data)){
      if(is.na(Data[i,index])){
        Data[i,index]=value
      }
    }
    return(Data)
  }
  myData<-myZeroAdjustement(myData,0.362,18)
  summary(myData$Employment_Info_6)
  #filling null values of insurance history 5
  myData<-myZeroAdjustement(myData,0,30)
  summary(myData$Insurance_History_5)
  #filling null values family history 2
  myData<-myZeroAdjustement(myData,0.464,35)
  #filling null values family history 3
  myData<-myZeroAdjustement(myData,0.52,36)
  #family history 4
  myData<-myZeroAdjustement(myData,0.445,36)
  #setting family history 5 as null
  myData$Family_Hist_5<-NULL
  #setting medical history 1 NA as 7.962
  myData<-myZeroAdjustement(myData,7.962,39)
  #setting medical null all other 
  myData$Medical_History_10<-NULL
  myData$Medical_History_15<-NULL
  myData$Medical_History_24<-NULL
  myData$Medical_History_32<-NULL
  myData$Id<-NULL
  myData[is.na(myData)]<-0
  #removing medical keyword 1 to 48
  myData<-myData[,-(38:121),drop=FALSE]
  myData$Product_Info_2_char<-as.factor(myData$Product_Info_2_char)
  myData$Product_Info_2_num<-as.factor(myData$Product_Info_2_num)
  return (myData)
}



CleanedData <- myDataCleainingFuntion("D:/subjects/Sping2018/IFO7800DataSciences/MidTermProject/train.csv")
View(CleanedData)

library(forcats)
CleanedData$Product_Info_3 <- fct_collapse(CleanedData$Product_Info_3, '0' = c('1','11','12','13','16','17','18','19','2','20','21','22','23','24','27','28','3','30','32','33','34','36','38','4','5','6','8','9'))
CleanedData$Employment_Info_2 <- fct_collapse(CleanedData$Employment_Info_2, '0' = c('13','16','17','18','19','2','20','21','22','23','25','26','27','28','29','30','31','33','34','35','36','37','38','4','5','6','7'))
summary(CleanedData$Product_Info_3)
summary(CleanedData$Employment_Info_2)
summary(CleanedData)



#59321 entries we have, randomly choose 20% as testing data.
testing_index <- sample(59381, 59381*0.2)

#partitioning
training_data <- CleanedData[-testing_index,]
testing_data <- CleanedData[testing_index,]

#see attributes' names of our data
head(training_data)

testing_Response <- testing_data$Response

#fit a tree based on training data

tree_model <- tree(Response~., training_data)

plot(tree_model)
text(tree_model, pretty = 0)

#check how the model is doing using the testing dataset
tree_pred <- predict(tree_model, testing_data, type = "class")

library(caret)
confusionMatrix(testing_Response,tree_pred)

#cross validation for pruning the tree
cv_tree <- cv.tree(tree_model)
names(cv_tree)
plot(cv_tree$size,
     cv_tree$dev,
     type = "b",
     xlab = "Tree Size",
     ylab = "MSE") #Mean Square Error

which.min(cv_tree$dev)
cv_tree$size[1]

#prune the tree
pruned_model <- prune.tree(tree_model, best = 5)

plot(pruned_model)
text(pruned_model)

#check the accuracy of the model using testing data
tree_pred <- predict(pruned_model, testing_data, type = "class")
confusionMatrix(testing_Response, tree_pred)

ans <- as.numeric(testing_Response)
pred <- as.numeric(tree_pred)
sqrt(mean((ans - pred)^2))




