#install libraries
library(ggplot2)
library(dplyr)
library(stargazer)
library(nnet)
#install.packages("neuralnet")
library(neuralnet)
library(tree)
library(MASS)
library(psych)
library(randomForest)
library(rpart)
#load data
hotel_bookings <- read.csv("./train.csv")

## ======================== Decision Tree ========================
#split data into train and test
set.seed(123)
train_index <- sample(1:nrow(hotel_bookings), 0.8*nrow(hotel_bookings))
train <- hotel_bookings[train_index,]
test <- hotel_bookings[-train_index,]
#build decision tree
tree_model_big <- rpart(booking_status ~ ., data = train, method = "class",control = rpart.control(cp = 0))
tree_model<-tree(booking_status ~ ., data = train, method = "class")
plotcp(tree_model_big,upper = c("size"))
#the best tree has ~156 leaves
tree_156<-prune(tree_model_big,cp=0.00024)
#tree depth and leaves
nodes <- as.numeric(rownames(tree_156$frame))
max(rpart:::tree.depth(nodes))
sum(tree_156$frme$var == "<leaf>")
#predicta
tree_156$y<-test$booking_status
tree_156$yhat <- predict(tree_156, test, type = "class")
#accuracy
mean(tree_156$y == tree_156$yhat)
#The pruned decision tree has an accuracy of 81.8%
#test of overfitting
tree_156$y_train<-predict(tree_156, train, type = "class")
mean(tree_156$y_train == train$booking_status)
#this model is not over fitting

## ======================== Random Forest ========================
#build random forest

rf_model <- randomForest(booking_status ~ ., data = train, importance = TRUE, ntree = 100,maxdepth = 5)
rf_model.train_predit = predict(rf_model,train)
rf_model.predict = predict(rf_model,test)

prop.table(table(rf_model.train_predit>0.5,train$booking_status))
#train accuracy is 98.254%
prop.table(table(rf_model.predict>0.5,test$booking_status))
#test accuracy is 82.447%
#random forest is overfitting no matter the tree depth or number of trees
#this may not be the best model for this data set

