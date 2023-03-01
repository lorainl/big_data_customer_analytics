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