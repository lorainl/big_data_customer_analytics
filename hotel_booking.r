#install libraries
library(ggplot2)
library(ggpubr)
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
library(viridis)
library(readxl)
library(Hmisc)
library(mfx)
library(rcompanion)

#load data
hotel_bookings <- read.csv("./train.csv")
hotel_booking<-drop_na(hotel_bookings)
head(hotel_bookings)

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

## ======================== Logistic Regression ========================
# simple data exploration
str(hotel_bookings)

# plot booking status by room type
ggplot(hotel_bookings, aes(x = as.factor(no_of_children),y=booking_status)) +
    stat_summary(fun = "mean", geom = "bar")+ 
    labs(title = "Percentage of confirmed bookings by family", x = "Number of Children", y = "Percentage Confirmed")+
    theme(plot.title = element_text(hjust = 0.5,size=32),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)
            )
#looks like the sample size of more children is small, we cannot make a conclusion here

# plot booking status by year
ggplot(hotel_bookings, aes(x = as.factor(arrival_year),y=booking_status)) +
    stat_summary(fun = "mean", geom = "bar")+ 
    labs(title = "Percentage of confirmed bookings by year", x = "Year", y = "Percentage Confirmed")+
    theme(plot.title = element_text(hjust = 0.5,size=32),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)
            )
# plot booking status by month
ggplot(hotel_bookings, aes(x = as.factor(arrival_month),y=booking_status)) +
    stat_summary(fun = "mean", geom = "bar")+ 
    labs(title = "Percentage of confirmed bookings by month", x = "Month", y = "Percentage Confirmed")+
    theme(plot.title = element_text(hjust = 0.5,size=32),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)
            )
# there is seasonality in hotel booking and cancellation

# plot booking status by room type
ggplot(hotel_bookings, aes(x = as.factor(room_type_reserved),y=booking_status)) +
    stat_summary(fun = "mean", geom = "bar")+ 
    labs(title = "Percentage of confirmed bookings by room type", x = "Room Type", y = "Percentage Confirmed")+
    theme(plot.title = element_text(hjust = 0.5,size=32),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)
            )
# there's some room type that are more likely to be cancelled

#plot booking status by weekend
ggplot(hotel_bookings, aes(x = as.factor(no_of_weekend_nights),y=booking_status)) +
    stat_summary(fun = "sum", geom = "bar")+ 
    labs(title = "Percentage of confirmed bookings by weekend", x = "Weekend", y = "Percentage Confirmed")+
    theme(plot.title = element_text(hjust = 0.5,size=32),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)
            )

ggplot(hotel_bookings, aes(x = as.factor(no_of_weekend_nights),y=booking_status)) +
    stat_summary(fun = "mean", geom = "bar")+ 
    labs(title = "Percentage of confirmed bookings by weekend", x = "Weekend", y = "Percentage Confirmed")+
    theme(plot.title = element_text(hjust = 0.5,size=32),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)
            )
#does not seem to have significant correlation

#plot booking status by required car parking
ggplot(hotel_bookings, aes(x = as.factor(required_car_parking_space),y=booking_status)) +
    stat_summary(fun = "mean", geom = "bar")+ 
    labs(title = "Percentage of confirmed bookings by required car parking", x = "Required Car Parking", y = "Percentage Confirmed")+
    theme(plot.title = element_text(hjust = 0.5,size=32),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)
            )
#those who required car parking are more likely to cancel, but number is small, we cannot say for sure

#plot average price and booking status
ggplot(hotel_bookings, aes(x = as.factor(booking_status),y=avg_price_per_room)) +
    geom_boxplot()+ 
    labs(title = "Average price by booking status", x = "Booking Status", y = "Average Price")+
    theme(plot.title = element_text(hjust = 0.5,size=32),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)
            )
#does not have significant difference

#plot lead time and booking status
ggplot(hotel_bookings, aes(x = as.factor(booking_status),y=lead_time)) +
    geom_boxplot()+ 
    labs(title = "Lead time by booking status", x = "Booking Status", y = "Lead Time")+
    theme(plot.title = element_text(hjust = 0.5,size=32),
            axis.title.x = element_text(size = 24),
            axis.title.y = element_text(size = 24)
            )
#seems like the lead time is longer for confirmed booking
cor.test(hotel_bookings$booking_status,hotel_bookings$repeated_guest)
#correlation is not significant
cor.test(hotel_bookings$booking_status,hotel_bookings$no_of_previous_cancellations)
#correlation is not significant
cor.test(hotel_bookings$booking_status,hotel_bookings$no_of_special_requests)
#some correlation but weak
cor.test(hotel_bookings$booking_status,hotel_bookings$avg_price_per_room)
#some correlation but weak
cor.test(hotel_bookings$booking_status,hotel_bookings$lead_time)
#some moderate correlation
#correlation matrix among all parameters
rcorr(as.matrix(hotel_bookings[,c("no_of_adults","no_of_children","no_of_weekend_nights","no_of_week_nights",
                                "type_of_meal_plan","required_car_parking_space","room_type_reserved","lead_time",
                                "arrival_month","market_segment_type","repeated_guest","no_of_previous_cancellations",
                                "avg_price_per_room","no_of_special_requests")]))
#some strong correlation exist between:
#no_of_children and market_segment_type
#no_of_weekend_nights and no_of_week_nights
#weekend_nights and repeated_guest
#meal_plan and room_type_reserved,number_of_children
#parking and segment_type,repeated guiests
#room_type_reserved and no of children and avg price
#repeated guest and market segment, previous cancellation (but this is expected?)
#avg price is correlated with number of visitors, room type, repeated guest(neg),special requests
# will drop average price, market segment, number of children for glm

##===================Logistic Regression===================
glm_model.all<-glm(booking_status~.-id, data=hotel_bookings, family=binomial)
stargazer(glm_model.all,type="text",no.space=TRUE)
glm_model.backward<-step(glm_model.all, direction="backward", trace=0)
stargazer(glm_model.backward,type="text",no.space=TRUE)
glm_model.minimal <- glm(booking_status ~ 1, data=hotel_bookings)
glm_model.forward<- step(glm_model.minimal, scope=list(upper = glm_model.all,lower= glm_model.minimal), direction="forward",trace=0)
stargazer(glm_model.forward,type="text",no.space=TRUE)
nagelkerke(glm_model.all)
nagelkerke(glm_model.backward)#this one gives the highest R^2, but is not great
nagelkerke(glm_model.forward)
#calculate mfx
backward_mfx<-logitmfx(booking_status~. -no_of_previous_bookings_not_canceled, data = hotel_bookings)
backward_mfx
#the most important variables are: room type (neg), market segment, number of special requests(neg), number of cancellations
