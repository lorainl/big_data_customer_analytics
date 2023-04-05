## ===== install libraries =====
library(tidyverse)
library(ggpubr)
library(dplyr)
library(stargazer)
#install.packages("neuralnet")
library(neuralnet)
library(MASS)
library(psych)
library(viridis)
library(readxl)
library(Hmisc)
library(mfx)
library(rcompanion)

## ======================== Load Data ========================
hotel_bookings <- read.csv("./train.csv")
test <- read.csv("./test.csv")
hotel_booking<-drop_na(hotel_bookings)
head(hotel_bookings)
str(hotel_bookings)
head(test)

## ======================== Variable Exploration ========================
#id (not useful)
#no_of_adults - this may have too much noise
#no_of_children - this may infer type of stay and may impact the outcome
#no_of_weekend_nights, - this may infer type of stay and may impact the outcome
#no_of_week_nights, - this may infer type of stay and may impact the outcome
#type_of_meal_plan, - may not be indicative.. but will take a look at the ggplot before deciding
#required_car_parking_space, - may have too much noise
#room_type_reserved, - may be useful, but will look at ggplot before deciding
#lead_time, - very useful
#arrival_year, - only have two years, this is not indicative
#arrival_month, - may infer to seasonality and may impact the outcome
#arrival_date, - not useful
#market_segment_type, - not sure if this is useful
#repeated_guest,
#no_of_previous_cancellations, - useful
#no_of_previous_bookings_not_canceled,
#avg_price_per_room, - may be useful
#no_of_special_requests - may have too much noise

str(hotel_bookings)

#check for missing values
colSums(is.na(hotel_bookings))

#find correlation of single variables with booking status
#Uncertain variables
cor.test(hotel_bookings$booking_status,hotel_bookings$no_of_adults)
png("no_of_adults_VS_cancellation.png", width = 1200, height = 1200, units = "px", res = 300)
ggplot(hotel_bookings %>% 
        count(no_of_adults, booking_status) %>%
        group_by(no_of_adults) %>%
        mutate(pct = prop.table(n) * 100),
    aes(x=as.factor(no_of_adults),fill=as.factor(booking_status))) + 
    geom_col(aes(y=n)) +
    geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%"),y = n),
    position = position_stack(vjust=0.5), check_overlap = TRUE,size=3)+
    ggtitle("Cancellation distribution by number of adults") +
    xlab("Number of Adults") +
    ylab("Count") +
    guides(fill=guide_legend(title="Booking Status"))+
    theme(
    title = element_text(size = 8),
    axis.title = element_text(size = 8,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
    )
dev.off()
# correlation is very weak, 0.007
cor.test(hotel_bookings$booking_status,hotel_bookings$no_of_children)
# correlation is very weak, 0.004
cor.test(hotel_bookings$booking_status,hotel_bookings$no_of_weekend_nights)
# cor 0.0443
cor.test(hotel_bookings$booking_status,hotel_bookings$no_of_week_nights)
# cor 0.0585
png("no_of_week_nights_VS_cancellation.png", width = 1200, height = 1200, units = "px", res = 300)
ggplot(hotel_bookings %>% 
        count(type_of_meal_plan, booking_status) %>%
        group_by(type_of_meal_plan) %>%
        mutate(pct = prop.table(n) * 100),
    aes(x=as.factor(type_of_meal_plan), fill=as.factor(booking_status))) + 
    geom_bar(stat = "identity", aes(y = n)) +
    geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%"),y = n),
    position = position_stack(vjust=0.5), check_overlap = TRUE,size=3)+
    ggtitle("Cancellation distribution by type of meal plan") +
    xlab("Type of Meal Plan") +
    ylab("Count")+
    guides(fill=guide_legend(title="Booking Status"))+
    theme(
    title = element_text(size = 8),
    axis.title = element_text(size = 8,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
    )
dev.off()
# there can be a correlation but rather weak
prop.table(table(hotel_bookings$booking_status, hotel_bookings$required_car_parking_space>0.5))
# can predict 58.8%
png("required_car_parking_space_VS_cancellation.png", width = 1200, height = 1200, units = "px", res = 300)
ggplot(hotel_bookings %>% 
        count(room_type_reserved, booking_status) %>%
        group_by(room_type_reserved) %>%
        mutate(pct = prop.table(n) * 100),
    aes(x=as.factor(room_type_reserved), fill=as.factor(booking_status))) + 
    geom_bar(stat = "identity", aes(y = n)) +
    geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%"),y = n),
    position=position_stack(vjust=0.5),
    size = 8.5)+
    ggtitle("Cancellation distribution by room type reserved") +
    xlab("Room Type Reserved") +
    ylab("Count")+
    theme(axis.text = element_text(size = 17.5),
    axis.title = element_text(size = 20,face="bold")
    )
# cannot tell correlation
png("arrival_month_VS_cancellation.png", width = 1200, height = 1200, units = "px", res = 300)
ggplot(hotel_bookings %>% 
        count(arrival_month, booking_status) %>%
        group_by(arrival_month) %>%
        mutate(pct = prop.table(n) * 100),
    aes(x=as.factor(arrival_month), fill=as.factor(booking_status))) + 
    geom_bar(stat = "identity", aes(y = n)) +
    geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%"),y = n),
    position=position_stack(vjust=0.5),
    size = 1.5)+
    ggtitle("Cancellation distribution by arrival month") +
    xlab("Month") +
    ylab("Count")+
    guides(fill=guide_legend(title="Booking Status"))+
    theme(
    title = element_text(size = 8),
    axis.title = element_text(size = 8,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
    )
dev.off()
# could be a strong predictor
png("market_segment_VS_cancellation.png", width = 1200, height = 1200, units = "px", res = 300)
ggplot(hotel_bookings %>% 
        count(market_segment_type, booking_status) %>%
        group_by(market_segment_type) %>%
        mutate(pct = prop.table(n) * 100),
    aes(x=market_segment_type,
                fill=as.factor(booking_status))) + 
    geom_bar(stat = "identity", aes(y = n)) +
    geom_text(aes(label=paste0(sprintf("%1.1f", pct),"%"),y = n),
    position=position_stack(vjust=0.5),
    check_overlap = TRUE,
    size = 2.5)+
    ggtitle("Cancellation distribution by market segment") +
    xlab("Type of Room Reserved") +
    ylab("Count") +
    guides(fill=guide_legend(title="Booking Status"))+
    theme(
    title = element_text(size = 8),
    axis.title = element_text(size = 8,face="bold"),
    axis.text = element_text(size = 8),
    legend.title = element_text(size = 8),
    legend.text = element_text(size = 6)
    )
dev.off()
#could be a strong predictor
prop.table(table(hotel_bookings$booking_status, hotel_bookings$repeated_guest>0.5))
# can predict 61.3%
cor.test(hotel_bookings$booking_status,hotel_bookings$no_of_previous_cancellations)
# cor -0.0456
# this factor is not important
cor.test(hotel_bookings$booking_status,hotel_bookings$no_of_previous_bookings_not_canceled)
#cor -0.0801
# this factor is not important
cor.test(hotel_bookings$booking_status,hotel_bookings$no_of_special_requests)
# cor -0.22 (this is a strong indicator)
# the more special requests, the more likely it will be cancelled
cor.test(hotel_bookings$booking_status,hotel_bookings$avg_price_per_room)
# cor 0.1575 (this is a moderate indicator)
#the higher the price, the less likely it will be cancelled
cor.test(hotel_bookings$booking_status,hotel_bookings$lead_time)
# cor 0.3749 (this is a strong indicator)
#seems like the lead time is longer for confirmed booking

## ======================== Choice of Variable========================
#no_of_weekend_nights
#no_of_week_nights
#type_of_meal_plan
#lead_time
#arrival_month
#market_segment_type
#repeated_guest
#avg_price_per_room
#no_of_special_requests

##===================Check Collinearity===================
cor_matrix<-rcorr(as.matrix(hotel_bookings[,c("no_of_weekend_nights","no_of_week_nights","lead_time","avg_price_per_room","type_of_meal_plan","arrival_month","market_segment_type","repeated_guest","no_of_special_requests")]))
cor_matrix
#A high correlation between: market segment & special reuests, market segment & repeated guest, market segment & no of special request, repeated guest & average price per room, repeated guest & lead time. 
#potential options: 
#1) remove repeated guest - since there is potentially a market segment calle drepeated guest/business travellers
hotel_bookings$mkt_repeat_guest<-0.1*hotel_bookings$repeated_guest+hotel_bookings$market_segment_type
cor_matrix<-rcorr(as.matrix(hotel_bookings[,c("no_of_weekend_nights","no_of_week_nights","lead_time","avg_price_per_room","type_of_meal_plan","arrival_month","mkt_repeat_guest","no_of_special_requests")]))
cor_matrix

##===================Logistic Regression===================
glm_model.all<-glm(booking_status~.-id-repeated_guest-market_segment_type, data=hotel_bookings, family=binomial)
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
#evaluate the accuracy of the backwawrd model
hotel_bookings$glm_pred<-predict(glm_model.backward,hotel_bookings, type="response")
prop.table(table(hotel_bookings$booking_status, hotel_bookings$glm_pred>0.5))
#The backward model can predict 76.7% of response correctly

glm_model_selection<-glm(booking_status~no_of_weekend_nights+no_of_week_nights+type_of_meal_plan+lead_time+arrival_month+mkt_repeat_guest+avg_price_per_room+no_of_special_requests, data=hotel_bookings, family=binomial)
stargazer(glm_model_selection,type="text",no.space=TRUE)
nagelkerke(glm_model_selection)

glm_model_elim_rep<-glm(booking_status~no_of_weekend_nights+no_of_week_nights+type_of_meal_plan+lead_time+arrival_month+avg_price_per_room+no_of_special_requests, data=hotel_bookings, family=binomial)
nagelkerke(glm_model_elim_rep)

# evaluate the accuracy of the selection model
hotel_bookings$glm_sele_pred<-predict(glm_model_selection,hotel_bookings, type="response")
prop.table(table(hotel_bookings$booking_status, hotel_bookings$glm_sele_pred>0.5))
# The variable combination model can predict 75.7% of response correctly

hotel_bookings$glm_elim_pred<-predict(glm_model_elim_rep,hotel_bookings, type="response")
prop.table(table(hotel_bookings$booking_status, hotel_bookings$glm_elim_pred>0.5))
#removal of repeated guest variable predicts 74.3% of response correctly

#some potential write up:
#even though the backward model has a higher R^2 and higher accuracy, the selection model can be better explained in a business context.

## ======================== Decision Tree ========================
library(tree)
library(rpart)
#split data into train and test
set.seed(123)
train_index <- sample(1:nrow(hotel_bookings), 0.8*nrow(hotel_bookings))
train <- hotel_bookings[train_index,]
val <- hotel_bookings[-train_index,]
#build decision tree
tree_model_big <- rpart(booking_status ~ .-id, data = train, method = "class",control = rpart.control(cp = 0))
tree_model<-tree(booking_status ~ .-id, data = train, method = "class")
plotcp(tree_model_big,upper = c("size"))
#the best tree has ~156 leaves
tree_156<-prune(tree_model_big,cp=0.00033)
#tree depth and leaves
nodes <- as.numeric(rownames(tree_156$frame))
max(rpart:::tree.depth(nodes))
sum(tree_156$frame$var == "<leaf>")
#plot the tree
png("tree_156.png", width = 2500, height = 2500, units = "px", res = 300)
plot(tree_156, uniform = TRUE)
text(tree_156, cex = 0.25)
dev.off()
#predict
tree_156$y<-val$booking_status
tree_156$yhat <- predict(tree_156, val, type = "class")
#accuracy
mean(tree_156$y == tree_156$yhat)
#The pruned decision tree has an accuracy of 81.5%
#test of overfitting
tree_156$y_train<-predict(tree_156, train, type = "class")
mean(tree_156$y_train == train$booking_status)
#this model is not over fitting
#predictions
tree_156$predict<- predict(tree_156, test, type = "class")

#comment on the tree: the tree is mainly using these variables to make a judgement:
#lead_time, market_segment_type, Avg_price_per_room, number_of special_requests, arrival_month to make predictions. 

## ======================== Random Forest ========================
library(randomForest)
#build random forest
rf_model <- randomForest(booking_status ~ .-id, data = train, importance = TRUE, ntree = 100,maxdepth = 5)
rf_model.train_predit = predict(rf_model,train)
rf_model.predict = predict(rf_model,val)
prop.table(table(rf_model.train_predit>0.5,train$booking_status))
#train accuracy is 95.21%
prop.table(table(rf_model.predict>0.5,val$booking_status))
#test accuracy is 82.04%
#random forest is overfitting no matter the tree depth or number of trees
#this may not be the best model for this data set


##===================Neural Network(nnet)===================
library(nnet)
#set seed
set.seed(123)
n <- names(train[,-1])
f <- as.formula(paste("booking_status ~", paste(n[!n %in% "booking_status"], collapse = " + ")))
nnet<-neuralnet(f, data=train, hidden = 10)
nnet.train<-predict(nnet, train)
nnet.val<-predict(nnet, val)
prop.table(table(nnet.train>0.5,train$booking_status))
prop.table(table(nnet.val>0.5,val$booking_status))
#this is a bad model to predict cancellation, since the calculation time is long for multiple layers, and the accuracy is bad

##===================keras===================
#install.packages("keras")
library(tensorflow)
library(keras)
data<-train[,-1]
data<-as.matrix(data)
labels<-train$booking_status
data_val<-val[,-1]
data_val<-as.matrix(data_val)
val_labels<-val$booking_status
head(labels)
model <- keras_model_sequential() %>%
    layer_dense(units = 5, activation = "relu",input_shape = c(18)) %>%
    layer_dense(units = 5, activation = "relu") %>%
    layer_dense(units = 1, activation = "sigmoid")
model %>% compile(
  optimizer = 'adam',
  loss = 'binary_crossentropy',
  metrics = list('accuracy')
)
history<-model %>% fit(
  data,
  labels,
  epochs = 5
)
results <- model %>% evaluate(data_val, val_labels)
plot(history)
history$metrics
results


## ======================== Scratch ========================