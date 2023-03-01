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
library(viridis)
#load data
hotel_bookings <- read.csv("./train.csv")
head(hotel_bookings)

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

