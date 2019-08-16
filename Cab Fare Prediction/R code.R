rm(list = ls())
library(ggplot2)
library(corrgram)
install.packages("geosphere")
library(geosphere)
library(caret)
library(rpart)
library(dplyr)
library(tidyr)
library(lubridate)
library(DMwR)
install.packages('randomForest')
library(randomForest)

setwd("C:/Users/Ankush Saha/Downloads/Compressed")

############################################loading the Data#############################################
x = read.csv("train_cab.csv", header = T)
bacup = x
#x = backup

############################################Preparing Data#############################################
fare_amount = x$fare_amount
x = x[,2:7]
x = cbind(x,fare_amount)
x$fare_amount[x$fare_amount %in% ''] = NA
x$passenger_count = as.factor(x$passenger_count)
x$pickup_longitude = as.factor(x$pickup_longitude)
x$pickup_latitude = as.factor(x$pickup_latitude)
x$dropoff_latitude = as.factor(x$dropoff_latitude)
x$dropoff_longitude = as.factor(x$dropoff_longitude)


############################################Data Pre-Processing#############################################

#####Missing Value Analysis#####
mval = data.frame(apply(x,2,function(y){sum(is.na(y))}))
mval$collumns = row.names(mval)
names(mval)[1] = "missing percentage"
mval$`missing percentage` = (mval$`missing percentage`*100)/nrow(x)
mval = mval[,c(2,1)]
mval = mval[order(-mval$`missing percentage`),]
rownames(mval) = NULL


x = x[which(!x$fare_amount %in% NA ),]
x = x[which(!x$passenger_count %in% NA ),]


#####Zero Value Analysis#####
zeroVal = data.frame(apply(x,2,function(y){sum(y == 0)}))
zeroVal$Collumns = row.names(zeroVal)
zeroVal = zeroVal[,c(2,1)]
names(zeroVal)[2] = "missing percentage"
zeroVal$`missing percentage` = (zeroVal$`missing percentage`* 100)/16067
rownames(zeroVal) = NULL
zeroVal = zeroVal[order(-zeroVal$`missing percentage`),]

x = x[which(!x$pickup_longitude %in% 0),]
x = x[which(!x$pickup_latitude %in% 0),]
x = x[which(!x$dropoff_longitude %in% 0),]
x = x[which(!x$dropoff_latitude %in% 0),]
x = x[which(!x$passenger_count %in% 0),]
x = x[which(!x$fare_amount %in% 0),]


#####Removing Outlier#####
x = x[which(!x$pickup_datetime %in% 43),]
x = x[which(!x$passenger_count %in% 0.12 ),]
x = x[which(!x$passenger_count %in% 1.3 ),]
x$fare_amount = as.numeric(as.character(x$fare_amount))
x = x[which(!x$fare_amount %in% NA),]
x = x[!(x$fare_amount < 0),]
x$passenger_count = as.numeric(as.character(x$passenger_count))
x = x[!(x$passenger_count > 6),]


########Formatting proper data types######
x$pickup_longitude = as.numeric(as.character(x$pickup_longitude))
x$pickup_latitude = as.numeric(as.character(x$pickup_latitude))
x$dropoff_latitude = as.numeric(as.character(x$dropoff_latitude))
x$dropoff_longitude = as.numeric(as.character(x$dropoff_longitude))
x$pickup_datetime = as.POSIXct(x$pickup_datetime)
x$passenger_count = as.factor(x$passenger_count)


##########Converting the longitude and latitude into Distance############
x = x[!(x$pickup_longitude > 360),]
x = x[!(x$pickup_latitude > 360),]
x = x[!(x$dropoff_longitude > 360),]
x = x[!(x$dropoff_latitude > 360),]
x$dist = distHaversine(x[,2:3], x[,4:5])/1000
x = x[which(!x$dist %in% 0),]
x = x[,c(1,6,8,7)]


#####Feature Sampling#####
set.seed(1234)
train_index = createDataPartition(x$fare_amount,p = 0.70, list = FALSE)
train = x[train_index,]
test = x[-train_index,]

############################################Developing Model#############################################

#####Linear regression Model#####

lm_model = lm(fare_amount ~. , data = train)
predicted = predict(lm_model, test[,-4])


#####model testing#####
regr.eval(test[,4], predicted, stats = c('mae','mape','rmse'))
#RMSE = 798.841

#####Decision tree#####
fit = rpart(fare_amount ~ ., data = train, method = 'anova')
predicted2 = predict(fit, test[,-4])

#####model testing#####
regr.eval(test[,4], predicted2, stats = c('mae','mape','rmse'))
#RMSE = 799.039
########Random Forest#########
m1 = randomForest(  formula = fare_amount ~ ., data= train)
predicted3 = predict(m1, test[,-4])

#####model testing#####
regr.eval(test[,4], predicted3, stats = c('mae','mape','rmse'))
#RMSE = 90.24

#####################Applying on Test data#####################
g = read.csv("test.csv")
y = g
#####Preparing Data###########
y$passenger_count = as.factor(y$passenger_count)
y$pickup_longitude = as.factor(y$pickup_longitude)
y$pickup_latitude = as.factor(y$pickup_latitude)
y$dropoff_latitude = as.factor(y$dropoff_latitude)
y$dropoff_longitude = as.factor(y$dropoff_longitude)


#####Missing Value Analysis#####
mval = data.frame(apply(y,2,function(y){sum(is.na(y))}))
mval$collumns = row.names(mval)
names(mval)[1] = "missing percentage"
mval$`missing percentage` = (mval$`missing percentage`*100)/nrow(y)
mval = mval[,c(2,1)]
mval = mval[order(-mval$`missing percentage`),]
rownames(mval) = NULL


#####Zero Value Analysis#####
zeroVal = data.frame(apply(y,2,function(y){sum(y == 0)}))
zeroVal$Collumns = row.names(zeroVal)
zeroVal = zeroVal[,c(2,1)]
names(zeroVal)[2] = "missing percentage"
zeroVal$`missing percentage` = (zeroVal$`missing percentage`* 100)/16067
rownames(zeroVal) = NULL
zeroVal = zeroVal[order(-zeroVal$`missing percentage`),]




y$pickup_longitude = as.numeric(as.character(y$pickup_longitude))
y$pickup_latitude = as.numeric(as.character(y$pickup_latitude))
y$dropoff_latitude = as.numeric(as.character(y$dropoff_latitude))
y$dropoff_longitude = as.numeric(as.character(y$dropoff_longitude))
y$passenger_count = as.numeric(as.character(y$passenger_count))
y$pickup_datetime = as.POSIXct(y$pickup_datetime)

y$passenger_count = as.factor(y$passenger_count)
y = y[!(y$pickup_longitude > 360),]
y = y[!(y$pickup_latitude > 360),]
y = y[!(y$dropoff_longitude > 360),]
y = y[!(y$dropoff_latitude > 360),]

y$dist = distHaversine(y[,3:2], y[,5:4])
#y = y[which(!y$dist %in% 0),]

y = y[,c(1,6,7)]


predictedFare = predict(m1, y)
g$'predicted Fare' = predictedFare

val = y$dist == 0

g$`predicted Fare`[val] = 0


write.csv(g, "predict2_R.csv", row.names = FALSE)
