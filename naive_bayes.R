library(dplyr)
library(na.tools)
library(caret)
library(lubridate)
library(naivebayes)

setwd("C:/Users/Joao Aguiar/Desktop/DataMining")

######################################## Load files csv #####################################################

fires2015_train <- read.csv("fires2015_train.csv", na.strings="?", header=TRUE)
fires2015_test <- read.csv("fires2015_test.csv", na.strings="?", header=TRUE)

#############################################################################################################
######################################## Clean-up and Pre-processing ########################################

fires2015_train <- subset(fires2015_train, select=-c(alert_source, firstInterv_date, firstInterv_hour, extinction_date, extinction_hour))
fires2015_train$region <- factor(fires2015_train$region, ordered=TRUE)
fires2015_train$district <- factor(fires2015_train$district, ordered=TRUE)
fires2015_train$municipality <- factor(fires2015_train$municipality, ordered=TRUE)
fires2015_train$parish <- factor(fires2015_train$parish, ordered=TRUE)
fires2015_train$origin <- factor(fires2015_train$origin, ordered=TRUE)
fires2015_train$cause_type <- factor(fires2015_train$cause_type, ordered=TRUE)
fires2015_train$lat <- factor(fires2015_train$lat, ordered=TRUE)
fires2015_train$lon <- factor(fires2015_train$lon, ordered=TRUE)
fires2015_train$alert_date <- ymd(fires2015_train$alert_date)
fires2015_train$alert_hour <- hms(fires2015_train$alert_hour)
fires2015_train <- fires2015_train %>% na.rm()

fires2015_test <- subset(fires2015_test, select=-c(alert_source, firstInterv_date, firstInterv_hour, extinction_date, extinction_hour))
fires2015_test$region <- factor(fires2015_test$region, ordered=TRUE)
fires2015_test$district <- factor(fires2015_test$district, ordered=TRUE)
fires2015_test$municipality <- factor(fires2015_test$municipality, ordered=TRUE)
fires2015_test$parish <- factor(fires2015_test$parish, ordered=TRUE)
fires2015_test$origin <- factor(fires2015_test$origin, ordered=TRUE)
fires2015_test$lat <- factor(fires2015_test$lat, ordered=TRUE)
fires2015_test$lon <- factor(fires2015_test$lon, ordered=TRUE)
fires2015_test$alert_date <- ymd(fires2015_test$alert_date)
fires2015_test$alert_hour <- hms(fires2015_test$alert_hour)

############################################################################################################
############################################# Dara Exploration #############################################

# Region/District/Municipality/Parish with most fires:
fires2015_train %>% group_by(region) %>% summarize(Fires=n()) %>% arrange(desc(Count))

# Most common type of fire:
fires2015_train %>% group_by(cause_type) %>% summarize(Fires=n()) %>% arrange(desc(Count))

# Most common origin:
fires2015_train %>% group_by(origin) %>% summarize(Fires=n()) %>% arrange(desc(Count))

# Fire that took the most time to put out:
fires2015_train %>% group_by(alert_date,extinction_date) %>% summarize(Time=as.Date(extinction_date)-as.Date(alert_date)) %>% arrange(desc(Time)) 

###########################################################################################################
################################################ Naive Bayes ################################################

set.seed(123)
input_train <- createDataPartition(y=fires2015_train$cause_type, p=1, list=FALSE)
fires_train <- fires2015_train %>% slice(input_train)
fires_test <- fires2015_test
fires_train <- subset(fires_train, select=-c(id, lat, lon, alert_date, alert_hour, total_area, village_area, village_veget_area))

nb.model <- naive_bayes(cause_type ~., data=fires_train, laplace=1)
nb.preds <- predict(nb.model, fires_test)

kaggle_fires <- data.frame(matrix(ncol=0, nrow=3218))
kaggle_fires$id <- fires2015_test$id
kaggle_fires$cause_type <- nb.preds
write.csv(kaggle_fires, 'kaggle_fires.csv')

#############################################################################################################