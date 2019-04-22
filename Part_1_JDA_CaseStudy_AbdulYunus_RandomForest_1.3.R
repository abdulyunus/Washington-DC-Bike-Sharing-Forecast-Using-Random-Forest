


# Title   : Bike Sharing in Washington D.C. Demand Forecast
# Author  : Abdul Yunus
# Version : 1.2
# Date    : 22nd April 2019
# Algorithm : Random Forest

# gc()
# rm(list = ls(all = TRUE))

# Here the user needs to provide the path of the input files

path <- "C:/Users/Abdul_Yunus/Desktop/Yunus_Personal/Learning/Case Study/JDA"

# Installed the required packages
packages<-function(x){
  x<-as.character(match.call()[[2]])
  if (!require(x,character.only=TRUE)){
    install.packages(pkgs=x,repos="http://cran.r-project.org")
    require(x,character.only=TRUE)
  }
}
# We will load the required packages

packages(caret)
packages(caTools)
packages(tree)
packages(ISLR)
packages(rpart)
packages(rpart.plot)
packages(randomForest)
packages(e1071)
packages(tidyverse)
# This package is used for accuracy check and error calculation
packages(Metrics)


# Set working directory
setwd(path)

# Import the day and Hour data

day_df <- read.csv('day.csv', header = TRUE)
hour_df <- read.csv('hour.csv', header = TRUE)


# Lets have a look at the data

head(day_df)
head(hour_df)

# hour_df data has same variables (columns) as day_df data except the additional 'hr' column.
# Lets check the structure of both the data sets.

str(day_df)
str(hour_df)

# There are some variables, that needs to be as factor. Lets convert some of numerical variable into factors into both the data.

# Convert the numeric variable into factor for day_df data
day_df$season <- as.factor(day_df$season)
day_df$yr <- as.factor(day_df$yr)
day_df$mnth <- as.factor(day_df$mnth)
day_df$holiday <- as.factor(day_df$holiday)
day_df$weekday <- as.factor(day_df$weekday)
day_df$workingday <- as.factor(day_df$workingday)
day_df$weathersit <- as.factor(day_df$weathersit)

# Convert the numeric variable into factor for hour_df data
hour_df$season <- as.factor(hour_df$season)
hour_df$yr <- as.factor(hour_df$yr)
hour_df$mnth <- as.factor(hour_df$mnth)
hour_df$holiday <- as.factor(hour_df$holiday)
hour_df$weekday <- as.factor(hour_df$weekday)
hour_df$workingday <- as.factor(hour_df$workingday)
hour_df$weathersit <- as.factor(hour_df$weathersit)
hour_df$hr <- as.factor(hour_df$hr)

# We can see that the dteday variable is chr variable in the data, it should be in the data format, lets convert it.

day_df$dteday <- as.Date(day_df$dteday)
hour_df$dteday <- as.Date(hour_df$dteday)


# Column 'instant' is simply the serial numbers, we can remove the column from both the data
day_df <- day_df[,-1]
hour_df <- hour_df[,-1]


# Lets use the day_df data our exploratory data analysis as we the hour_df data is aggregated at a day level and moulded into day_df data.


# Get the average count of bikes rent by season, hr
season_summary_by_hour <- hour_df %>%
  select(season, hr, cnt) %>%
  group_by(season, hr) %>%
  summarise(Count = mean(cnt)) 

# Lets plot this

ggplot(hour_df, aes(x= hr, y= Count, color=as.factor(season)))+
  geom_point(data = season_summary_by_hour, aes(group = as.factor(season)))+
  geom_line(data = season_summary_by_hour, aes(group = as.factor(season)))+
  ggtitle("Bikes Rent By Season")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_hue('Season',breaks = levels(as.factor(season_summary_by_hour$season)), labels=c('spring', 'summer', 'fall', 'winter'))

# From the above plot wecan say
# There are more rental in morning(from 7-9th hour) and evening(17-19th hour)
# People rent bikes more in Fall, and much less in Spring


# Get the average count of bikes rent by weather, hr

weather_summary_by_hour <- hour_df %>%
  select(weathersit, hr, cnt) %>%
  group_by(weathersit, hr) %>%
  summarise(Count = mean(cnt)) 

ggplot(hour_df, aes(x= hr, y= Count, color= as.factor(weathersit)))+
  geom_point(data = weather_summary_by_hour, aes(group = weathersit))+
  geom_line(data = weather_summary_by_hour, aes(group = weathersit))+
  ggtitle("Bikes Rent By Weather")+
  theme(plot.title = element_text(hjust = 0.5))+
  scale_colour_hue('Weathersit',breaks = levels(as.factor(hour_df$weathersit)), labels=c('Good', 'Normal', 'Bad', 'Very Bad'))

# From the above plot wecan say
# People rent bikes more when weather is good
# We see bike rent only at 18th hour when weather is very bad



# Now we can plot bike rental activity depending on the type of day (working days or non-working days)

hour_df.wd <- subset(hour_df, workingday == 1)

ggplot(hour_df.wd, aes( x = hr, y = cnt))+
  geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp))+
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on Working Days")

# From the above plot we can see
# On working days, bike rental acitivty peaks in the morning (~8PM) and in the afternoon (~5PM to 6PM).

hour_df.nwd <- subset(hour_df, workingday == 0)

ggplot(hour_df.nwd, aes( x = hr, y = cnt))+
  geom_point(position = position_jitter(w = 1, h = 0), aes(color = temp))+
  scale_color_gradient(low = "#88d8b0", high = "#ff6f69") + ggtitle("Bike Count on Working Days")


# On non-working days we see a gradual increase in bike rental activity that peaks between 1PM and 3PM.


# Fitting the Model
# I have observe that there are many categorical variable we have in the data which is labled.
# We can do the one-hot encoding on those variables.

# Lets do all this on the  hour_df data set as we are using this dataset for model building

# One Hot Encoding for season
hour_df$season_2 <- ifelse(hour_df$season ==2,1,0)
hour_df$season_3 <- ifelse(hour_df$season ==3,1,0)
hour_df$season_4 <- ifelse(hour_df$season ==4,1,0)


# One Hot Encoding for weathersit
hour_df$weathersit_2 <- ifelse(hour_df$weathersit ==2,1,0)
hour_df$weathersit_3 <- ifelse(hour_df$weathersit ==3,1,0)
hour_df$weathersit_4 <- ifelse(hour_df$weathersit ==4,1,0)


# Since we have created dummy variables for season, weathersit.
# Lets remove these variables from data and keep only respective dummy variables


final_df <- subset(hour_df, select = - c(season, weathersit))

# Since we have all information regarding date, month and time, we can remove the dteday column from the data
# Column cnt is the sum of  casual and registered, we can also remove these variables from our data.

final_df <- subset(final_df, select = -c(dteday, casual, registered))

# Data partitioning


set.seed(123)
id <- sample.split(Y = final_df$cnt, SplitRatio = 0.75)
train_df <- subset(final_df, id == "TRUE")
test_df <- subset(final_df, id == "FALSE")

# --- Lets build a decision tree on the data first -----#
# we create a decision tree model by calling the rpart function. 
# Let's first create a base model with default parameters and value. 


tree1 <- rpart(cnt ~., data = train_df)
rpart.plot(tree1)

pred1 = predict(object = tree1, newdata = test_df)

test_df_val = cbind(test_df, pred1)
test_df_val$NegativeVal <- ifelse(test_df_val$pred1 < 0,1,0)
# If we take the sum of all the values in the column 'NegativeVal' we can find the number of negative values.

sum(test_df_val$NegativeVal)
# SInce the sum of the 'NegativeVal' column is zero, it means the algorithm is not predicting any negative value.

# Lets calculate RMSE (Root Mean Square Error) and MAD (Mean Absolute Deviation) and MAPE (Mean Absolute Percentage Error).
test_df_val$Error <- test_df_val$cnt - test_df_val$pred1

RMSE_Dectree1 <- sum(sqrt(mean((test_df_val$Error)^2)))

MAD_Dectree1  <- sum(abs(test_df_val$pred1 - mean(test_df_val$pred1))/length(test_df_val$pred1))

MAPE_Dectree1 <- mean(abs((test_df_val$cnt - test_df_val$pred1)/test_df_val$cnt)*100)

Dec_tree_Accuracy <- data.frame(RMSE_Dectree1, MAD_Dectree1, MAPE_Dectree1)

# Model Accuracy for the decision tree
Dec_tree_Accuracy

# Using Decision Tree Model we can get the RMSE = 89, and MAD ~ 114 which comparatively better than a regression model

# let build the random forest model on the this data
set.seed(123)
rf1 = randomForest(cnt ~., data = train_df, mtry = ncol(train_df)-1)

# if we check the Out of Bag Error (OOB estimate of error) - this represents the accuracy. lets print the model
# When we do classification - Accuracy is OOB
# When we do Regression - Accuracy is RMSE and R-sq

print(rf1)

attributes(rf1)

# Gives GINI Index (Priority of Variables)
rf1$importance


#prediction on the training data set

pred_rf1 = predict(object = rf1, newdata = train_df)

train_df_val = cbind(train_df, pred_rf1)
train_df_val$NegativeVal <- ifelse(train_df_val$pred_rf1 < 0,1,0)
# If we take the sum of all the values in the column 'NegativeVal' we can find the number of negative values.

sum(train_df_val$NegativeVal)
# SInce the sum of the 'NegativeVal' column is zero, it means the algorithm is not predicting any negative value.

# Lets calculate RMSE ,MAD, MAPE

train_df_val$Error <- train_df_val$cnt - train_df_val$pred_rf1
# Calculate RMSE
RMSE_train_rf1 <- sum(sqrt(mean((train_df_val$Error)^2)))

# Calculate MAD
MAD_train_rf1  <- sum(abs(train_df_val$pred_rf1 - mean(train_df_val$pred_rf1))/length(train_df_val$pred_rf1))

# Calculate MAPE
MAPE_train_rf1 <- mean(abs((train_df_val$cnt - train_df_val$pred_rf1)/train_df_val$cnt)*100)

Accuracy_table <- data.frame(RMSE_Dectree1, MAD_Dectree1, MAPE_Dectree1, RMSE_train_rf1,MAD_train_rf1, MAPE_train_rf1)

Accuracy_table




#prediction on the test data set

pred_rf_test = predict(object = rf1, newdata = test_df)

test_df_val = cbind(test_df, pred_rf_test)
test_df_val$NegativeVal <- ifelse(test_df_val$pred_rf_test < 0,1,0)
# If we take the sum of all the values in the column 'NegativeVal' we can find the number of negative values.

sum(test_df_val$NegativeVal)
# SInce the sum of the 'NegativeVal' column is zero, it means the algorithm is not predicting any negative value.

# Lets calculate RMSE, MAD and MAPE.
test_df_val$Error <- test_df_val$cnt - test_df_val$pred_rf_test

RMSE_test_rf1 <- sum(sqrt(mean((test_df_val$Error)^2)))

MAD_test_rf1  <- sum(abs(test_df_val$pred_rf_test- mean(test_df_val$pred_rf_test))/length(test_df_val$pred_rf_test))

MAPE_test_rf1 <- mean(abs((test_df_val$cnt - test_df_val$pred_rf_test)/test_df_val$cnt)*100)

Accuracy_table <- data.frame(RMSE_Dectree1,  RMSE_train_rf1,RMSE_test_rf1 ,MAD_Dectree1,MAD_train_rf1,MAD_test_rf1,  MAPE_Dectree1, MAPE_train_rf1,MAPE_test_rf1)

Accuracy_table



# Error rate of Random Forest
# Lets plot the RF model to check the OBB - it actually tell us where OBB is steady or constant.
# This plot is also used to know how many tree to used for random forest
plot(rf1)

# From the plot, we can see that after 100trees, the Error is steady and no more drop in the error value.
# Lets tune the model to find out mtry values

t = tuneRF(train_df, train_df$cnt,
           stepFactor = 1.2,
           plot = T,
           ntreeTry = 100,
           improve = 0.05)

# After tuning these parameter, we found that optimal mtry value could be 12.
# Let put these parameter in our model and re run it

set.seed(1234)
rf2 = randomForest(cnt ~., data = train_df,
                   mtry = 12,
                   ntree = 100,
                   importance = T,
                   proximity = T)
print(rf2)



# Gives GINI Index (Priority of Variables)
rf2$importance


#prediction on the training data set

pred_rf2 = predict(object = rf2, newdata = train_df)

#sum(pred_rf2)

train_df_val = cbind(train_df, pred_rf2)
train_df_val$NegativeVal <- ifelse(train_df_val$pred_rf2 < 0,1,0)
# If we take the sum of all the values in the column 'NegativeVal' we can find the number of negative values.

sum(train_df_val$NegativeVal)
# Since the sum of the 'NegativeVal' column is zero, it means the algorithm is not predicting any negative value.

# Lets calculate RMSE (Root Mean Square Error) and MAD (Mean Absolute Deviation)
train_df_val$Error <- train_df_val$cnt - train_df_val$pred_rf2

RMSE_train_rf2 <- sum(sqrt(mean((train_df_val$Error)^2)))

MAD_train_rf2  <- sum(abs(train_df_val$pred_rf2 - mean(train_df_val$pred_rf2, na.rm = T))/length(train_df_val$pred_rf2))
MAPE_train_rf2 <- mean(abs((train_df_val$cnt - train_df_val$pred_rf2)/train_df_val$cnt)*100)

Accuracy_table_RMSE <- data.frame(RMSE_Dectree1, RMSE_train_rf1,RMSE_test_rf1 ,RMSE_train_rf2)
Accuracy_table_MAD <- data.frame(MAD_Dectree1, MAD_train_rf1,MAD_test_rf1, MAD_train_rf2)
Accuracy_table_MAPE <- data.frame(MAPE_Dectree1, MAPE_train_rf1,MAPE_test_rf1,MAPE_train_rf2)

# lets see the tables
Accuracy_table_RMSE
Accuracy_table_MAD
Accuracy_table_MAPE


#prediction on the test data set

pred_rf_test = predict(object = rf2, newdata = test_df)

test_df_val = cbind(test_df, pred_rf_test)
test_df_val$NegativeVal <- ifelse(test_df_val$pred_rf_test < 0,1,0)
# If we take the sum of all the values in the column 'NegativeVal' we can find the number of negative values.

sum(test_df_val$NegativeVal)
# SInce the sum of the 'NegativeVal' column is zero, it means the algorithm is not predicting any negative value.

# Lets calculate RMSE (Root Mean Square Error) and MAD (Mean Absolute Deviation)

test_df_val$Error <- test_df_val$cnt - test_df_val$pred_rf_test

RMSE_test_rf2 <- sum(sqrt(mean((test_df_val$Error)^2)))

MAD_test_rf2  <- sum(abs(test_df_val$pred_rf_test- mean(test_df_val$pred_rf_test))/length(test_df_val$pred_rf_test))
MAPE_test_rf2 <- mean(abs((test_df_val$cnt - test_df_val$pred_rf_test)/test_df_val$cnt)*100)
Accuracy_table_RMSE <- data.frame(RMSE_Dectree1, RMSE_train_rf1,RMSE_test_rf1 ,RMSE_train_rf2,RMSE_test_rf2)

Accuracy_table_MAD <- data.frame(MAD_Dectree1, MAD_train_rf1,MAD_test_rf1, MAD_train_rf2,MAD_test_rf2)
Accuracy_table_MAPE <- data.frame(MAPE_Dectree1, MAPE_train_rf1,MAPE_test_rf1,MAPE_train_rf2,MAPE_test_rf2 )

# lets see the tables
Accuracy_table_RMSE
Accuracy_table_MAD
Accuracy_table_MAPE