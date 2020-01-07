library(readr)
library(caret)

# load & check validation vs training data----
validationData <- read_csv("Data etc/validationData.csv")
trainingData <- read_rds("ToFilterData.rds")
summary(validationData[, 521:529])

plot(validationData$LONGITUDE, validationData$LATITUDE)
plot(trainingData$LONGITUDE, trainingData$LATITUDE)

# preprocess validationdata similar to traindata
## as_datetime(1371713733) = "2013-06-20 07:35:33 UTC"
PP_Valdata <- validationData %>% 
  mutate(DateTime = as_datetime(TIMESTAMP))
saveRDS(PP_Valdata, file = "PPValData.rds")

# validation data is missing some records for areas
# idea is to fill the long/lat based on similar WAP's and their long/lat 
#in training

## filter validation data per building----
B0_v <- filter(PP_Valdata, BUILDINGID == 0)
B1_v <- filter(PP_Valdata, BUILDINGID == 1)
B2_v <- filter(PP_Valdata, BUILDINGID == 2)

## create dataframe for validationdata with only predictors WAP and the variable
lat_v <- data.frame(validationData$LATITUDE, validationData[,1:520])
long_v <- data.frame(validationData$LONGITUDE, validationData[,1:520])
floor_v <- data.frame(validationData$FLOOR, validationData[,1:520])

# load trained models for predictions----
## KNN----
Fit_lat_B0 <- read_rds("KNN_Fit_lat_B0.rds")
Fit_lat_B1 <- read_rds("KNN_Fit_lat_B1.rds")
Fit_lat_B2 <- read_rds("KNN_Fit_lat_B2.rds")
Fit_long_B0 <- read_rds("KNN_Fit_long_B0.rds")
Fit_long_B1 <- read_rds("KNN_Fit_long_B1.rds")
Fit_long_B2 <- read_rds("KNN_Fit_long_B2.rds")
Fit_floor_B0 <- read_rds("KNN_Fit_floor_B0.rds")
Fit_floor_B1 <- read_rds("KNN_Fit_floor_B1.rds")
Fit_floor_B2 <- read_rds("KNN_Fit_floor_B2.rds")

#### PREDICT & check performance ####

# Predict B0----
## Lat----
predictions_KNNB0Lat= predict(Fit_lat_B0, lat_v)

#Evaluate predictions----
table(predictions_KNNB0Lat)
str(predictions_KNNB0Lat)

error_lat_Fit_lat_B0 <- predictions_KNNB0Lat - lat_v$validation.LATITUDE 
rmse_lat_Fit_lat_B0 <- sqrt(mean(error_lat_Fit_lat_B0^2))
rmse_lat_Fit_lat_B0

rsquared_lat.FIt_lat_B0 <- 1 - (sum(error_lat_Fit_lat_B0^2) / 
                                  sum((lat_v$validation.LATITUDE-mean(lat.v$validation.LATITUDE))^2))
rsquared.lat.rfor <- rsquared.lat.rfor * 100
rsquared.lat.rfor

#Confusion matrix----
confusionMatrix(predictions_KNNB0Lat,------])