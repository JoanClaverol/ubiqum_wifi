# MENTOR: 

# ###########################################################################-
# GOAL: Wifi Locationing
# DESCRIPTION: First analysis
# AUTHOR: Aniss N
# ###########################################################################-

# MENTOR: DO NOT SET WD. IT WILL CRASH MY CODE
setwd("C:\\Users\\nisso\\Desktop\\Ubiqum\\Projects")

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(tibble)
library(lubridate)
library(caret)
library(plotly)

# Importing data ----------------------------------------------------------

wifi_training <- read_csv("./Module 3 - Task 3/Raw Data/trainingData.csv")
wifi_validation <- read_csv("./Module 3 - Task 3/Raw Data/validationData.csv")

# MENTOR: NEVER COMMENT CODE
# Checking data
#summary(wifi_training[521:529])
#sum(is.na(wifi_training))
#sum(duplicated(wifi_training)) # <------- Go deeper

# MENTOR: AVOID DUPLICITY
wifi_training$FLOOR <- factor(wifi_training$FLOOR)
wifi_training$BUILDINGID <- factor(wifi_training$BUILDINGID)
wifi_training$SPACEID <- factor(wifi_training$SPACEID)
wifi_training$RELATIVEPOSITION <- factor(wifi_training$RELATIVEPOSITION)
wifi_training$USERID <- factor(wifi_training$USERID)
wifi_training$PHONEID <- factor(wifi_training$PHONEID)
wifi_training$TIMESTAMP <- as_datetime(wifi_training$TIMESTAMP)
# MENTOR: BETTER WAY TO DO IT 
factor_var <- c("FLOOR","BUILDINGID") 
wifi_training[factor_var] <- apply(
  wifi_training[factor_var], MARGIN = 2, function(x) factor(x)
  )
# MENTOR: OTHER WAY
library(magrittr)
wifi_training %<>%
  mutate_at(.vars = vars(BUILDINGID, FLOOR), .funs = factor)

# MENTOR: show how to create a function 
# Creating a sample of 1000 rows
#set.seed(123)
#wifi_tsample <- wifi_training[sample(nrow(wifi_training), 1000), ]

set.seed(123)
indTrain <- createDataPartition(y=wifi_training$LATITUDE, p=0.75, list=FALSE)
wifi_mod_trn <- wifi_training[indTrain,]
wifi_mod_tst <- wifi_training[-indTrain,]
fitControl <- trainControl(method = "repeatedcv", number = 10, repeats = 1)


# Finding the building and floor

# ------------- Building

#set.seed(123)
#KNNmod_building <- train(factor(BUILDINGID)~.,
#                data = add_column(wifi_mod_trn[1:520], BUILDINGID = wifi_mod_trn$BUILDINGID),
#                method = "kknn",
#                trControl = fitControl, tuneLength = 2, preProc = c("center","scale"))
#saveRDS(KNNmod_building, file = "./Module 3 - Task 3/First analysis/KNNmod_building.rds")

KNNmod_building <- readRDS(file = "./Module 3 - Task 3/First analysis/KNNmod_building.rds")
wifi_mod_tst_building <- predict(KNNmod_building, newdata = wifi_mod_tst)
conKNN <- confusionMatrix(data = wifi_mod_tst_building, wifi_mod_tst$BUILDINGID)
conKNN

varImp(KNNmod_building)

# ------------- Floor

#set.seed(123)
#KNNmod_floor <- train(factor(FLOOR)~.,
#                data = add_column(wifi_mod_trn[1:520], FLOOR = wifi_mod_trn$FLOOR),
#                method = "kknn",
#                trControl = fitControl, tuneLength = 2, preProc = c("center","scale"))
#saveRDS(KNNmod_floor, file = "./Module 3 - Task 3/First analysis/KNNmod_floor.rds")

KNNmod_floor <- readRDS(file = "./Module 3 - Task 3/First analysis/KNNmod_floor.rds")
wifi_mod_tst_floor <- predict(KNNmod_floor, newdata = wifi_mod_tst)
wifi_mod_tst_floor <- factor(wifi_mod_tst_floor, levels = c("0","1","2","3","4"))
conKNN <- confusionMatrix(data = wifi_mod_tst_floor, wifi_mod_tst$FLOOR)
conKNN

# Finding the latitude and longetude

# ------------- Longitude

#set.seed(123)
#KNNmod_lon <- train(LONGITUDE~.,
#                data = add_column(wifi_mod_trn[1:520], LONGITUDE = wifi_mod_trn$LONGITUDE),
#                method = "knn",
#                trControl = fitControl, tuneLength = 2, preProc = c("center","scale"))
#saveRDS(KNNmod_lon, file = "./Module 3 - Task 3/First analysis/KNNmod_lon.rds")

KNNmod_lon <- readRDS(file = "./Module 3 - Task 3/First analysis/KNNmod_lon.rds")
wifi_mod_tst_lon <- predict(KNNmod_lon, newdata = wifi_mod_tst)

# ------------- Latitude

#set.seed(123)
#KNNmod_lat <- train(LATITUDE~.,
#                data = add_column(wifi_mod_trn[1:520], LATITUDE = wifi_mod_trn$LATITUDE),
#                method = "knn",
#                trControl = fitControl, tuneLength = 2, preProc = c("center","scale"))
#saveRDS(KNNmod_lat, file = "./Module 3 - Task 3/First analysis/KNNmod_lat.rds")

KNNmod_lat <- readRDS(file = "./Module 3 - Task 3/First analysis/KNNmod_lat.rds")
wifi_mod_tst_lat <- predict(KNNmod_lat, newdata = wifi_mod_tst)

# ------------- VIZ

plot_ly(wifi_mod_tst,
        x=~LONGITUDE, y=~LATITUDE, z=~as.numeric(as.character(FLOOR)),
        type="scatter3d", mode="markers")

wifi_mod_tst_results <- tibble(BUILDINGID = wifi_mod_tst$BUILDINGID,
                               FLOOR = wifi_mod_tst$FLOOR,
                               LONGITUDE = wifi_mod_tst$LONGITUDE,
                               LATITUDE = wifi_mod_tst$LATITUDE,
                               BUILDINGID_P = wifi_mod_tst_building, FLOOR_P = wifi_mod_tst_floor,
                               LONGITUDE_P = wifi_mod_tst_lon, LATITUDE_P = wifi_mod_tst_lat)

# MENTOR: comment between the pipes to explain step by step
ggplotly(
  wifi_mod_tst_results %>%
    filter(BUILDINGID != BUILDINGID_P) %>% 
    filter(row_number() == 1L) %>% 
    ggplot() +
    geom_point(data = wifi_mod_tst_results, aes(x = LONGITUDE, y = LATITUDE, color = BUILDINGID), alpha = 0.01) +
    geom_point(aes(x = LONGITUDE, y = LATITUDE), color = "gold3", alpha = 1) +
    geom_point(aes(x = LONGITUDE_P, y = LATITUDE_P), color = "darkred", alpha = 1) +
    facet_wrap( ~ FLOOR)
)

# MENTOR: str(), summary(), glimpse()... avoid them on the scripts
str(wifi_mod_tst_results)
view(wifi_mod_tst_results %>%
       filter(BUILDINGID != BUILDINGID_P))
