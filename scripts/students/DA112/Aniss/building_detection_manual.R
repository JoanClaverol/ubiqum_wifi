# ###########################################################################-
# GOAL: Wifi Locationing
# DESCRIPTION: Building detection
# AUTHOR: Aniss N
# MENTOR: IT IS GOOD TO ADD THE DATE
# Tue Jan  7 13:25:49 2020 ------------------------------
# ###########################################################################-

setwd("C:\\Users\\nisso\\Desktop\\Ubiqum\\Projects")

# Libraries ---------------------------------------------------------------
library(ggplot2)
library(dplyr)
library(readr)
library(tibble)
library(lubridate)
library(caret)
library(plotly)
library(tidyr)
library(foreign)
library(FactoMineR)
library(factoextra)
library(corrplot)

# Import data -------------------------------------------------------------

wifi_training <- readRDS("./Module 3 - Task 3/New Data/wifi_training_pp.rds")
wifi_validation <- readRDS("./Module 3 - Task 3/New Data/wifi_validation_pp.rds")
wifi_training_nodup <- readRDS("./Module 3 - Task 3/New Data/wifi_training_pp_nodup.rds")

# Creation of table waps vs building --------------------------------------
# Create the table waps vs building based on training
wifi_training_nodup %>% 
  pivot_longer(cols = starts_with("WAP"),
               names_to = "WAP") %>% 
  select(BUILDINGID, WAP, value) %>% 
  mutate(count = if_else(value == -110, 0, 1)) %>% 
  group_by(BUILDINGID, WAP) %>% 
  summarise(count = sum(count)) %>% 
  # MENTOR: ungroup and commment
  pivot_wider(names_from = BUILDINGID, values_from = count) %>% 
  mutate(Building = as.numeric(colnames(.)[apply(.,1,which.max)])) %>% 
  select(WAP, Building) -> wap_vs_b


# Algorithm to detect the building ----------------------------------------
wifi_tsample <- wifi_training_nodup

# Calculation of ratio of detected waps
wifi_tsample %>% 
  mutate(ID = seq.int(n())) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>%
  left_join(wap_vs_b) %>% 
  mutate(Building = if_else(value == -110,-1,Building)) %>%
  filter(Building != -1) %>%
  arrange(ID,desc(value)) %>% 
  group_by(ID,Building) %>%
  #top_n(n=9, wt = WAP) %>% 
  summarise(count = n(), value = max(value)) %>%
  arrange(ID,desc(value)) %>%
  pivot_wider(names_from = Building, values_from = c(count,value)) %>% 
  mutate_all(~replace(., is.na(.), 0)) %>%
  # MENTOR: USE ONE MUTATE
  mutate(ratio_01 = if_else(count_0 == 0 | count_1 == 0, 0,
                            abs(min(c(count_0,count_1))/max(c(count_0,count_1)))*100)) %>% 
  mutate(ratio_02 = if_else(count_0 == 0 | count_2 == 0, 0,
                            abs(min(c(count_0,count_2))/max(c(count_0,count_2)))*100)) %>% 
  mutate(ratio_12 = if_else(count_1 == 0 | count_2 == 0, 0,
                            abs(min(c(count_1,count_2))/max(c(count_1,count_2)))*100)) %>% 
  select(ID, ratio_01, ratio_02, ratio_12) -> ratio_per_id
  # MENTOR: IF YOU GROUP_BY() ALWAYS UNGROUP THEM
  # https://community.rstudio.com/t/is-ungroup-recommended-after-every-group-by/5296

# Identify the building based on table waps vs building
wifi_tsample %>% 
  mutate(ID = seq.int(n())) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>%
  left_join(wap_vs_b) %>% 
  mutate(Building = if_else(value == -110,-1,Building)) %>%
  filter(Building != -1) %>%
  arrange(ID,desc(value)) %>% 
  group_by(ID,Building) %>%
  #top_n(n=10, wt = WAP) %>% 
  summarise(count = n(), max = max(value), mean = mean(value)) %>%
  arrange(ID,desc(max)) %>%
  left_join(ratio_per_id) %>% 
  #filter(duplicated(ID) | duplicated(ID, fromLast = T)) %>% 
  #filter(ratio_01 > 50 | ratio_02 > 50 | ratio_12 > 50)
  group_by(ID) %>% 
  summarise(Building = if_else(mean(ratio_01) > 45 | mean(ratio_02) > 45 | mean(ratio_12) > 45,
                               Building[which.max(max)],
                               Building[which.max(count)])) %>%
  pull(Building) -> predict_building

confusionMatrix(data = factor(predict_building, levels = levels(wifi_tsample$BUILDINGID)), wifi_tsample$BUILDINGID)


# Error checking ----------------------------------------------------------

#Error viz
wifi_tsample %>% 
  add_column(BUILDINGID_P = predict_building) %>% 
  filter(BUILDINGID == 2 & BUILDINGID_P == 1) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  filter(value != -110) %>% 
  pivot_wider(names_from = WAP, values_from = value) %>%
  ggplot() +
  geom_point(data = wifi_tsample, aes(x = LONGITUDE, y = LATITUDE, color = BUILDINGID), alpha = 0.01) +
  geom_point(aes(x = LONGITUDE, y = LATITUDE), color = "darkred", alpha = 1) +
  facet_wrap( ~ FLOOR)

# Save only the detected waps of errors
wifi_tsample %>%
  add_column(BUILDINGID_P = predict_building) %>% 
  filter(BUILDINGID == 0 & BUILDINGID_P == 1) %>%
  #slice(1L) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  filter(value != -110) %>% 
  pivot_wider(names_from = WAP, values_from = value) %>% #view()
  colnames(.) %>% tibble(WAP = .) %>% # MENTOR: explain that steps 
  left_join(wap_vs_b, by = c("WAP" = "WAP")) %>%
  slice(11:n()) %>% #-> temp_waps_b # MENTOR: WHAT DOES IT DO?
  group_by(Building) %>% summarize(n = n())

# Display mean, min or max of the detected buildings
wifi_tsample %>%
  add_column(BUILDINGID_P = predict_building) %>% 
  filter(BUILDINGID == 0 & BUILDINGID_P == 1) %>%
  #slice(1L) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  filter(value != -110) %>% 
  left_join(temp_waps_b) %>% 
  group_by(Building) %>% 
  summarize(value = mean(value))

# Display the error data
wifi_tsample %>%
  mutate(ID = seq.int(n())) %>% 
  add_column(BUILDINGID_P = predict_building) %>% 
  filter(BUILDINGID == 0 & BUILDINGID_P == 1) %>%
  #slice(1L) %>% 
  pivot_longer(cols = starts_with("WAP"), names_to = "WAP") %>% 
  filter(value != -110) %>% 
  pivot_wider(names_from = WAP, values_from = value) %>% view()