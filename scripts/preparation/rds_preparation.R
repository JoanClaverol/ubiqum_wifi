# -------------------------------------------------------------------------
# GOAL: load raw data and store as rds file
# DESCRIPTION:
# -------------------------------------------------------------------------

# libraries ---------------------------------------------------------------
if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(
  readr, magrittr
)

# load and save data ------------------------------------------------------
# train 
train <- read_csv("data/raw/trainingData.csv") %>% 
  write_rds("data/clean/train.rds")

# validation
validation <- read_csv("data/raw/validationData.csv") %>% 
  write_rds("data/clean/validation.rds")

