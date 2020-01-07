# libraries
if (require(pacman) == FALSE) {
  install.packages("pacman")
}
pacman::p_load(tidyverse, rpart)


mod <- rpart(factor(BUILDINGID, levels = c(0,1,2)) ~ ., 
      data = train %>% select(starts_with("WAP"), BUILDINGID))

write_rds(mod, "mod.rds")

# app creation ------------------------------------------------------------

guy <- validation[42:56,]

# preprocess 

# call the model
mod <- read_rds("mod.rds")

# predict position based on a guy
predict(object = mod, newdata = guy, type = "class")
 
pred_b <- function(guy, model, type, name) {
  library(readr)
  if (type == "B") {
    # guy data preprocess
    
    # predictions
    pred_building <- predict(object = model, newdata = guy, type = "class")
    print(paste("You are in the building", pred_building))
    
    # store results
    write_rds(pred_building, paste0("data/clean/model_results_",name,".rds"))
  }
  
  if (type == "F") {
    # predictions
    # pred_building <- predict(object = model, newdata = guy, type = "class")
    print(paste("You are in the floor", "unkown"))
    
  }
  
}

results <- pred_b(guy = guy, model = mod, type = "B", name = "dt_new")
