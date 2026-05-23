library(tidyverse)
library(randomForest)
library(caret)

clean_asteroids <- readRDS ("clean_asteroids.rds")

set.seed(28)

train_index <- createDataPartition(clean_asteroids$potentially_hazardous, p = 0.8, list = FALSE)
train_data <- clean_asteroids[train_index, ]
test_data <- clean_asteroids[-train_index,]

rf_model <- randomForest(
  potentially_hazardous ~ .,
  data = train_data,
  ntree = 500,
  importance = TRUE
)

predictions <- predict(rf_model, test_data)

conf_matrix <- confusionMatrix(predictions, test_data$potentially_hazardous)

print(conf_matrix)

varImpPlot(rf_model, main = "Asteroid Feature Improtance")

#saveRDS(rf_model, "asteroid_rf_model.rds")