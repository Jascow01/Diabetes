library(dplyr)
library(rpart)
library(rpart.plot)
library(tidymodels)
set.seed(1234)


dane <- read.csv("diabetes_prediction_dataset.csv")

dane$diabetes <- as.factor(dane$diabetes)
dane$gender <- as.factor(dane$diabetes)
dane$smoking_history <- as.factor(dane$smoking_history)
dane$hypertension <- as.factor(dane$hypertension)
dane$heart_disease <- as.factor(dane$heart_disease)

dane <- dane %>% 
  mutate(`age` = cut(
  age,
  breaks = c(0, 13, 18, 24, 34, 44, 59, Inf),
  labels = c("0-13", "14-18", "19-24", "25-34", "35-44", "45-59", "60+"),
  right = TRUE,
  include.lowest = TRUE
    )
  ) %>% 
  select(-bmi)


split <- initial_split(dane, strata = diabetes)
train_data <- training(split)
test_data <- testing(split)

rec <- recipe(diabetes ~ ., data = train_data)

model_spec <- decision_tree(cost_complexity = tune(), tree_depth = tune()) %>%
  set_mode("classification") %>%
  set_engine("rpart")

wf <- workflow() %>%
  add_recipe(rec) %>%
  add_model(model_spec)

folds <- vfold_cv(train_data, v = 5, strata = diabetes)
grid <- grid_regular(cost_complexity(), tree_depth(range = c(1,5)), levels = 3)

tuned <- tune_grid(
  wf,
  resamples = folds,
  grid = grid,
  metrics = metric_set(roc_auc, accuracy)
)

best_auc <- select_best(tuned, metric = "roc_auc")
final <- finalize_workflow(wf, best_auc)

final_model <- last_fit(final, split)

model <- extract_workflow(final_model)


saveRDS(model,"decision_tree.rds")

new_data <- data.frame(
  gender = factor("0"),
  age = factor("19-24"),
  hypertension = factor(0),
  heart_disease = factor(0),
  smoking_history = factor("never"),
  HbA1c_level = 9,
  blood_glucose_level = 250
)

predict(model, new_data = new_data, type = "class")



