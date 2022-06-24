## https://github.com/rahul-raoniar/Rahul_Raoniar_Blogs/tree/main/Modeling%20Logistic%20Regression%20using%20Tidymodels%20Library%20in%20R

library(mlbench)
library(tidymodels)
library(tibble)


#### import data ####

## data frame to tibble
data(PimaIndiansDiabetes2)
PimaIndiansDiabetes2 <- tibble::as_tibble(PimaIndiansDiabetes2)

## view the structures of data
glimpse(PimaIndiansDiabetes2)
str(PimaIndiansDiabetes2)

#### data preprocessing ####

## removing NA values
Diabetes <- na.omit(PimaIndiansDiabetes2)
glimpse(Diabetes)

## check the levels of outcome
levels(Diabetes$diabetes)

## setting reference level
Diabetes$diabetes <- relevel(Diabetes$diabetes, ref = "pos")
levels(Diabetes$diabetes)

## Train-Test Split
set.seed(123)

diabetes_split <- initial_split(Diabetes,
                                prop = 0.75,
                                strata = diabetes)

diabetes_train <- diabetes_split %>%
  training()

diabetes_test <- diabetes_split %>%
  testing()

nrow(diabetes_train)
nrow(diabetes_test)

## Cross validation (추가예정정)

## fitting randomForest
# fitted_rf_model <- parsnip::rand_forest(trees = 200, min_n = 5) %>%
#  parsnip::set_engine("randomForest") %>%
#  parsnip::set_mode("classification") %>%
#  parsnip::fit(diabetes~., data = diabetes_train)

f <- "diabetes~."
fitted_rf_model <- goophi::randomForest(data = diabetes_train, formula = f)



fitted_rf_model


#### result ####

parsnip::predict.model_fit(fitted_rf_model, diabetes_test)

#### Predictions ####
fitted_rf_model %>%
  parsnip::predict.model_fit(diabetes_train) %>%
  dplyr::bind_cols(diabetes_train)   %>%
  glimpse()

#### Model Validation ####
fitted_rf_model %>%
  parsnip::predict.model_fit(diabetes_train) %>%
  dplyr::bind_cols(diabetes_train)   %>%
  metrics(truth = diabetes, estimate = .pred_class)

#### Per classifier metrics ####
fitted_rf_model %>%
  parsnip::predict.model_fit(diabetes_test, type = "prob") %>%
  glimpse()

rf_probs <- fitted_rf_model %>%
  parsnip::predict.model_fit(diabetes_test, type = "prob") %>%
  bind_cols(diabetes_test)

glimpse(rf_probs)

rf_probs %>%
  yardstick::gain_curve(diabetes, .pred_pos) %>%
  ggplot2::autoplot()

parsnip::predict.model_fit(fitted_rf_model, diabetes_test, type = "prob") %>%
  dplyr::bind_cols(predict.model_fit(fitted_rf_model, diabetes_test)) %>%
  dplyr::bind_cols(select(diabetes_test, diabetes)) %>%
  glimpse()

parsnip::predict.model_fit(fitted_rf_model, diabetes_test, type = "prob") %>%
  dplyr::bind_cols(predict.model_fit(fitted_rf_model, diabetes_test)) %>%
  dplyr::bind_cols(select(diabetes_test, diabetes)) %>%
  metrics(diabetes, .pred_pos,estimate = .pred_class)
