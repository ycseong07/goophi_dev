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

## fitting knn
# fitted_knn_model <- parsnip::nearest_neighbor() %>%
#  parsnip::set_engine("kknn") %>%
#  parsnip::set_mode("classification") %>%
#  parsnip::fit(diabetes~., data = diabetes_train) %>%


f <- "diabetes~."
fitted_knn_model <- goophi::knn(data = diabetes_train, formula = f)

fitted_knn_model

dplyr::bind_cols(
  parsnip::predict.model_fit(fitted_knn_model, diabetes_test),
  parsnip::predict.model_fit(fitted_knn_model, diabetes_test, type = "prob")
)

