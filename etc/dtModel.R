## https://github.com/rahul-raoniar/Rahul_Raoniar_Blogs/tree/main/Modeling%20Logistic%20Regression%20using%20Tidymodels%20Library%20in%20R

library(mlbench)
library(tidymodels)
library(tibble)
library(rpart)
library(rpart.plot)


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

# fitting decision tree
# fitted_dt_model <- parsnip::decision_tree() %>%
#   parsnip::set_engine("rpart") %>%
#   parsnip::set_mode("classification") %>%
#   parsnip::fit(diabetes~., data = diabetes_train)

f <- "diabetes~."
fitted_dt_model <- goophi::decisionTree(data = diabetes_train, formula = f)

fitted_dt_model
