## 유저로부터 받는 입력은 camel case,
##예시로 사용한 변수 및 snake case로 작성된 dependencies의 함수명은 snake case로 표기합니다.


## data import
library(tidyverse)
library(tidymodels)
library(dplyr)
library(recipes)
library(parsnip)
library(tune)
library(rsample)
library(vip)
library(ggrepel)
library(ggfortify)
library(ggdendro)
library(goophi)

set.seed(1234)

#### data load classification ####

data(titanic_train, package = "titanic")

cleaned_data <- tibble::as_tibble(titanic_train) %>%
  dplyr::select(-c(PassengerId, Name, Cabin, Ticket)) %>%
  dplyr::mutate(across(where(is.character), factor)) %>%
  dplyr::mutate(Survived = as.factor(Survived ))

## one-hot encoding
rec <- recipe(Survived ~ ., data = cleaned_data) %>%
  step_dummy(all_predictors(), -all_numeric())

rec_prep <- prep(rec)

cleaned_data <- bake(rec_prep, new_data = cleaned_data)

#### (1) Train-test split ####

# target 변수를 사용자로부터 입력 받습니다
targetVar <- "Survived"

# 아래 3 가지 data를 생성합니다.
data_train <- goophi::trainTestSplit(data = cleaned_data, target = targetVar)[[1]] # train data
data_test <- goophi::trainTestSplit(data = cleaned_data, target = targetVar)[[2]] # test data
data_split <- goophi::trainTestSplit(data = cleaned_data, target = targetVar)[[3]] # whole data with split information

#### (2) Make recipe for CV ####

# 아래 변수들을 사용자로부터 입력 받습니다
imputation <- TRUE
normalization <- TRUE
pca <- FALSE ## need to fix warning
formula <- "Survived ~ ."
pcaThres <- "0.7"

####################

#### data load regression ####

# cleaned_data <- read.csv("~/git/goophi_dev/data/winequality-red.csv", sep = ",")
#
# cleaned_data
#
# #### (1) Train-test split ####
#
# # target 변수를 사용자로부터 입력 받습니다
# targetVar <- "quality"
#
# # 아래 3 가지 data를 생성합니다.
# data_train <- goophi::trainTestSplit(data = cleaned_data, target = targetVar)[[1]] # train data
# data_test <- goophi::trainTestSplit(data = cleaned_data, target = targetVar)[[2]] # test data
# data_split <- goophi::trainTestSplit(data = cleaned_data, target = targetVar)[[3]] # whole data with split information
#
# #### (2) Make recipe for CV ####
#
# # 아래 변수들을 사용자로부터 입력 받습니다
# imputation <- TRUE
# normalization <- TRUE
# pca <- FALSE ## need to fix warning
# formula <- "quality ~ ."
# pcaThres <- "0.7"

####################


# train data에 대한 전처리 정보가 담긴 recipe를 생성합니다.
rec <- goophi::preprocessing(data = data_train,
                             formula,
                             imputationType = "mean",
                             normalizationType = "range", # min-max normalization as default
                             pcaThres = pcaThres,
                             imputation = imputation,
                             normalization = normalization,
                             pca = pca)
rec

#### (3) Modeling ####
## todo: make goophi to install dependencies when the engine is not installed

#### classification ####
# engine, mode 사용자로부터 입력 받습니다
engine = "rpart"
mode = "classification"

# 사용자정의 ML 모델을 생성합니다
model <- goophi::decisionTree_phi(engine = engine,
                             mode = mode)

model

#### (4) Grid serach CV ####

# 모델에 사용되는 parameter들을 사용해 parameterGrid를 입력받습니다 (사용자로부터 parameter grid를 받는 방법 고민)
parameterGrid <- dials::grid_regular(
  tree_depth(range = c(10, 30)),
  min_n(range = c(2, 10)),
  cost_complexity = sample_prop(c(0.1, 0.5)),
  levels = 5)
# trining data를 몇 개로 나눌지 입력받습니다.
v <- 2
####################

#### regression ####
# # engine, mode 사용자로부터 입력 받습니다
# engine = "rpart"
# mode = "regression"
#
# # 사용자정의 ML 모델을 생성합니다
# model <- goophi::decisionTree_phi(engine = engine,
#                                   mode = mode)
#
# model
#
# #### (4) Grid serach CV ####
#
# # 모델에 사용되는 parameter들을 사용해 parameterGrid를 입력받습니다 (사용자로부터 parameter grid를 받는 방법 고민)
# parameterGrid <- dials::grid_regular(
#   tree_depth(range = c(10, 30)),
#   min_n(range = c(2, 10)),
#   cost_complexity = sample_prop(c(0.1, 0.5)),
#   levels = 5)
# # trining data를 몇 개로 나눌지 입력받습니다.
# v <- 2
####################

parameterGrid

# parameter grid를 적용한 cross validation을 수행합니다

grid_search_result <- goophi::gridSerachCV(rec = rec,
                                           model = model,
                                           v = v,
                                           data = data_train,
                                           parameterGrid = parameterGrid
)
grid_search_result


#### (5) Finalize model ####

# 최종 모델 object를 생성합니다

#### classification ####
finalized <- goophi::fitBestModel(gridSearchResult = grid_search_result,
                                  metric = "roc_auc",
                                  model = model,
                                  formula = formula,
                                  trainingData = data_train,
                                  splitedData = data_split)
####################

#### regression ####
# finalized <- goophi::fitBestModel(gridSearchResult = grid_search_result,
#                                   metric = "rmse",
#                                   model = model,
#                                   formula = formula,
#                                   trainingData = data_train,
#                                   splitedData = data_split)
####################


final_model <- finalized[[1]]
last_fitted_model <- finalized[[2]]

final_model
last_fitted_model

#### classification ####

# performance of final model
last_fitted_model %>% collect_metrics()

last_fitted_model[[5]]

# ROC Curve
options(yardstick.event_first = FALSE) # 오름차순으로 factor의 level 설정된다고 가정

last_fitted_model %>%
  tune::collect_predictions() %>%
  dplyr::mutate(.pred_class = as.numeric(.pred_class)) %>%
  yardstick::roc_curve(Survived, .pred_class) %>%
  parsnip::autoplot()

# confusion matrix
last_fitted_model %>%
  tune::collect_predictions() %>%
  yardstick::conf_mat(Survived, .pred_class) %>%
  autoplot(type = "heatmap")

# evaluation index

custom_metrics <- yardstick::metric_set(yardstick::accuracy,
                                        yardstick::sens,
                                        yardstick::spec,
                                        yardstick::precision,
                                        yardstick::recall,
                                        yardstick::f_meas,
                                        yardstick::kap,
                                        yardstick::mcc
)
custom_metrics(last_fitted_model %>%
                 tune::collect_predictions(),
               truth = Survived,
               estimate = .pred_class)
####################

#### regression ####
# last_fitted_model %>% collect_metrics()
#
# ames_test_res <- last_fitted_model$.predictions[[1]][1]
# ames_test_res <- bind_cols(ames_test_res, data_test %>% select(quality))
# ames_test_res
#
# # regression plot
# last_fitted_model %>% collect_metrics()
#
# last_fitted_model %>%
#   tune::collect_predictions() %>%
#   ggplot(aes(x = quality, y = last_fitted_model$.predictions[[1]]$.pred)) + geom_abline(color = "gray50", lty = 2) + geom_point(alpha = 0.5) + coord_obs_pred()
#
# # evaluation index
#
# ames_metrics <- metric_set(rmse, rsq, mae)
# ames_metrics(ames_test_res, truth = quality, estimate = .pred)

####################



