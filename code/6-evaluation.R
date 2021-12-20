library(glmnetUtils)
library(tidyverse)
library(rpart)

match_test = read_csv("data/clean/match_test.csv")

#load linear model
load("results/lm_fit.Rda")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

#load optimal tree
load("results/optimal_tree.Rda")

#load random forest
load("results/random_forest.Rda")

#calculate LM test predictions and RMSE
linear_predictions = predict(lm_fit, newdata = match_test)
linear_RMSE = sqrt(mean((linear_predictions - match_test$yes_freq)^2))

#calculate LM train predictions and RMSE
linear_train = predict(lm_fit, newdata = match_train)
linear_train_RMSE = sqrt(mean((linear_train - match_train$yes_freq)^2))

# evaluate ridge test predictions and RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = match_test, 
                            s = "lambda.1se") %>%
  as.numeric()
ridge_RMSE = sqrt(mean((ridge_predictions-match_test$yes_freq)^2))

#calculate ridge train predictions and RMSE
ridge_train = predict(ridge_fit, 
                            newdata = match_train, 
                            s = "lambda.1se") %>%
  as.numeric()
ridge_train_RMSE = sqrt(mean((ridge_train - match_train$yes_freq)^2))

# evaluate lasso test RMSE and predictions
lasso_predictions = predict(lasso_fit, 
                            newdata = match_test, 
                            s = "lambda.1se") %>%
  as.numeric()
lasso_RMSE = sqrt(mean((lasso_predictions-match_test$yes_freq)^2))

#evaluate lasso train RMSE and predictions
lasso_train = predict(lasso_fit, 
                      newdata = match_train, 
                      s = "lambda.1se") %>%
  as.numeric()
lasso_train_RMSE = sqrt(mean((lasso_train - match_train$yes_freq)^2))

#evaluate decision tree test predictions and RMSE
dt_predictions = predict(optimal_tree, 
                           newdata = match_test,
                           type="vector")
dt_RMSE = sqrt(mean((dt_predictions - match_test$yes_freq)^2))

#evaluate decision tree train predictions and RMSE
dt_train = predict(optimal_tree, 
                         newdata = match_train,
                         type="vector")
dt_train_RMSE = sqrt(mean((dt_train - match_test$yes_freq)^2))

#calculate random forest test predictions and RMSE
rf_predictions = predict(rf_fit, 
                         newdata = match_test,
                         type = "response")
rf_RMSE = sqrt(mean((rf_predictions - match_test$yes_freq)^2))

#calculate random forest train predictions and RMSE
rf_train = predict(rf_fit, 
                         newdata = match_train,
                         type = "response")
rf_train_RMSE = sqrt(mean((rf_train - match_train$yes_freq)^2))

#form pretty littte tibble of RMSEs <3
rmse = tibble(Method = c("Ordinary Least-Squares", "Ridge", "Lasso", 
                         "Decision Tree", 
                         "Random Forest"), 
              'Train RMSE' = c(linear_train_RMSE, ridge_train_RMSE, 
                               lasso_train_RMSE, dt_train_RMSE,
                               rf_train_RMSE),
       `Test RMSE` = c(linear_RMSE, ridge_RMSE, lasso_RMSE, dt_RMSE,
                       rf_RMSE))

write_csv(x = rmse, file = "results/rmse.csv")