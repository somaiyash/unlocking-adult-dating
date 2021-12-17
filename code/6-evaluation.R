library(glmnetUtils)
library(tidyverse)

match_test = read_csv("data/clean/match_test.csv")

# load ridge fit object
load("results/ridge_fit.Rda")

# load lasso fit object
load("results/lasso_fit.Rda")

# evaluate ridge RMSE
ridge_predictions = predict(ridge_fit, 
                            newdata = match_test, 
                            s = "lambda.1se") %>%
  as.numeric()
ridge_RMSE = sqrt(mean((ridge_predictions-match_test$yes_freq)^2))

# evaluate lasso RMSE
lasso_predictions = predict(lasso_fit, 
                            newdata = match_test, 
                            s = "lambda.1se") %>%
  as.numeric()
lasso_RMSE = sqrt(mean((lasso_predictions-match_test$yes_freq)^2))

tibble(Method = c("Ridge", "Lasso"), `Test RMSE` = c(ridge_RMSE, lasso_RMSE))