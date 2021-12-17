library(glmnetUtils)                    # to run ridge and lasso
source("code/functions/plot_glmnet.R") 

match_train = read_csv("data/clean/match_train.csv")
match_test = read_csv("data/clean/match_test.csv")

glm_fit_multiple = glm(yes_freq ~ . - iid, 
                       family = "binomial", 
                       data = match_train) 

set.seed(1)
ridge_fit = cv.glmnet(yes_freq ~ . - iid,
                      alpha = 0,
                      nfolds = 10,
                      data = match_train)

save(ridge_fit, file = "results/ridge_fit.cda")

set.seed(1)
lasso_fit = cv.glmnet(yes_freq ~ . - iid,
                      alpha = 1,
                      nfolds = 10,
                      data = match_train)

save(lasso_fit, file = "results/ridge_fit.cda")