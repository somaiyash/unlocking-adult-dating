library(glmnetUtils)                    # to run ridge and lasso
source("code/functions/plot_glmnet.R") 

#load train and test data
match_train = read_csv("data/clean/match_train.csv")
match_test = read_csv("data/clean/match_test.csv")

#fit linear model (OLS) on match_train, response = yes_freq
lm_fit = lm(yes_freq ~ . - iid, data = match_train)

#make residual plot and normal q-q plot of lm_fit
residual_plot = plot(fitted(lm_fit), residuals)
qq_plot = qqnorm(residuals) 
qqline(residuals)

#save linear model
save(lm_fit, file = "results/lm_fit.Rda")

#model ridge_fit
set.seed(1)
ridge_fit = cv.glmnet(yes_freq ~ . - iid,
                      alpha = 0,
                      nfolds = 10,
                      data = match_train)

#save ridge fit
save(ridge_fit, file = "results/ridge_fit.cda")

#save lasso_fit
set.seed(1)
lasso_fit = cv.glmnet(yes_freq ~ . - iid,
                      alpha = 1,
                      nfolds = 10,
                      data = match_train)

#save lasso fit
save(lasso_fit, file = "results/lasso_fit.Rda")
