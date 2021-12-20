#necessary libraries
library(rpart) 
library(rpart.plot)
library(randomForest)
library(gbm)
library(tidyverse)
library(kableExtra)
library(cowplot)

#download train and test data
match_train = read_csv("data/clean/match_train.csv")
match_test = read_csv("data/clean/match_test.csv")

# fit regression tree
tree_fit = rpart(yes_freq ~ . - iid,
                 method = "anova", 
                 parms = list(split = "gini"), 
                 data = match_train)
#fit deepest tree
set.seed(1)
deepest_tree_fit = rpart(yes_freq ~ . - iid,
                         method = "anova",
                         parms = list(split = "gini"),
                         control = rpart.control(minsplit = 1,
                                                 minbucket = 1,
                                                 cp = 0),
                         data = match_train)

#generate cp table of deepest tree as tibble
cp_table = deepest_tree_fit$cptable %>% as_tibble()

#CP error plot
cp_table %>%
  filter(nsplit >= 2) %>%
  ggplot(aes(x = nsplit + 1, y = xerror,
             ymin = xerror - xstd, ymax = xerror + xstd)) +
  scale_x_log10() + 
  geom_point() + 
  geom_line() +
  geom_errorbar(width = 0.25) +
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed")

#extract optimal tree from CP table
optimal_tree_info = cp_table %>% 
  filter(xerror - xstd < min(xerror)) %>% 
  arrange(nsplit) %>%
  head(1)

#find optimal number of splits in optimal tree
optimal_nsplit = optimal_tree_info$nsplit

#prune deepest tree to get optimal tree
optimal_tree = prune(tree = tree_fit, 
                     cp = optimal_tree_info$CP)

#save optimal tree
save(optimal_tree, file = "results/optimal_tree.Rda")

#plot optimal tree
optimal_tree_plot = rpart.plot(optimal_tree)

#run default random forest fit
rf_fit = randomForest(yes_freq ~ . - iid, method = "anova",
                      data = match_train)

#produce variable importance plot on random forest
varImpPlot(rf_fit, n.var = 10)

#save random forest
save(rf_fit, file = "results/random_forest.Rda")

