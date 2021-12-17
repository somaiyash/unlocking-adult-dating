library(rpart) 
library(rpart.plot)
library(randomForest)
library(gbm)
library(tidyverse)
library(kableExtra)
library(cowplot)

match_train = read_csv("data/clean/match_train.csv")
match_test = read_csv("data/clean/match_test.csv")

match_train
match_test

# fit classification tree
tree_fit = rpart(yes_freq ~ . - iid,
                 method = "anova", 
                 parms = list(split = "gini"), 
                 data = match_train)
set.seed(1)

deepest_tree_fit = rpart(yes_freq ~ . - iid,
                         method = "anova",
                         parms = list(split = "gini"),
                         control = rpart.control(minsplit = 1,
                                                 minbucket = 1,
                                                 cp = 0),
                         data = match_train)

cp_table = deepest_tree_fit$cptable %>% as_tibble()

cp_table %>%
  filter(nsplit >= 2) %>%
  ggplot(aes(x = nsplit + 1, y = xerror,
             ymin = xerror - xstd, ymax = xerror + xstd)) +
  scale_x_log10() + 
  geom_point() + 
  geom_line() +
  geom_errorbar(width = 0.25) +
  geom_hline(aes(yintercept = min(xerror)), linetype = "dashed")

optimal_tree_info = cp_table %>% 
  filter(xerror - xstd < min(xerror)) %>% 
  arrange(nsplit) %>%
  head(1)

optimal_nsplit = optimal_tree_info$nsplit

optimal_tree = prune(tree = tree_fit, 
                     cp = optimal_tree_info$CP)

optimal_tree_plot = rpart.plot(optimal_tree)