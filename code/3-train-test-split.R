#download match data
match_data = read_csv("data/clean/match_data.csv")

#generate random 80/20 split in indices
set.seed(5)
n = nrow(match_data)
train_samples = sample(1:n, round(0.8*n))

#select train and test samples
match_train <- match_data %>% slice(train_samples)
match_test <- match_data %>% slice(-train_samples)

#write train and test data to csv to save
write_csv(x = match_train, file = "data/clean/match_train.csv")
write_csv(x = match_test, file = "data/clean/match_test.csv")