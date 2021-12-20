#load libraries
library(tidyverse)

file = "data/raw/raw_data.csv" 
raw_data = read_csv(file) #download raw data file

match_data = raw_data %>%
  filter((wave < 6 || wave > 9) && wave != 12) %>% #filter out bad waves
  select(c(iid, id, gender, dec_o, age, attr_o, sinc_o, intel_o, fun_o, amb_o,
           shar_o, field_cd, race, imprace,
           imprelig, goal, date, go_out,
           career_c, sports, tvsports, exercise, dining,
           museums, art, hiking, gaming, clubbing, reading,
           tv, theater, movies, concerts, music, shopping,
           yoga, attr1_1, sinc1_1, intel1_1, fun1_1, 
           amb1_1, shar1_1)) %>% #select relevant columns
  na.omit() %>% #omit NAs
  mutate(tally = 1) 
#add a tally column to count how many times each iid occurs

match_data = match_data %>%
  group_by(iid) %>% #group by individual id
  mutate(count = sum(tally), #count number of iid occurrences
            yes_count = sum(dec_o), #count number of yeses received by iid
            avg_attr = mean(attr_o), #find mean attractive rating
            avg_sinc = mean(sinc_o), #find mean sincere rating
            avg_intel = mean(intel_o), #find mean intelligence rating
            avg_fun = mean(fun_o), #find mean fun rating
            avg_amb = mean(amb_o), #find mean ambition rating
            avg_shar = mean(shar_o)) %>% #find mean shared interests rating
  mutate(yes_freq = yes_count/count) %>% #compute yes frequency
  select(-c(id, dec_o, attr_o, sinc_o, intel_o, fun_o, amb_o, shar_o,
            count, yes_count, tally)) %>% #delete unneeded columns
  distinct(.keep_all = TRUE) %>% #remove duplicate rows for simplicity
  rename(field_of_study = field_cd,
         career = career_c,
         attr = attr1_1,
         sinc = sinc1_1,
         intel = intel1_1,
         fun = fun1_1,
         amb = amb1_1,
         shar = shar1_1) #rename columns

#Some of our columns are categorical but have the categories defined
#by numbers. Set these column types to factor.
match_data$gender <- as.factor(match_data$gender)
match_data$field_of_study <- as.factor(match_data$field_of_study)
match_data$race <- as.factor(match_data$race)
match_data$goal <- as.factor(match_data$goal)
match_data$date <- as.factor(match_data$date)
match_data$go_out <- as.factor(match_data$go_out)
match_data$career <- as.factor(match_data$career)

#write clean data to csv
write_csv(x = match_data, file="data/clean/match_data.csv")
