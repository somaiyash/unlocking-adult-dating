#load libraries
library(tidyverse)

file = "data/raw/raw_data.csv"
#raw_data = read_csv(file)

match_data = raw_data %>%
  filter((wave < 6 || wave > 9) && wave != 12) %>% #filter out bad waves
  select(c(iid, id, gender, dec_o, age, attr_o, sinc_o, intel_o, fun_o, amb_o,
           shar_o, field_cd, race, imprace,
           imprelig, goal, date, go_out,
           career_c, sports, tvsports, exercise, dining,
           museums, art, hiking, gaming, clubbing, reading,
           tv, theater, movies, concerts, music, shopping,
           yoga, attr1_1, sinc1_1, intel1_1, fun1_1, 
           amb1_1, shar1_1)) %>%
  na.omit() %>%
  mutate(tally = 1)

match_data = match_data %>%
  group_by(iid) %>%
  mutate(count = sum(tally),
            yes_count = sum(dec_o),
            avg_attr = mean(attr_o),
            avg_sinc = mean(sinc_o),
            avg_intel = mean(intel_o),
            avg_fun = mean(fun_o),
            avg_amb = mean(amb_o),
            avg_shar = mean(shar_o)) %>%
  mutate(yes_freq = yes_count/count) %>%
  select(-c(id, dec_o, attr_o, sinc_o, intel_o, fun_o, amb_o, shar_o,
            count, yes_count, tally)) %>%
  distinct(.keep_all = TRUE) %>%
  rename(field_of_study = field_cd,
         career = career_c,
         attr = attr1_1,
         sinc = sinc1_1,
         intel = intel1_1,
         fun = fun1_1,
         amb = amb1_1,
         shar = shar1_1)