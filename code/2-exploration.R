# load libraries
library(kableExtra)              
library(cowplot)                        
library(tidyverse)

#split data by gender
females = match_data %>% filter(gender == 0)
males = match_data %>% filter(gender == 1)

num_females <- nrow(females)
num_males <- nrow(males)

mean_yes_freq_female <- mean(females$yes_freq)
mean_yes_freq_male <- mean(males$yes_freq)

female_yes_boxplot =
  females %>%
  ggplot(aes(y = yes_freq)) + 
  geom_boxplot()

male_yes_boxplot = 
  males %>%
  ggplot(aes(y = yes_freq)) +
  geom_boxplot()

genderplot <- plot_grid(female_yes_boxplot, male_yes_boxplot, ncol = 2,
          labels = c("Female", "Male"))

female_pref <- c(mean(females$attr), mean(females$sinc), mean(females$intel),
                 mean(females$fun), mean(females$amb), mean(females$shar))

male_pref <- c(mean(males$attr), mean(males$sinc), mean(males$intel),
               mean(males$fun), mean(males$amb), mean(males$shar))

pref_table <- tibble(attributes = c('Attractive', 'Sincere', 'Intelligent', 
                                    'Fun', 'Ambitious', 'Shared Interest'),
                     female_pref, male_pref)

female_hobby <- c(mean(females$sports), mean(females$tvsports),
                  mean(females$exercise), mean(females$dining),
                  mean(females$museums),
                  mean(females$art), mean(females$hiking),
                  mean(females$gaming), mean(females$clubbing),
                  mean(females$reading), mean(females$tv),
                  mean(females$theater), mean(females$movies),
                  mean(females$concerts), mean(females$music),
                  mean(females$shopping), mean(females$yoga))

male_hobby <- c(mean(males$sports), mean(males$tvsports),
                  mean(males$exercise), mean(males$dining),
                  mean(males$museums),
                  mean(males$art), mean(males$hiking),
                  mean(males$gaming), mean(males$clubbing),
                  mean(males$reading), mean(males$tv),
                  mean(males$theater), mean(males$movies),
                  mean(males$concerts), mean(males$music),
                  mean(males$shopping), mean(males$yoga))

hobbies <-c('Sports', 'TV Sports', 'Exercise', 'Dining', 'Museum',
            'Art', 'Hiking', 'Gaming', 'Clubbing', 'Reading', 
            'TV', 'Theater', 'Movies', 'Concerts', 'Music', 'Shopping',
            'Yoga')

hobby_table <- tibble(hobbies, female_hobby, male_hobby)
hobby_table <- hobby_table %>%
  mutate(difference = female_hobby - male_hobby)

race_1 <- match_data %>% filter(race == 1)
race_2 <- match_data %>% filter(race == 2)
race_3 <- match_data %>% filter(race == 3)
race_4 <- match_data %>% filter(race == 4)
race_5 <- match_data %>% filter(race == 5)
race_6 <- match_data %>% filter(race == 6)

num_race1 <- nrow(race_1)
num_race2 <- nrow(race_2)
num_race3 <- nrow(race_3)
num_race4 <- nrow(race_4)
num_race5 <- nrow(race_5)
num_race6 <- nrow(race_6)

race = c('Black/African American', 
         'European/Caucasian-American', 
         'Latino/Hispanic American',
         'Asian/Pacific Islander/Asian-American',
         'Native American', 'Other')

numrace <- tibble(race, 
                  num_races = c(num_race1, num_race2, num_race3, num_race4,
                                num_race5, num_race6))

mean_yes_race1 <- mean(race_1$yes_freq)
mean_yes_race2 <- mean(race_2$yes_freq)
mean_yes_race3 <- mean(race_3$yes_freq)
mean_yes_race4 <- mean(race_4$yes_freq)
mean_yes_race5 <- mean(race_5$yes_freq)
mean_yes_race6 <- mean(race_6$yes_freq)

yespref_race <- tibble(race, 
                  yespref_races = c(mean_yes_race1, mean_yes_race2, 
                                    mean_yes_race3, mean_yes_race4,
                                mean_yes_race5, mean_yes_race6))

race2_female <- race_2 %>% filter(gender == 0)
race2_male <- race_2 %>% filter(gender == 1)
race4_female <- race_4 %>% filter(gender == 0)
race4_male <- race_4 %>% filter(gender == 1)

numrace_gender <- tibble(race = c('White', 'White', 'Asian', 'Asian'),
                  gender = c('Female', 'Male', 'Female', 'Male'),
                  num_races = c(nrow(race2_female),
                                nrow(race2_male),
                                nrow(race4_female),
                                nrow(race4_male)))

race2_f_yes <- mean(race2_female$yes_freq)
race2_m_yes <- mean(race2_male$yes_freq)
race4_f_yes <- mean(race4_female$yes_freq)
race4_m_yes <- mean(race4_male$yes_freq)

numrace_gender <- tibble(race = c('White', 'White', 'Asian', 'Asian'),
                         gender = c('Female', 'Male', 'Female', 'Male'),
                         yes_freq = c(race2_f_yes,
                                       race2_m_yes,
                                       race4_f_yes,
                                       race4_m_yes))

mean_imprace_race1 <- mean(race_1$imprace)
mean_imprace_race2 <- mean(race_2$imprace)
mean_imprace_race3 <- mean(race_3$imprace)
mean_imprace_race4 <- mean(race_4$imprace)
mean_imprace_race5 <- mean(race_5$imprace)
mean_imprace_race6 <- mean(race_6$imprace)

imprace_race <- tibble(race, 
                       imprace_races = c(mean_imprace_race1, 
                                         mean_imprace_race2, 
                                         mean_imprace_race3, mean_imprace_race4,
                                         mean_imprace_race5, mean_imprace_race6))

attr_scatter <-
  match_data %>%
    ggplot(aes(x = avg_attr, y = yes_freq)) + 
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_attr,
                                   y = yes_freq),
              se = FALSE)

sinc_scatter <-
  match_data %>%
  ggplot(aes(x = avg_sinc, y = yes_freq)) + 
  geom_point()+
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_sinc,
                                   y = yes_freq),
              se = FALSE)

intel_scatter <-
  match_data %>%
  ggplot(aes(x = avg_intel, y = yes_freq)) + 
  geom_point()+
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_intel,
                                   y = yes_freq),
              se = FALSE)

fun_scatter <-
  match_data %>%
  ggplot(aes(x = avg_fun, y = yes_freq)) + 
  geom_point()+
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_fun,
                                   y = yes_freq),
              se = FALSE)

amb_scatter <-
  match_data %>%
  ggplot(aes(x = avg_amb, y = yes_freq)) + 
  geom_point()+
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_amb,
                                   y = yes_freq),
              se = FALSE)

shar_scatter <-
  match_data %>%
  ggplot(aes(x = avg_shar, y = yes_freq)) + 
  geom_point()+
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_shar,
                                   y = yes_freq),
              se = FALSE)

ratingplots <-
  plot_grid(attr_scatter, sinc_scatter, intel_scatter,
            fun_scatter, amb_scatter, shar_scatter, 
            labels = c("Attractive", "Sincere",
                       "Intelligent", "Fun", "Ambitious",
                       "Shared Interest"))
