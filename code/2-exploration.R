# load libraries
library(kableExtra)              
library(cowplot)                        
library(tidyverse)
library(corrplot)

#download clean match data
match_data = read_csv("data/clean/match_data.csv")

#calculate median yes frequency
yes_freq_median = median(match_data$yes_freq)

#plot histogram of yes frequency distribution
yes_freq_plot = match_data %>%
  ggplot(aes(x = yes_freq)) +
  geom_histogram(color = "black", fill = "grey") +
  labs(x = "Frequency of Yes From Partner",
       y = "Count",
       title = "Histogram of Frequency of Yes From Partner") +
  theme(plot.title = element_text(hjust = 0.5)) +
  geom_vline(xintercept = yes_freq_median, linetype = "dashed")

#save plot as png
ggsave(filename = "results/yes-freq-distribution.png",
       plot = yes_freq_plot,
       device = "png",
       width = 6,
       height = 4)

#split data by gender
females = match_data %>% filter(gender == 0)
males = match_data %>% filter(gender == 1)

#calculate number of each gender
num_females <- nrow(females)
num_males <- nrow(males)

#calculate median yes frequency by gender
median_yes_freq_female <- median(females$yes_freq)
median_yes_freq_male <- median(males$yes_freq)

#boxplot of female yes frequency distribution
female_yes_boxplot =
  females %>%
  ggplot(aes(y = yes_freq)) + 
  geom_boxplot(color = "magenta") +
  labs(y = "Frequency of Yes From Partner",
       title = "Boxplot of Frequency of Yes From Partner for Females") +
  theme(plot.title = element_text(hjust = 0.5))

#boxplot of male yes frequency distribution
male_yes_boxplot = 
  males %>%
  ggplot(aes(y = yes_freq)) +
  geom_boxplot(color = "dodgerblue") +
  labs(y = "Frequency of Yes From Partner",
       title = "Boxplot of Frequency of Yes From Partner for Males")+
  theme(plot.title = element_text(hjust = 0.5))

#plot side-by-side
genderplot <- plot_grid(female_yes_boxplot, male_yes_boxplot, ncol = 2)

#save plot as png
ggsave(filename = "results/gender-boxplots.png",
       plot = genderplot,
       device = "png",
       width = 9,
       height = 6)

#calculate mean female characteristic preference rating
female_pref <- c(mean(females$attr), mean(females$sinc), mean(females$intel),
                 mean(females$fun), mean(females$amb), mean(females$shar))

#calculate mean male characteristic preference rating
male_pref <- c(mean(males$attr), mean(males$sinc), mean(males$intel),
               mean(males$fun), mean(males$amb), mean(males$shar))

#present gendered mean characteristic preference rating in a tibble
pref_table <- tibble(attributes = c('Attractive', 'Sincere', 'Intelligent', 
                                    'Fun', 'Ambitious', 'Shared Interest'),
                     female_pref, male_pref)

#write tibble to a csv to store results
write_csv(x = pref_table, file = "results/gender-preferences.csv")

#calculate mean female acitvity/hobby ratings
female_hobby <- c(mean(females$sports), mean(females$tvsports),
                  mean(females$exercise), mean(females$dining),
                  mean(females$museums),
                  mean(females$art), mean(females$hiking),
                  mean(females$gaming), mean(females$clubbing),
                  mean(females$reading), mean(females$tv),
                  mean(females$theater), mean(females$movies),
                  mean(females$concerts), mean(females$music),
                  mean(females$shopping), mean(females$yoga))

#calculate mean male activity/hobby ratings
male_hobby <- c(mean(males$sports), mean(males$tvsports),
                  mean(males$exercise), mean(males$dining),
                  mean(males$museums),
                  mean(males$art), mean(males$hiking),
                  mean(males$gaming), mean(males$clubbing),
                  mean(males$reading), mean(males$tv),
                  mean(males$theater), mean(males$movies),
                  mean(males$concerts), mean(males$music),
                  mean(males$shopping), mean(males$yoga))

#list of activities
Activities <-c('Sports', 'TV Sports', 'Exercise', 'Dining', 'Museum',
            'Art', 'Hiking', 'Gaming', 'Clubbing', 'Reading', 
            'TV', 'Theater', 'Movies', 'Concerts', 'Music', 'Shopping',
            'Yoga')

#form hobby tibble by gender
hobby_table <- tibble(Activities, 'Female' = female_hobby, 'Male' = male_hobby)
#calculate difference column in tibble
hobby_table <- hobby_table %>%
  mutate('Difference' = female_hobby - male_hobby)

#write to csv
write_csv(x = hobby_table, file = "results/gender-hobbies.csv")

#split data by race
race_1 <- match_data %>% filter(race == 1)
race_2 <- match_data %>% filter(race == 2)
race_3 <- match_data %>% filter(race == 3)
race_4 <- match_data %>% filter(race == 4)
race_5 <- match_data %>% filter(race == 5)
race_6 <- match_data %>% filter(race == 6)

#compute number of people by race
num_race1 <- nrow(race_1)
num_race2 <- nrow(race_2)
num_race3 <- nrow(race_3)
num_race4 <- nrow(race_4)
num_race5 <- nrow(race_5)
num_race6 <- nrow(race_6)

#list of races
Race = c('Black/African American', 
         'European/Caucasian-American', 
         'Latino/Hispanic American',
         'Asian/Pacific Islander/Asian-American',
         'Native American', 'Other')

#form tibble by number of each race
numrace <- tibble(race, 
                  num_races = c(num_race1, num_race2, num_race3, num_race4,
                                num_race5, num_race6))

#write tibble to csv
write_csv(x = numrace, file = "results/race-frequencies.csv")

#calculate mean yes frequency by racw
mean_yes_race1 <- mean(race_1$yes_freq)
mean_yes_race2 <- mean(race_2$yes_freq)
mean_yes_race3 <- mean(race_3$yes_freq)
mean_yes_race4 <- mean(race_4$yes_freq)
mean_yes_race5 <- mean(race_5$yes_freq)
mean_yes_race6 <- mean(race_6$yes_freq)

#write to tibble
yespref_race <- tibble(Race, 
                  'Frequency of Yes' = c(mean_yes_race1, mean_yes_race2, 
                                    mean_yes_race3, mean_yes_race4,
                                mean_yes_race5, mean_yes_race6))

#write to csv to save
write_csv(x = yespref_race, file = "results/race-yes-pref.csv")

#calculate imprace by race
mean_imprace_race1 <- mean(race_1$imprace)
mean_imprace_race2 <- mean(race_2$imprace)
mean_imprace_race3 <- mean(race_3$imprace)
mean_imprace_race4 <- mean(race_4$imprace)
mean_imprace_race5 <- mean(race_5$imprace)
mean_imprace_race6 <- mean(race_6$imprace)

#write to tibble
imprace_race <- tibble(race, 
                       imprace_races = c(mean_imprace_race1, 
                                         mean_imprace_race2, 
                                         mean_imprace_race3, mean_imprace_race4,
                                         mean_imprace_race5, mean_imprace_race6))

#write to csv to save results
write_csv(x = imprace_race, file = "results/race-imprace.csv")

#scatter plot of average attractive rating vs. yes frequency
attr_scatter <-
  match_data %>%
    ggplot(aes(x = avg_attr, y = yes_freq)) + 
  geom_point() +
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_attr,
                                   y = yes_freq),
              se = FALSE,
              color = "red") +
  labs(x = "Average Attractive Rating from Partners (out of 10)",
       y = "Frequency of Yes from Partners")

#scatter plot of average sincere rating vs. yes frequency
sinc_scatter <-
  match_data %>%
  ggplot(aes(x = avg_sinc, y = yes_freq)) + 
  geom_point()+
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_sinc,
                                   y = yes_freq),
              se = FALSE,
              color = "magenta")+
  labs(x = "Average Sincere Rating from Partners (out of 10)",
       y = "Frequency of Yes from Partners")

#scatter plot of average intelligent rating vs. yes frequency
intel_scatter <-
  match_data %>%
  ggplot(aes(x = avg_intel, y = yes_freq)) + 
  geom_point()+
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_intel,
                                   y = yes_freq),
              se = FALSE,
              color = "dodgerblue")+
  labs(x = "Average Intelligent Rating from Partners (out of 10)",
       y = "Frequency of Yes from Partners")

#scatter plot of average fun rating vs. yes frequency
fun_scatter <-
  match_data %>%
  ggplot(aes(x = avg_fun, y = yes_freq)) + 
  geom_point()+
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_fun,
                                   y = yes_freq),
              se = FALSE,
              color="darkgreen")+
  labs(x = "Average Fun Rating from Partners (out of 10)",
       y = "Frequency of Yes from Partners")

#scatter plot of average ambition rating vs. yes frequency
amb_scatter <-
  match_data %>%
  ggplot(aes(x = avg_amb, y = yes_freq)) + 
  geom_point()+
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_amb,
                                   y = yes_freq),
              se = FALSE,
              color = "yellow2")+
  labs(x = "Average Ambitious Rating from Partners (out of 10)",
       y = "Frequency of Yes from Partners")

#scatter plot of average shared interest rating vs. yes frequency
shar_scatter <-
  match_data %>%
  ggplot(aes(x = avg_shar, y = yes_freq)) + 
  geom_point()+
  geom_smooth(method = "lm",
              formula = y ~ x, aes(x = avg_shar,
                                   y = yes_freq),
              se = FALSE,
              color="orange")+
  labs(x = "Average Shared Interest Rating from Partners (out of 10)",
       y = "Frequency of Yes from Partners")

#plot 6 rating plots in a grid
ratingplots <-
  plot_grid(attr_scatter, sinc_scatter, intel_scatter,
            fun_scatter, amb_scatter, shar_scatter)

#save plot grid
ggsave(filename = "results/rating-scatters.png",
       plot = ratingplots,
       device = "png",
       width = 18,
       height = 12)

#plot distribution of subject ages in a histogram
agedist <- match_data %>% ggplot(aes(x = age)) + 
  geom_histogram(binwidth = 2, color = "black", fill = "grey") +
  labs(x = "Age",
       y = "Count",
       title = "Histogram of the Ages of Subjects") +
  geom_vline(xintercept = median(match_data$age),
             linetype = "dashed") +
  theme(plot.title = element_text(hjust = 0.5))

#save plot
ggsave(filename = "results/age-distribution.png",
       plot = agedist,
       device = "png",
       width = 6,
       height = 4)

#find correlation matrix for ratings and put into correlation plot
rating_correlation = cor(match_data %>% select(c(avg_attr, avg_sinc, avg_intel, 
                                                avg_fun, avg_amb, avg_shar)))
rating_corrplot = recordPlot(corrplot(rating_correlation, type = "upper", 
                                      order = "hclust",
                                      tl.col = "black", tl.srt = 45))

#find correlation matrix for demographics and put into correlation plot
demographic_correlation = cor(match_data %>% select(c(gender, age, 
                                                       field_of_study, race,
                                                       career)))
demographic_corrplot = recordPlot(corrplot(demographic_correlation, 
                                           type = "upper", 
                                           order = "hclust", 
                                           tl.col = "black", tl.srt = 45))

#find correlation matrix for preference and put into correlation plot
preference_correlation = cor(match_data %>% select(c(imprace, imprelig, goal)))
preference_corrplot = recordPlot(corrplot(preference_correlation, 
                                          type = "upper", 
                                          order = "hclust", 
                                          tl.col = "black", tl.srt = 45))

#find correlation matrix for activities and put into correlation plot
activities_correlation = cor(match_data %>% select(c(date, go_out, sports, 
                                                      tvsports, exercise, 
                                                      dining, museums, art, 
                                                      hiking,gaming, clubbing, 
                                                      reading,  tv, theater, 
                                                      movies, concerts, music,
                                                      shopping, yoga)))
activities_corrplot = recordPlot(corrplot(activities_correlation, 
                                          type = "upper", order = "hclust", 
                                          tl.col = "black", tl.srt = 45))

#find correlation matrix for all variables and put into correlation plot
correlation = cor(match_data %>% select(-c(iid)))
corrplot = recordPlot(corrplot(correlation, type = "upper", order = "hclust", 
                                          tl.col = "black", tl.srt = 45))

