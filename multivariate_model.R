library(pacman)
p_load(tidyverse)

load(file="./data/analysis_data")

analysis_data <-
  analysis_data %>% 
  filter((!is.na(fa_gender))) %>% 
  filter((!is.na(la_gender))) %>% 
  filter(PubDate >= 2010) %>% 
  # mutate(fa_gender = factor(fa_gender, levels = c("male", "female"))) %>% 
  # mutate(la_gender = factor(la_gender, levels = c("male", "female")))
  mutate(fa_gender = (fa_gender == "female")) %>% 
  mutate(la_gender = (la_gender == "female"))



# model <- glm(formula = fa_gender ~ PubDate + la_gender, data = analysis_data, family = "binomial")

model <- glm(formula = fa_gender ~ PubDate + Journal + la_gender, data = analysis_data, family = "binomial")

summary(model)
logistic.display(model) -> temp
write.csv(temp, file = "./data/odds_ratios.csv")
with(summary(model), 1 - deviance/null.deviance)

table(analysis_data$Journal) %>% length()
