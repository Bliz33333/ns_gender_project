library("pacman")
p_load(tidyverse, rlang, ggpubr, tools)
load(file = "./data/analysis_data")

#-------------
analysis_data <-
  analysis_data %>% 
  select(PubDate, Journal, fa_gender, la_gender) %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023) %>% 
  mutate(fa_gender = as.factor(fa_gender)) %>% 
  mutate(la_gender = as.factor(la_gender)) 

analysis_data <-
  analysis_data %>% 
  mutate(Count = 1)

sum_data <-
  analysis_data %>% 
  group_by(PubDate, Journal, fa_gender, la_gender) %>% 
  summarise(Count = sum(Count), .groups = "keep") %>% 
  ungroup() 

colnames(sum_data) <- c("Year of Publication", "Journal", "First Author Gender", "Last Author Gender", "Number of Articles")

sum_data <-
  sum_data %>% 
  mutate(`First Author Gender` = str_to_title(`First Author Gender`)) %>% 
  mutate(`Last Author Gender` = str_to_title(`Last Author Gender`))

sum_data$`First Author Gender` <- factor(sum_data$`First Author Gender`, levels = c("Male", "Female", "None"))
sum_data$`Last Author Gender` <- factor(sum_data$`Last Author Gender`, levels = c("Male", "Female", "None"))

fa_sum <-
  sum_data %>% 
  filter(`First Author Gender` != "None")

la_sum <-
  sum_data %>% 
  filter(`Last Author Gender` != "None")

both_sum <-
  sum_data %>% 
  filter(`Last Author Gender` != "None") %>% 
  filter(`First Author Gender` != "None")

#------------

collapse_others <- function(my_dat, keeps)
{
  
  keeps <- syms(keeps)
  
  return(
    my_dat %>% 
      group_by(!!!keeps) %>% 
      summarise(`Number of Articles` = sum(`Number of Articles`), .groups = "keep") %>% 
      ungroup()
  )
}
 
#collapse_others(sum_data, c('PubDate'))

#-------------



#formula <- y ~ x


fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `First Author Gender`)) +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA)
  # stat_regline_equation(
  #   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  #   formula = formula
  # )


fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`First Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `First Author Gender`)) +
  ylab("Proportion of Articles") +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA)

la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA)
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )


la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  ylab("Proportion of Articles") +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA)


# both_sum %>% 
#   filter(`First Author Gender` == "Female") %>% 
#   collapse_others(c("Year of Publication","Last Author Gender")) %>%
#   group_by(`Year of Publication`) %>% 
#   mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
#   ungroup() %>%
#   ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
#   theme_classic() +
#   stat_smooth(method = "lm", 
#               formula = y ~ x, 
#               geom = "smooth") +
#   geom_point() +
#   ylim(0, NA)
  