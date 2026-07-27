library("pacman")
p_load(tidyverse)
load(file = "./data/gendered_paper_data")
load(file = "./data/abns_gender_filtered")

all_attr <-
  gendered_paper_data %>% 
  select(fa_male, fa_female, la_male, la_female) %>% 
  unlist() %>% 
  table() %>% 
  as_tibble()

colnames(all_attr) <- c("auth", "n")

all_attr <-
  all_attr %>% 
  filter(auth > 0) %>% 
  arrange(desc(n)) %>% 
  mutate(auth = as.numeric(auth))

all_attr <-
  all_attr %>% 
  mutate(orig_fa = abns_gender_filtered$`First Name`[all_attr$auth]) %>% 
  mutate(orig_la = abns_gender_filtered$`Last Name`[auth]) %>%
  mutate(clean_fa = names(abns_gender_filtered$`First Name`)[auth]) %>% 
  mutate(clean_la = names(abns_gender_filtered$`Last Name`)[auth])

