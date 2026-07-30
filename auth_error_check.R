library("pacman")
p_load(tidyverse)
load(file = "./data/gendered_paper_data_filtered")
load(file = "./data/abns_cleaned")

all_attr <-
  gendered_paper_data_filtered %>% 
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
  mutate(clean_fn = abns_gender_filtered$`First Name`[all_attr$auth]) %>% 
  mutate(clean_ln = abns_gender_filtered$`Last Name`[auth]) %>%
  mutate(orig_fn = names(abns_gender_filtered$`First Name`)[auth]) %>% 
  mutate(orig_ln = names(abns_gender_filtered$`Last Name`)[auth])

gendered_paper_data_filtered <-
  gendered_paper_data_filtered %>% 
  mutate(FA_name = paste(FA_ForeName, FA_LastName)) %>% 
  mutate(LA_name = paste(LA_ForeName, LA_LastName))

match_extract_both <-
  gendered_paper_data_filtered %>% 
  mutate(fa_auth = fa_male + fa_female) %>% 
  mutate(la_auth = la_male + la_female) %>% 
  select(fa_auth, la_auth, FA_name, LA_name) 

match_extract_fa <-
  match_extract_both %>% 
  select(fa_auth, FA_name)
colnames(match_extract_fa) <- c("abns_index", "pub_med_name")

match_extract_la <-
  match_extract_both %>% 
  select(la_auth, LA_name)
colnames(match_extract_la) <- c("abns_index", "pub_med_name")

match_extract <- 
  rbind(match_extract_fa, match_extract_la) %>% 
  filter(abns_index > 0)

all_attr <- 
  all_attr %>% 
  rowwise() %>% 
  mutate(match_list = list(unique(match_extract$pub_med_name[match_extract$abns_index == auth]))) %>% 
  ungroup()

names(all_attr$match_list) = paste(all_attr$orig_fn, all_attr$orig_ln, all_attr$auth)  

all_attr <-
  all_attr %>% 
  mutate(num_unique = lengths(match_list)) %>% 
  arrange(desc(num_unique))
  
View(all_attr$match_list)

save(all_attr, file = "./data/all_attr")  

#----
load(file = "./data/all_attr")  
View(all_attr$match_list)  
