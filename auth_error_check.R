library("pacman")
p_load(tidyverse)
load(file = "./data/gendered_paper_data")
load(file = "./data/abns_cleaned")

all_attr <-
  gendered_paper_data %>% 
  select(fa_abns_id, la_abns_id) %>% 
  unlist() %>% 
  table() %>% 
  as_tibble()

colnames(all_attr) <- c("abns_id", "n")

all_attr <-
  all_attr %>% 
  filter(abns_id > 0) %>% 
  arrange(desc(n)) %>% 
  mutate(abns_id = as.numeric(abns_id))

all_attr <-
  all_attr %>% 
  mutate(clean_fn = abns_cleaned$fn[match(abns_id, abns_cleaned$abns_id)]) %>% 
  mutate(clean_ln = abns_cleaned$ln[match(abns_id, abns_cleaned$abns_id)]) %>%
  mutate(orig_fn = abns_cleaned$orig_fn[match(abns_id, abns_cleaned$abns_id)]) %>% 
  mutate(orig_ln = abns_cleaned$orig_ln[match(abns_id, abns_cleaned$abns_id)])

gendered_paper_data <-
  gendered_paper_data %>% 
  mutate(FA_name = paste(FA_ForeName, FA_LastName)) %>% 
  mutate(LA_name = paste(LA_ForeName, LA_LastName))

match_extract_both <-
  gendered_paper_data %>% 
  select(fa_abns_id, la_abns_id, FA_name, LA_name) 

match_extract_fa <-
  match_extract_both %>% 
  select(fa_abns_id, FA_name)
colnames(match_extract_fa) <- c("abns_index", "pub_med_name")

match_extract_la <-
  match_extract_both %>% 
  select(la_abns_id, LA_name)
colnames(match_extract_la) <- c("abns_index", "pub_med_name")

match_extract <- 
  rbind(match_extract_fa, match_extract_la) %>% 
  filter(abns_index > 0)

all_attr <- 
  all_attr %>% 
  rowwise() %>% 
  mutate(match_list = list(unique(match_extract$pub_med_name[match_extract$abns_index == abns_id]))) %>% 
  ungroup()

names(all_attr$match_list) = paste(all_attr$orig_fn, all_attr$orig_ln, all_attr$abns_id)  

all_attr <-
  all_attr %>% 
  mutate(num_unique = lengths(match_list)) %>% 
  arrange(desc(num_unique))
  
# View(all_attr$match_list)

save(all_attr, file = "./data/all_attr")  

#----
 
