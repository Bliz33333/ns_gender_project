library(pacman)
p_load(tidyverse)

load(file = "./data/abns_cleaned")
load(file = "./data/pubmed_cleaned")

#-----------



article_matches <- tibble(PMID = pubmed_cleaned$PMID, fa_abns_id = -1, la_abns_id = -1)

article_matches$fa_abns_id <- match(pubmed_cleaned$fa_full, abns_cleaned$fullname)
article_matches$la_abns_id <- match(pubmed_cleaned$la_full, abns_cleaned$fullname)

((!is.na(article_matches$fa_abns_id)) | (!is.na(article_matches$la_abns_id))) %>% sum()














article_matches %>% 
  select(fa_abns_id, la_abns_id) %>% 
  unlist() %>% 
  table() %>% 
  View()

temp <- pubmed_cleaned$fa_full %in% abns_cleaned$fullname
