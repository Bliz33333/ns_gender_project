library(pacman)
p_load(tidyverse)

load(file = "./data/abns_cleaned")
load(file = "./data/pubmed_cleaned")
load(file = "./data/tabulated_data_recoded")
# load(file = "./data/old_gdp")

journals <- read_excel("journals.xlsx")

#-----------



article_matches <- tibble(PMID = pubmed_cleaned$PMID, fa_abns_id = -1, la_abns_id = -1)

article_matches$fa_abns_id <- abns_cleaned$abns_id[match(pubmed_cleaned$fa_full, abns_cleaned$fullname)]
article_matches$la_abns_id <- abns_cleaned$abns_id[match(pubmed_cleaned$la_full, abns_cleaned$fullname)]

((!is.na(article_matches$fa_abns_id)) | (!is.na(article_matches$la_abns_id))) %>% sum()



article_matches <-
  article_matches %>% 
  mutate(fa_gender = abns_cleaned$gender[match(article_matches$fa_abns_id, abns_cleaned$abns_id)]) %>% 
  mutate(la_gender = abns_cleaned$gender[match(article_matches$la_abns_id, abns_cleaned$abns_id)])


gendered_paper_data <- merge(tabulated_data, article_matches)

gendered_paper_data <-
  gendered_paper_data %>% 
  filter((!is.na(fa_gender)) | (!is.na(la_gender)))

save(gendered_paper_data, file = "./data/gendered_paper_data")

analysis_data <-
  gendered_paper_data %>% 
  select(PubDate, Journal, fa_gender, la_gender, Type)

analysis_data <-
  analysis_data %>% 
  mutate(j_type = "n")

analysis_data <-
  analysis_data %>% 
  mutate(Journal = tolower(str_replace_all(Journal,"[[:punct:]]","")))

analysis_data$Journal[analysis_data$Journal == "lancet london england"] <- "lancet"
analysis_data$Journal[analysis_data$Journal == "science new york ny"] <- "science"
analysis_data$Journal[analysis_data$Journal == "journal of neurointerventional surgery"] <- "journal of neuro interventional surgery"

journals_temp <- journals
journals_temp$common_name <- tolower(str_replace_all(journals$common_name, "[[:punct:]]",""))


for (i in 1:nrow(analysis_data)) {
  analysis_data$j_type[i] <-  journals_temp$type[journals_temp$common_name == (analysis_data$Journal[i])]
}

analysis_data <- 
  analysis_data %>% 
  mutate(Journal = as.factor(Journal)) %>% 
  mutate(j_type = as.factor(j_type))

save(analysis_data, file = "./data/analysis_data")

# article_matches %>% 
#   select(fa_abns_id, la_abns_id) %>% 
#   unlist() %>% 
#   table() %>% 
#   View()
# 
# temp <- pubmed_cleaned$fa_full %in% abns_cleaned$fullname
