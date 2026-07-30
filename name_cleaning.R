library(pacman)
p_load(stringr, stringi, readxl, tidyverse)

load(file = "./data/tabulated_data_recoded")

tabulated_data <-
  tabulated_data %>% 
  select(PMID, FA_ForeName, FA_LastName, LA_ForeName, LA_LastName) %>% 
  filter

abns_gender_filtered <- 
  read_csv("data/abns_gendering_enriched.csv") %>% 
  filter(ga_accuracy >= 75) %>% 
  filter(ga_gender %in% c("male", "female")) %>% 
  select(`First Name`, `Last Name`, ga_gender)

filt_words <- c("de", "al", "el", "van", "st", "jr", "del", "der", "la", "le", "los", "md", "von","i", "ii", "iii", "iv","v","vi","vii","viii","ix","x", "ma", "mac", "phd","mph", "san", "veer", "ibn", "den", "the", "lancet", "di", "da", "ben", "dos", "des", "ten", "ter", "lo", "of", "du", "do", "sa")

#testing----------

last_name_strings <- 
  abns_gender_filtered$`Last Name` %>% 
  stri_trans_general(id = "Latin-ASCII") %>% 
  tolower() %>%  
  str_replace_all(pattern = "[-,\\.\\(\\)]",replacement = " ") %>%
  str_replace_all(pattern = " +",replacement = " ") %>%
  trimws() %>% 
  str_split(pattern = " ") %>%
  lapply(function(x) x[!(x %in% filt_words)]) %>% 
  `[`(lengths(.)>1) %>%
  unlist() %>% 
  table() %>% 
  sort(decreasing = T) %>% 
  as_tibble()

last_name_strings$n[last_name_strings$. %in% filt_words]

#abns_last_names-----------

abns_last_names <- 
  abns_gender_filtered$`Last Name` %>% 
  stri_trans_general(id = "Latin-ASCII") %>% 
  tolower() %>%  
  str_replace_all(pattern = "[-,\\.\\(\\)]",replacement = " ") %>%
  str_replace_all(pattern = " +",replacement = " ") %>%
  trimws() %>% 
  str_split(pattern = " ") %>%
  lapply(function(x) x[!(x %in% filt_words)])
abns_last_names[lengths(abns_last_names)>2] <- c("")



#abns_first_names----


abns_first_names <- 
  abns_gender_filtered$`First Name` %>% 
  stri_trans_general(id = "Latin-ASCII") %>% 
  tolower() %>%  
  str_replace_all(pattern = "[,\\.\\(\\)]",replacement = " ") %>%
  str_replace_all(pattern = " +",replacement = " ") %>%
  trimws() %>% 
  str_split(pattern = " ") %>%
  lapply(function(x) x[nchar(x)>1]) %>% 
  lapply(function(x) x[1]) %>% 
  unlist()

stopifnot(length(abns_first_names)== nrow(abns_gender_filtered))





#abns_cleaned------

strip_list <- function(this_row, col_num = 3)
{
  this_row[,col_num] <- paste(unlist(this_row[,col_num]), collapse = " ")
  return(this_row)
}

abns_ln_indexed <-
  tibble(abns_id = 1:length(abns_last_names), fn =abns_first_names, ln = abns_last_names, gender = abns_gender_filtered$ga_gender, orig_fn = abns_gender_filtered$`First Name`, orig_ln = abns_gender_filtered$`Last Name`)

abns_cleaned <- tibble(abns_id = integer(1), fn = character(1), ln = character(1), gender = character(1), orig_fn = character(1), orig_ln = character(1), .rows = 2*nrow(abns_ln_indexed))
working_index <- 1
for(i in 1:nrow(abns_ln_indexed))
{
  
  stopifnot((length(abns_ln_indexed$ln[i]))>0, length(abns_ln_indexed$ln[i]) <3)
  
  if(length(abns_ln_indexed$ln[[i]])==2)
  {
    abns_cleaned[working_index,] <- strip_list(abns_ln_indexed[i,])
    working_index <- working_index + 1
    
    this_row <- abns_ln_indexed[i,]
    lnames = abns_ln_indexed$ln[[i]]
    
    this_row <-
      this_row %>% 
      mutate(ln = lnames[1])
    
    abns_cleaned[working_index,] <- this_row
    working_index <- working_index + 1
    
    this_row <-
      this_row %>% 
      mutate(ln = lnames[2])
    
    abns_cleaned[working_index,] <- this_row
    working_index <- working_index + 1
    
  } else {
    abns_cleaned[working_index,] <- strip_list(abns_ln_indexed[i,])
    working_index <- working_index + 1
  }
  
  if(i %% 100 == 0)
  {
    print(i)
  }
  
}

abns_cleaned <-
  abns_cleaned %>% 
  filter(abns_id > 0) %>% 
  mutate(fullname = paste(fn,ln))

# table((abns_cleaned$fullname)) %>% as.data.frame() %>% view()

table(abns_cleaned$fullname, abns_cleaned$gender) %>% 
  as.data.frame() %>% 
  as_tibble() %>% 
  filter(Freq > 0) %>% 
  nrow()

length(unique(abns_cleaned$fullname))

save(abns_cleaned, file = "./data/abns_cleaned")

#########--------
#pubmed_LN----------

pubmed_LN_clean <- function(names)
{
  names %>% 
    stri_trans_general(id = "Latin-ASCII") %>% 
    tolower() %>%  
    str_replace_all(pattern = "[-',\\.\\(\\)]",replacement = " ") %>%
    str_replace_all(pattern = " +",replacement = " ") %>%
    trimws() %>% 
    str_split(pattern = " ") %>%
    lapply(function(x) x[nchar(x)>1]) %>% 
    lapply(function(x) x[!(x %in% filt_words)]) %>% 
    lapply(function(x) paste(x, collapse = " ")) %>% 
    unlist() %>% 
    return()
}

pubmed_FA_last_names <- 
  tabulated_data$FA_LastName %>% 
  pubmed_LN_clean()

# pubmed_FA_last_names %>% 
#   stri_trans_general(id = "Latin-ASCII") %>% 
#   tolower() %>%  
#   str_replace_all(pattern = "[-',\\.\\(\\)]",replacement = " ") %>%
#   str_replace_all(pattern = " +",replacement = " ") %>%
#   trimws() %>% 
#   str_split(pattern = " ") %>%
#   lapply(function(x) x[nchar(x)>1]) %>% 
#   lapply(function(x) x[!(x %in% filt_words)]) %>% 
#   `[`(lengths(.)>2) %>%
#   # unlist() %>% 
#   # table() %>% 
#   # sort(decreasing = T) %>% 
#   # as_tibble() %>% 
#   View()

pubmed_LA_last_names <- 
  tabulated_data$LA_LastName %>% 
  pubmed_LN_clean()

stopifnot(length(pubmed_FA_last_names)==nrow(tabulated_data), length(pubmed_LA_last_names)==nrow(tabulated_data))


#pubmed_FN--------

pubmed_FN_clean <- function(names)
{
  temp <-
    names %>% 
    stri_trans_general(id = "Latin-ASCII") %>% 
    tolower() %>%  
    str_replace_all(pattern = "[,\\.\\(\\)]",replacement = " ") %>%
    str_replace_all(pattern = " +",replacement = " ") %>%
    trimws() %>% 
    str_split(pattern = " ") %>%
    lapply(function(x) x[nchar(x)>1]) %>% 
    lapply(function(x) x[1]) %>% 
    unlist()
  
  temp[is.na(temp)] <- "NA"
  
  return(temp)
  
}

pubmed_FA_first_names <- 
  tabulated_data$FA_ForeName %>% 
  pubmed_FN_clean()

# pubmed_LA_first_names %>% 
#   table() %>% 
#   sort(decreasing = T) %>% 
#   as_tibble() %>% 
#   View()

pubmed_LA_first_names <- 
  tabulated_data$LA_ForeName %>% 
  pubmed_FN_clean()

stopifnot(length(pubmed_FA_first_names)==nrow(tabulated_data), length(pubmed_LA_first_names)==nrow(tabulated_data))

#pubmed_cleaned--------

pubmed_cleaned <-
  tabulated_data %>% 
  mutate(fa_fn = pubmed_FA_first_names) %>% 
  mutate(fa_ln = pubmed_FA_last_names) %>% 
  mutate(la_fn = pubmed_LA_first_names) %>% 
  mutate(la_ln = pubmed_LA_last_names) %>% 
  select(PMID, fa_fn, fa_ln, la_fn, la_ln) %>% 
  mutate(fa_full = paste(fa_fn, fa_ln)) %>% 
  mutate(la_full = paste(la_fn, la_ln))

save(pubmed_cleaned, file = "./data/pubmed_cleaned")


