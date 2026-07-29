last_name_strings <- 
  abns_gender_filtered$`Last Name` %>% 
  tolower() %>%  
  str_replace_all(pattern = coll("  "),replacement = " ") %>%
  str_replace_all(pattern = coll("-"),replacement = " ") %>%
  str_replace_all(pattern = coll("  "),replacement = " ") %>%
  str_replace_all(pattern = coll(","),replacement = " ") %>%
  str_replace_all(pattern = coll("  "),replacement = " ") %>%
  str_replace_all(pattern = coll("."),replacement = " ") %>%
  str_replace_all(pattern = coll("  "),replacement = " ") %>%
  str_replace_all(pattern = coll("("),replacement = " ") %>%
  str_replace_all(pattern = coll("  "),replacement = " ") %>%
  str_replace_all(pattern = coll(")"),replacement = " ") %>%
  str_replace_all(pattern = coll("  "),replacement = " ") %>%
  trimws() %>% 
  str_split(pattern = " ") %>%
  `[`(lengths(.)>1) %>%
  unlist() %>% 
  table() %>% 
  sort(decreasing = T) %>% 
  as_tibble()


last_name_strings$n[last_name_strings$. %in% c("de", "al", "el", "van", "st", "jr", "del", "der", "la", "le", "los", "md", "von", "iii", "iv", "ma", "mac", "phd", "san", "veer")]

#12  8  8  7  4  4  3  2  2  2  2  2  2  1  1  1  1  1  1  1
#12  8  8  7  4  3  2  2  2  2  2  2  2  1  1  1  1  1  1  1