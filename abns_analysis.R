load(file = "./data/tabulated_data_recoded")
load(file = "./data/abns_names")
tabulated_data <- tibble(tabulated_data)

articles <- 
  tabulated_data %>% 
  filter(grepl(pattern = "Article", x = Type,fixed = T)) %>% 
  filter(PubDate >= 2010)
