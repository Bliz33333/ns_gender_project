load(file = "./data/tabulated_data_recoded")
load(file = "./data/abns_names")
load(file = "./data/abns_raw")
tabulated_data <- tibble(tabulated_data)

articles <- 
  tabulated_data %>% 
  filter(grepl(pattern = "Article", x = Type,fixed = T)) %>% 
  filter(PubDate >= 2010)

temp2 <- sapply(res_data, "[[", c("profile","userType"))

nulls2 <- sapply(temp2, is.null)
nulls2 <- which(nulls2)
                