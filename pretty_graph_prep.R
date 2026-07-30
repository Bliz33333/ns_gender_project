library("pacman")
p_load(tidyverse, rlang, ggpubr, tools, readxl)
source(file = "util_funcs.R")

log_files = list()

mode = "_gen"
# mode = ""

if(mode == "")
{
  load(file = "./data/analysis_data")
  load(file = "./data/auth_fa")
  load(file = "./data/auth_la")
  load(file = "./data/prod_fa")
  load(file = "./data/prod_la")
} else if(mode == "_gen")
{
  load(file = "./data/analysis_data")
  load(file = "./data/auth_fa_gen")
  load(file = "./data/auth_la_gen")
  load(file = "./data/prod_fa_gen")
  load(file = "./data/prod_la_gen")
  
  analysis_data <-
    analysis_data %>%
    filter(j_type %in% c("gen", "med")) 
}


#-------------
analysis_data <-
  analysis_data %>% 
  select(PubDate, Journal, fa_gender, la_gender, j_type) %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023) %>% 
  mutate(fa_gender = as.factor(fa_gender)) %>% 
  mutate(la_gender = as.factor(la_gender)) 

analysis_data <-
  analysis_data %>% 
  mutate(Count = 1)

sum_data <-
  analysis_data %>% 
  group_by(PubDate, Journal, fa_gender, la_gender, j_type) %>% 
  summarise(Count = sum(Count), .groups = "keep") %>% 
  ungroup() 

colnames(sum_data) <- c("Year of Publication", "Journal", "First Author Gender", "Last Author Gender", "Journal Type","Number of Articles")

sum_data <-
  sum_data %>% 
  mutate(`First Author Gender` = str_to_title(`First Author Gender`)) %>% 
  mutate(`Last Author Gender` = str_to_title(`Last Author Gender`))

sum_data$`First Author Gender` <- factor(sum_data$`First Author Gender`, levels = c("Male", "Female", "None"))
sum_data$`Last Author Gender` <- factor(sum_data$`Last Author Gender`, levels = c("Male", "Female", "None"))

fa_sum <-
  sum_data %>% 
  filter(`First Author Gender` != "None")
save(fa_sum, file = paste0("./data/fa_sum",mode))

la_sum <-
  sum_data %>% 
  filter(`Last Author Gender` != "None")
save(la_sum, file = paste0("./data/la_sum",mode))

both_sum <-
  sum_data %>% 
  filter(`Last Author Gender` != "None") %>% 
  filter(`First Author Gender` != "None")
save(both_sum, file = paste0("./data/both_sum",mode))

######scatter_fa--------------

temp = auth_fa

colnames(temp)[2] = "First Author Gender"

temp2 = 
  fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender"))
temp3 <- 
  fa_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  mutate("First Author Gender" = "Sum")

temp4 = rbind(temp2, temp3)

fa_merged <- 
  temp %>% 
  merge(temp4, by = c("Year of Publication", "First Author Gender")) %>% 
  merge(prod_fa, by = c("Year of Publication", "First Author Gender"))

save(fa_merged, file = paste0("./data/fa_merged",mode))
######scatter_la--------------

temp = auth_la

colnames(temp)[2] = "Last Author Gender"

temp2 = 
  la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender"))
temp3 <- 
  la_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  mutate("Last Author Gender" = "Sum")

temp4 = rbind(temp2, temp3)

la_merged <- 
  temp %>% 
  merge(temp4, by = c("Year of Publication", "Last Author Gender")) %>% 
  merge(prod_la, by = c("Year of Publication", "Last Author Gender"))

save(la_merged, file = paste0("./data/la_merged",mode))