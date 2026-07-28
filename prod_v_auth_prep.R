library("pacman")
p_load(tidyverse, rlang, ggpubr, tools)
source(file = "util_funcs.R")

mode = "_gen"

if(mode == "")
{
  load(file = "./data/analysis_data")
  load(file = "./data/auth_fa")
  load(file = "./data/auth_la")
  load(file = "./data/prod_fa")
  load(file = "./data/prod_la")
  load(file = "./data/fa_sum")
  load(file = "./data/la_sum")
} else if(mode == "_gen"){
  load(file = "./data/analysis_data")
  load(file = "./data/auth_fa_gen")
  auth_fa <- auth_fa_gen
  load(file = "./data/auth_la_gen")
  auth_la <- auth_la_gen
  load(file = "./data/prod_fa_gen")
  prod_fa <- prod_fa_gen
  load(file = "./data/prod_la_gen")
  prod_la <- prod_la_gen
  
  load(file = "./data/fa_sum_gen")
  load(file = "./data/la_sum_gen")
  
  analysis_data <-
    analysis_data %>%
    filter(j_type %in% c("gen", "med")) 
}



#------------
fa_articles <- 
  fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender"))

la_articles <- 
  la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender"))

#-----
auth_ratio_fa <-
  auth_fa %>% 
  filter(`Unique First Author Gender` != "Sum") %>% 
  pivot_wider(names_from = `Unique First Author Gender`, values_from = `Number of Unique Authors`) %>% 
  mutate(auth_ratio = Male/Female) %>% 
  select(`Year of Publication`,auth_ratio)

prod_ratio_fa <-
  prod_fa %>% 
  filter(`First Author Gender` != "Sum") %>% 
  pivot_wider(names_from = `First Author Gender`, values_from = `Mean Productivity`) %>% 
  mutate(prod_ratio = Male/Female) %>% 
  select(`Year of Publication`, prod_ratio)

ratios_merged_fa <- 
  merge(auth_ratio_fa, prod_ratio_fa)

ratios_merged_fa <- 
  ratios_merged_fa %>% 
  mutate(auth_log2ratio = log2(auth_ratio)) %>% 
  mutate(prod_log2ratio = log2(prod_ratio)) %>% 
  select(`Year of Publication`, auth_log2ratio, prod_log2ratio) %>% 
  pivot_longer(cols = c(auth_log2ratio, prod_log2ratio), values_to = "log2ratio")

colnames(auth_fa)[2] = "First Author Gender"

raw_merged_fa <- 
  merge(fa_articles, auth_fa) %>% 
  merge(prod_fa)

save(ratios_merged_fa, file = paste0("./data/ratios_merged_fa",mode))
save(raw_merged_fa, file = paste0("./data/raw_merged_fa",mode))

#----
auth_ratio_la <-
  auth_la %>% 
  filter(`Unique Last Author Gender` != "Sum") %>% 
  pivot_wider(names_from = `Unique Last Author Gender`, values_from = `Number of Unique Authors`) %>% 
  mutate(auth_ratio = Male/Female) %>% 
  select(`Year of Publication`,auth_ratio)

prod_ratio_la <-
  prod_la %>% 
  filter(`Last Author Gender` != "Sum") %>% 
  pivot_wider(names_from = `Last Author Gender`, values_from = `Mean Productivity`) %>% 
  mutate(prod_ratio = Male/Female) %>% 
  select(`Year of Publication`, prod_ratio)

ratios_merged_la <- 
  merge(auth_ratio_la, prod_ratio_la)

ratios_merged_la <- 
  ratios_merged_la %>% 
  mutate(auth_log2ratio = log2(auth_ratio)) %>% 
  mutate(prod_log2ratio = log2(prod_ratio)) %>% 
  select(`Year of Publication`, auth_log2ratio, prod_log2ratio) %>% 
  pivot_longer(cols = c(auth_log2ratio, prod_log2ratio), values_to = "log2ratio")

colnames(auth_la)[2] = "Last Author Gender"

raw_merged_la <- 
  merge(la_articles, auth_la) %>% 
  merge(prod_la)

save(ratios_merged_la, file = paste0("./data/ratios_merged_la",mode))
save(raw_merged_la, file = paste0("./data/raw_merged_la", mode))

#----
