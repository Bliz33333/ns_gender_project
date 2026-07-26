library("pacman")
p_load(tidyverse, rlang, ggpubr, tools)
load(file = "./data/analysis_data")
load(file = "./data/auth_fa")
load(file = "./data/auth_la")
load(file = "./data/prod_fa")
load(file = "./data/prod_la")


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

ggplot(ratios_merged_fa, aes(x = `Year of Publication`, y = log2ratio, color = name)) +
  geom_col(position = "stack")
