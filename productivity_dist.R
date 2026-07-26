
#f_fa----
prod_data_f_fa_yearly <-
  gendered_paper_data %>% 
  select(PubDate, fa_female) %>% 
  group_by(PubDate, fa_female) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  filter(fa_female != 0) %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023)

prod_data_f_fa_all <-
  gendered_paper_data %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023) %>% 
  select(fa_female) %>% 
  group_by(fa_female) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  filter(fa_female != 0)

prod_data_f_fa_yearly %>% 
  filter(PubDate == 2020) %>% 
  select(num) %>% 
  unlist() %>% 
  hist()

prod_data_f_fa_all %>% 
  select(num) %>% 
  unlist() %>% 
  hist()
  

#m_fa----
prod_data_m_fa_yearly <-
  gendered_paper_data %>% 
  select(PubDate, fa_male) %>% 
  group_by(PubDate, fa_male) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  filter(fa_male != 0) %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023)

prod_data_m_fa_all <-
  gendered_paper_data %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023) %>% 
  select(fa_male, FA_LastName, FA_ForeName) %>% 
  group_by(fa_male) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  filter(fa_male != 0)

prod_data_m_fa_yearly %>% 
  filter(PubDate == 2020) %>% 
  select(num) %>% 
  unlist() %>% 
  hist()

prod_data_m_fa_all %>% 
  select(num) %>% 
  unlist() %>% 
  hist()

#f_la----

prod_data_f_la_yearly <-
  gendered_paper_data %>% 
  select(PubDate, la_female) %>% 
  group_by(PubDate, la_female) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  filter(la_female != 0) %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023)

prod_data_f_la_all <-
  gendered_paper_data %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023) %>% 
  select(la_female) %>% 
  group_by(la_female) %>% 
  summarise(num = n()) %>% 
  ungroup() %>% 
  filter(la_female != 0)

prod_data_f_la_yearly %>% 
  filter(PubDate == 2020) %>% 
  select(num) %>% 
  unlist() %>% 
  hist()

prod_data_f_la_all %>% 
  select(num) %>% 
  unlist() %>% 
  hist()