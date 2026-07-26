library("pacman")
p_load(tidyverse)
load(file = "./data/gendered_paper_data")
load(file = "./data/analysis_data")

#----

sci_med <- c("Nature", "Lancet (London, England)", "JAMA", "Science (New York, N.Y.)", "Nature medicine", "The New England journal of medicine", "Science translational medicine", "JAMA surgery")

gendered_paper_data <- 
  gendered_paper_data %>% 
  filter(Journal %in% sci_med)

analysis_data <-
  analysis_data %>% 
  filter(j_type %in% c("gen","med"))

gendered_paper_data %>% 
  filter(Journal %in% sci_med) %>% 
  filter(PubDate >= 2010) %>% 
  filter(PubDate <= 2023) %>% 
  nrow()



num_unique_auth <- 
  gendered_paper_data %>% 
  filter(Journal %in% sci_med) %>% 
  group_by(PubDate) %>% 
  summarise(
    fa_f_num = length(unique(fa_female)),
    fa_m_num = length(unique(fa_male)),
    la_f_num = length(unique(la_female)),
    la_m_num = length(unique(la_male))
  ) %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023)

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

yearly_data <-
  analysis_data %>% 
  group_by(PubDate, fa_gender, la_gender) %>% 
  summarise(Count = sum(Count), .groups = "keep") %>% 
  ungroup() 

colnames(yearly_data) <- c("Year of Publication", "First Author Gender", "Last Author Gender","Number of Articles")

fa_yearly <- 
  yearly_data %>% 
  group_by(`Year of Publication`, `First Author Gender`) %>% 
  summarise(`Number of Articles` = sum(`Number of Articles`)) %>% 
  ungroup()

la_yearly <- 
  yearly_data %>% 
  group_by(`Year of Publication`, `Last Author Gender`) %>% 
  summarise(`Number of Articles` = sum(`Number of Articles`)) %>% 
  ungroup()

#-----

fa_yearly <-
  fa_yearly %>% 
  pivot_wider(names_from = `First Author Gender`, values_from = `Number of Articles`) %>% 
  select(!none)

la_yearly <-
  la_yearly %>% 
  pivot_wider(names_from = `Last Author Gender`, values_from = `Number of Articles`) %>% 
  select(!none) %>% 
  select(!`Year of Publication`)

yearly_sum <- cbind(fa_yearly, la_yearly)

num_per_unique <- yearly_sum/num_unique_auth
num_per_unique[,1] <- yearly_sum[,1]

#-----

colnames(num_unique_auth) <- c("Year of Publication", "Unique Women First Authors", "Unique Male First Authors", "Unique Women Last Authors", "Unique Male Last Authors")

num_unique_auth <-
  num_unique_auth %>% 
  mutate(`Unique First Authors` = `Unique Women First Authors` + `Unique Male First Authors`) %>% 
  mutate(`Unique Last Authors` = `Unique Women Last Authors` + `Unique Male Last Authors`)

auth_fa <-   num_unique_auth[,c(1,2,3,6)] 
colnames(auth_fa)[c(2,3,4)] <- c("Female","Male","Sum")
auth_fa <-
  auth_fa %>% 
  pivot_longer(cols = c("Female","Male","Sum"), names_to = "Unique First Author Gender", values_to = "Number of Unique Authors")

auth_la <-   num_unique_auth[,c(1,4,5,7)] 
colnames(auth_la)[c(2,3,4)] <- c("Female","Male","Sum")
auth_la <-
  auth_la %>% 
  pivot_longer(cols = c("Female","Male","Sum"), names_to = "Unique Last Author Gender", values_to = "Number of Unique Authors")

#-----



colnames(num_per_unique) <- c("Year of Publication", "Female First Author Mean Productivity", "Male First Author Mean Productivity", "Female Last Author Mean Productivity", "Male Last Author Mean Productivity")

num_per_unique <-
  num_per_unique %>% 
  mutate(`First Author Productivity` = `Female First Author Mean Productivity` + `Male First Author Mean Productivity`) %>% 
  mutate(`Last Author Productivity` = `Female Last Author Mean Productivity` + `Male Last Author Mean Productivity`)

prod_fa <-   num_per_unique[,c(1,2,3,6)] 
colnames(prod_fa)[c(2,3,4)] <- c("Female","Male","Sum")
prod_fa <-
  prod_fa %>% 
  pivot_longer(cols = c("Female","Male","Sum"), names_to = "First Author Gender", values_to = "Mean Productivity")

prod_la <-   num_per_unique[,c(1,4,5,7)] 
colnames(prod_la)[c(2,3,4)] <- c("Female","Male","Sum")
prod_la <-
  prod_la %>% 
  pivot_longer(cols = c("Female","Male","Sum"), names_to = "Last Author Gender", values_to = "Mean Productivity")

#---------
auth_fa_gen = auth_fa
save(auth_fa_gen, file = "./data/auth_fa_gen")
auth_la_gen = auth_la
save(auth_la_gen, file = "./data/auth_la_gen")
prod_fa_gen = prod_fa
save(prod_fa_gen, file = "./data/prod_fa_gen")
prod_la_gen = prod_la
save(prod_la_gen, file = "./data/prod_la_gen")