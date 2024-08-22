p_load(epiDisplay)
load(file = "./data/gendered_paper_data")


FIX_ME <- 1711

#al de is this name 1711
filtered_index <- FIX_ME

gendered_paper_data_filtered <- 
  gendered_paper_data %>% 
  filter(!(fa_male %in% filtered_index)) %>% 
  filter(!(la_male %in% filtered_index)) %>% 
  filter(PubDate >= 2010) %>% 
#  filter(PubDate <= 2022) %>%
  filter(fa_gender != "mix") %>% 
  filter(la_gender != "mix")

analysis_data <-
  gendered_paper_data_filtered %>% 
  dplyr::select(PubDate, Journal, fa_gender, la_gender, Type)

# article_types <-
#   analysis_data$Type %>% 
#   unlist() %>% 
#   str_split(pattern = "!") %>% 
#   unlist() %>% 
#   table() %>% 
#   as.data.frame()
# 
# #cutoff = 100
# 
# article_types <-
#   article_types[article_types[,2] >= 100,1]
# 
# article_types <- as.character(article_types)

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
  
#save(analysis_data, file = "./data/analysis_data")

fa_data <-
  analysis_data %>% 
  filter(fa_gender != "none") %>% 
  mutate(fa_gender = as.factor(fa_gender))
  
la_data <-
  analysis_data %>% 
  filter(la_gender != "none") %>% 
  mutate(la_gender = as.factor(la_gender))

both_data <-
  analysis_data %>% 
  filter(la_gender != "none") %>% 
  filter(fa_gender != "none") %>% 
  mutate(fa_gender = as.factor(fa_gender)) %>% 
  mutate(la_gender = as.factor(la_gender))

my_reg <- glm(fa_gender ~ PubDate + Journal, data = fa_data, family=binomial(link=logit))
summary(my_reg)

my_reg <- glm(fa_gender ~ PubDate + j_type, data = fa_data, family=binomial(link=logit))
summary(my_reg)

# my_reg <- glm(la_gender ~ PubDate + Journal, data = la_data, family=binomial(link=logit))
# summary(my_reg)
# my_reg <- glm(la_gender ~ PubDate + j_type, data = la_data, family=binomial(link=logit))
# summary(my_reg)

both_data$fa_gender <- factor(both_data$fa_gender, levels = c("male", "female"))
both_data$la_gender <- factor(both_data$la_gender, levels = c("male", "female"))

my_reg <- glm(fa_gender ~ PubDate + Journal + la_gender, data = both_data, family=binomial(link=logit))
summary(my_reg)
logistic.display(my_reg) -> temp
write.csv(temp, file = "./data/odds_ratios.csv")
with(summary(my_reg), 1 - deviance/null.deviance)

my_reg <- glm(fa_gender ~ PubDate + j_type + la_gender, data = both_data, family=binomial(link=logit))
summary(my_reg)
logistic.display(my_reg)
with(summary(my_reg), 1 - deviance/null.deviance)

#----------

gendered_paper_data_filtered %>% 
  filter(fa_gender != "none") %>%
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023) %>%
  #  filter(la_gender == "female") %>% 
  ggplot(aes(x = PubDate, fill = fa_gender)) +
  geom_bar(position = position_dodge())

gendered_paper_data_filtered %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023) %>%
  filter(la_gender != "none") %>% 
  #  filter(la_gender == "female") %>% 
  ggplot(aes(x = PubDate, fill = la_gender)) +
  geom_bar(position = position_dodge())

gendered_paper_data_filtered %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023) %>%
  filter(fa_gender != "none") %>% 
  #  filter(la_gender == "female") %>% 
  ggplot(aes(x = PubDate, fill = fa_gender)) +
  geom_bar(position = "fill")

gendered_paper_data_filtered %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023) %>%
  filter(la_gender != "none") %>% 
  #  filter(la_gender == "female") %>% 
  ggplot(aes(x = PubDate, fill = la_gender)) +
  geom_bar(position = "fill")


gendered_paper_data_filtered %>% 
  filter(fa_gender != "none") %>% 
  filter(Journal %in% journals$common_name[journals$type == "nsurg"]) %>% 
  ggplot(aes(x = PubDate, fill = fa_gender)) +
  geom_bar(position = position_dodge())

#spearman r
#----------------------

gendered_paper_data_filtered %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023) %>%
  filter(fa_gender != "none") %>% 
  select(PubDate, fa_gender) %>% 
  table() %>% 
  as.data.frame() -> temp

temp$PubDate <- as.numeric(temp$PubDate)
for (yr in temp$PubDate) {
  temp$Freq[temp$PubDate == yr] <- temp$Freq[temp$PubDate == yr]/ sum(temp$Freq[temp$PubDate == yr])
}

fa_year_table_female <- temp[temp$fa_gender == "female",c(1,3)]
fa_year_table_male <- temp[temp$fa_gender == "male",c(1,3)]
cor(fa_year_table_female$PubDate, fa_year_table_female$Freq, method = "spearman") -> fa_female_r
cor(fa_year_table_male$PubDate, fa_year_table_male$Freq, method = "spearman") -> fa_male_r


gendered_paper_data_filtered %>% 
  filter(PubDate >= 2010) %>%
  filter(PubDate <= 2023) %>%
  filter(la_gender != "none") %>% 
  select(PubDate, la_gender) %>% 
  table() %>% 
  as.data.frame() -> temp

temp$PubDate <- as.numeric(temp$PubDate)
for (yr in temp$PubDate) {
  temp$Freq[temp$PubDate == yr] <- temp$Freq[temp$PubDate == yr]/ sum(temp$Freq[temp$PubDate == yr])
}

la_year_table_female <- temp[temp$la_gender == "female",c(1,3)]
la_year_table_male <- temp[temp$la_gender == "male",c(1,3)]
cor(la_year_table_female$PubDate, la_year_table_female$Freq, method = "spearman") -> la_female_r
cor(la_year_table_male$PubDate, la_year_table_male$Freq, method = "spearman") -> la_male_r

fa_female_r
fa_male_r
la_female_r
la_male_r