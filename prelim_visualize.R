

load(file = "./data/fullname_data")

fullname_data %>% 
  filter(FA_NS) %>% 
  filter(FA_Gender != "") %>% 
  filter(PubDate > 2010) %>% 
  filter(PubDate <= 2022) %>% 
  ggplot(aes(x=PubDate, fill = FA_Gender)) +
  geom_bar(position = "dodge")

fullname_data %>% 
  filter(FA_NS) %>% 
  filter(FA_Gender != "") %>% 
  filter(PubDate > 2010) %>% 
  filter(PubDate <= 2022) %>% 
  ggplot(aes(x=PubDate, fill = FA_Gender)) +
  geom_bar(position = "fill")

fullname_data %>% 
  filter(FA_NS) %>% 
  filter(FA_Gender != "") %>% 
  filter(PubDate > 2010) %>% 
  filter(PubDate <= 2022) %>% 
  ggplot(aes(x=PubDate, color = FA_Gender)) +
  geom_line(stat = "count")

fullname_data %>% 
  filter(FA_NS) %>% 
  filter(FA_Gender != "") %>% 
  filter(PubDate > 2010) %>% 
  filter(PubDate <= 2022) %>% 
  ggplot(aes(x=PubDate, color = FA_Gender)) +
  stat_count() +
  geom_point(y=after_stat(count))


fullname_data %>% 
  filter(FA_NS) %>% 
  filter(FA_Gender != "") %>% 
  filter(PubDate > 2010) %>% 
  filter(PubDate <= 2022) %>% 
  ggplot(aes(x=PubDate, color = FA_Gender)) +
  geom_point(aes(y = after_stat(count))) +
  stat_count()
