library("pacman")
p_load(tidyverse, rlang, ggpubr, tools)
load(file = "./data/analysis_data")

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

la_sum <-
  sum_data %>% 
  filter(`Last Author Gender` != "None")

both_sum <-
  sum_data %>% 
  filter(`Last Author Gender` != "None") %>% 
  filter(`First Author Gender` != "None")

#------------

collapse_others <- function(my_dat, keeps)
{
  
  keeps <- syms(keeps)
  
  return(
    my_dat %>% 
      group_by(!!!keeps) %>% 
      summarise(`Number of Articles` = sum(`Number of Articles`), .groups = "keep") %>% 
      ungroup()
  )
}
 
#collapse_others(sum_data, c('PubDate'))
formula <- y ~ x
#-------------



#FA Women ~ LA, absolute-------------
both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`First Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  coord_cartesian(xlim = (c(2010,2023)), ylim = c(0,155)) +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  scale_y_continuous(limits = c(-100,300))
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Male") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Male") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()


#FA women ~ LA, relative---------------
both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`First Author Gender` == "Female") %>%  
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  ylab("Proportion of Articles") +
  theme_classic() +
  coord_cartesian(xlim = (c(2010,2023))) +
  geom_point() +
  scale_x_continuous(breaks = 2010:2023) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  scale_shape_manual(values = 17)
#  scale_y_continuous(limits = c(-1,2)) +

both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`First Author Gender` == "Female") %>%  
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`First Author Gender` == "Female") %>%  
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

#first author absolute, gender split-------------
fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `First Author Gender`)) +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023)
  # stat_regline_equation(
  #   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
  #   formula = formula
  # )

fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  filter(`First Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  filter(`First Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  filter(`First Author Gender` == "Male") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  filter(`First Author Gender` == "Male") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()


#first author absolute, sum------------
fa_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`)) +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point(shape = 15) +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) 
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )
fa_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

#first author relative, gender split---------------
fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`First Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `First Author Gender`)) +
  ylab("Proportion of Articles") +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  scale_shape_manual(values = 17)

fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`First Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`First Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

#last author absolute, gender split-------------
la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023)

la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Male") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Male") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

#last author absolute, sum-------------
la_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`)) +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point(shape = 15) +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023)

la_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

la_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

#last author relative, split-----------
la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  ylab("Proportion of Articles") +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  scale_shape_manual(values = 17)

la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()


#first author absolute, gender split, general/gen med-------------
fa_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `First Author Gender`)) +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023)
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

fa_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  filter(`First Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

fa_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  filter(`First Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

fa_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  filter(`First Author Gender` == "Male") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

fa_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  filter(`First Author Gender` == "Male") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()


#first author absolute, sum, general/gen med------------
fa_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`)) +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point(shape = 15) +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) 
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )
fa_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication")) %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

fa_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication")) %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

#first author relative, gender split, general/gen med---------------
fa_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`First Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `First Author Gender`)) +
  ylab("Proportion of Articles") +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  scale_shape_manual(values = 17)

fa_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`First Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

fa_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`First Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

#last author absolute, gender split, general/gen med-------------
la_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023)

la_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

la_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

la_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Male") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

la_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  filter(`Last Author Gender` == "Male") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

#last author absolute, sum, general/gen med-------------
la_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`)) +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point(shape = 15) +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023)

la_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication")) %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

la_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication")) %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()

# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

#last author relative, split, general/gen med-----------
la_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  ylab("Proportion of Articles") +
  theme_classic() +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  scale_shape_manual(values = 17)

la_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

la_sum %>% 
  filter(`Journal Type` %in% c("gen","med")) %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  confint()



#-----------

# both_sum %>% 
#   filter(`First Author Gender` == "Female") %>% 
#   collapse_others(c("Year of Publication","Last Author Gender")) %>%
#   group_by(`Year of Publication`) %>% 
#   mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
#   ungroup() %>%
#   ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
#   theme_classic() +
#   stat_smooth(method = "lm", 
#               formula = y ~ x, 
#               geom = "smooth") +
#   geom_point() +
#   ylim(0, NA)


  



#----------------
fa_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  lm(data = ., formula = `Number of Articles` ~ `Year of Publication`) %>% 
  summary()

la_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  lm(data = ., formula = `Number of Articles` ~ `Year of Publication`) %>% 
  summary()
