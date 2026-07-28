library("pacman")
p_load(tidyverse, rlang, ggpubr, tools)
source(file = "util_funcs.R")
load(file = "./data/analysis_data")
load(file = "./data/auth_fa")
load(file = "./data/auth_la")
load(file = "./data/prod_fa")
load(file = "./data/prod_la")

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
save(fa_sum, file = "./data/fa_sum")

la_sum <-
  sum_data %>% 
  filter(`Last Author Gender` != "None")
save(la_sum, file = "./data/la_sum")

both_sum <-
  sum_data %>% 
  filter(`Last Author Gender` != "None") %>% 
  filter(`First Author Gender` != "None")
save(both_sum, file = "./data/both_sum")

#------------



#FA Women ~ LA, absolute-------------
FA_func_LA_abs <-
  both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`First Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  coord_cartesian(xlim = (c(2010,2023)), ylim = c(0,155)) +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  scale_y_continuous(limits = c(-100,300)) +
  theme(text = element_text(size= 12))

ggsave(filename = "FA_func_LA_abs.pdf", plot = FA_func_LA_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

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
FA_func_LA_rel <-
  both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`First Author Gender` == "Female") %>%  
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  ylab("Proportion of Articles") +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  coord_cartesian(xlim = (c(2010,2023))) +
  geom_point() +
  scale_x_continuous(breaks = 2010:2023) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  theme(text = element_text(size= 12))

ggsave(filename = "FA_func_LA_rel.pdf", plot = FA_func_LA_rel, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
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

#-------------
#first author absolute, gender split-------------

plot_name = "FA_split_abs"
base_df = "fa_sum"
x = "Year of Publication"
y = "First Author Gender"
shape = "First Author Gender"

temp_df <-
  get(base_df) %>% 
  collapse_others(x, shape)

temp_plot <-
  temp_df %>%
  ggplot(aes(y = .data[[y]], x = .data[[x]], shape = .data[[shape]])) +
  theme_classic() + scale_shape_manual(values = c(
    "Female" = 16,
    "Male" = 17,
    "Sum" = 15
  )) +
  stat_smooth(method = "lm",
              formula = y ~ x,
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = 2010:2023)

plot_finish_both_gender(plot_name, temp_plot, x, y, mode, x)

#first author absolute, sum------------
FA_sum_abs <-
  fa_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  mutate(`First Author Gender` = "Sum") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))

ggsave(filename = "FA_sum_abs.pdf", plot = FA_sum_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)



fa_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  lm(`Number of Articles` ~ `Year of Publication`, data = .) %>% 
  summary()

#first author relative, gender split---------------
FA_split_rel <-
  fa_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`First Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `First Author Gender`)) +
  ylab("Proportion of Articles") +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))

ggsave(filename = "FA_split_rel.pdf", plot = FA_split_rel, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

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
LA_split_abs <-
  la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))

ggsave(filename = "LA_split_abs.pdf", plot = LA_split_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

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
LA_sum_abs <-
  la_sum %>% 
  collapse_others(c("Year of Publication")) %>% 
  mutate(`Last Author Gender` = "Sum") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))

ggsave(filename = "LA_sum_abs.pdf", plot = LA_sum_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

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
LA_split_rel <-
  la_sum %>% 
  collapse_others(c("Year of Publication","Last Author Gender")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  ylab("Proportion of Articles") +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))

ggsave(filename = "LA_split_rel.pdf", plot = LA_split_rel, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

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


#----
#author first author absolute, gender split-------------
auth_FA_split_abs <-
  auth_fa %>% 
  filter(`Unique First Author Gender` %in% c("Female","Male")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Unique Authors`, shape = `Unique First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))


ggsave(filename = "auth_FA_split_abs.pdf", plot = auth_FA_split_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

auth_fa %>% 
  filter(`Unique First Author Gender` == "Female") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  summary()

auth_fa %>% 
  filter(`Unique First Author Gender` == "Female") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  confint()

auth_fa %>% 
  filter(`Unique First Author Gender` == "Male") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  summary()

auth_fa %>% 
  filter(`Unique First Author Gender` == "Male") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  confint()


#author first author absolute, sum------------
auth_FA_sum_abs <-
  auth_fa %>% 
  filter(`Unique First Author Gender` %in% c("Sum")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Unique Authors`, shape = `Unique First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))


ggsave(filename = "auth_FA_sum_abs.pdf", plot = auth_FA_sum_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

auth_fa %>% 
  filter(`Unique First Author Gender` == "Sum") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  summary()

auth_fa %>% 
  filter(`Unique First Author Gender` == "Sum") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  confint()

#author first author relative, gender split---------------
auth_FA_split_rel <-
  auth_fa %>% 
  filter(`Unique First Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Unique Authors` = `Number of Unique Authors`/sum(`Number of Unique Authors`)) %>% 
  ungroup() %>% 
  filter(`Unique First Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Unique Authors`, shape = `Unique First Author Gender`)) +
  ylab("Proportion of Unique Authors") +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))

ggsave(filename = "auth_FA_split_rel.pdf", plot = auth_FA_split_rel, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

auth_fa %>% 
  filter(`Unique First Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Unique Authors` = `Number of Unique Authors`/sum(`Number of Unique Authors`)) %>% 
  ungroup() %>% 
  filter(`Unique First Author Gender` == "Female") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  summary()

auth_fa %>% 
  filter(`Unique First Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Unique Authors` = `Number of Unique Authors`/sum(`Number of Unique Authors`)) %>% 
  ungroup() %>% 
  filter(`Unique First Author Gender` == "Male") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  summary()


#author last author absolute, gender split-------------
auth_LA_split_abs <-
  auth_la %>% 
  filter(`Unique Last Author Gender` %in% c("Female","Male")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Unique Authors`, shape = `Unique Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))


ggsave(filename = "auth_LA_split_abs.pdf", plot = auth_LA_split_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

auth_la %>% 
  filter(`Unique Last Author Gender` == "Female") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  summary()

auth_la %>% 
  filter(`Unique Last Author Gender` == "Female") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  confint()

auth_la %>% 
  filter(`Unique Last Author Gender` == "Male") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  summary()

auth_la %>% 
  filter(`Unique Last Author Gender` == "Male") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  confint()


#author last author absolute, sum------------
auth_LA_sum_abs <-
  auth_la %>% 
  filter(`Unique Last Author Gender` %in% c("Sum")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Unique Authors`, shape = `Unique Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))


ggsave(filename = "auth_LA_sum_abs.pdf", plot = auth_LA_sum_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

auth_la %>% 
  filter(`Unique Last Author Gender` == "Sum") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  summary()

auth_la %>% 
  filter(`Unique Last Author Gender` == "Sum") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  confint()

#author last author relative, gender split---------------
auth_LA_split_rel <-
  auth_la %>% 
  filter(`Unique Last Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Unique Authors` = `Number of Unique Authors`/sum(`Number of Unique Authors`)) %>% 
  ungroup() %>% 
  filter(`Unique Last Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Unique Authors`, shape = `Unique Last Author Gender`)) +
  ylab("Proportion of Unique Authors") +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))

ggsave(filename = "auth_LA_split_rel.pdf", plot = auth_LA_split_rel, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

auth_la %>% 
  filter(`Unique Last Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Unique Authors` = `Number of Unique Authors`/sum(`Number of Unique Authors`)) %>% 
  ungroup() %>% 
  filter(`Unique Last Author Gender` == "Female") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  summary()

auth_la %>% 
  filter(`Unique Last Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Number of Unique Authors` = `Number of Unique Authors`/sum(`Number of Unique Authors`)) %>% 
  ungroup() %>% 
  filter(`Unique Last Author Gender` == "Male") %>% 
  lm(`Number of Unique Authors` ~ `Year of Publication`, data = .) %>% 
  summary()


#------
#prod first author absolute, gender split-------------
prod_FA_split_abs <-
  prod_fa %>% 
  filter(`First Author Gender` %in% c("Female","Male")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Mean Productivity`, shape = `First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))


ggsave(filename = "prod_FA_split_abs.pdf", plot = prod_FA_split_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

prod_fa %>% 
  filter(`First Author Gender` == "Female") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  summary()

prod_fa %>% 
  filter(`First Author Gender` == "Female") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  confint()

prod_fa %>% 
  filter(`First Author Gender` == "Male") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  summary()

prod_fa %>% 
  filter(`First Author Gender` == "Male") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  confint()


#prod first author absolute, sum------------
prod_FA_sum_abs <-
  prod_fa %>% 
  filter(`First Author Gender` %in% c("Sum")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Mean Productivity`, shape = `First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))


ggsave(filename = "prod_FA_sum_abs.pdf", plot = prod_FA_sum_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

prod_fa %>% 
  filter(`First Author Gender` == "Sum") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  summary()

prod_fa %>% 
  filter(`First Author Gender` == "Sum") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  confint()

#prod first author relative, gender split---------------
prod_FA_split_rel <-
  prod_fa %>% 
  filter(`First Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Mean Productivity` = `Mean Productivity`/sum(`Mean Productivity`)) %>% 
  ungroup() %>% 
  filter(`First Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Mean Productivity`, shape = `First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))

ggsave(filename = "prod_FA_split_rel.pdf", plot = prod_FA_split_rel, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

prod_fa %>% 
  filter(`First Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Mean Productivity` = `Mean Productivity`/sum(`Mean Productivity`)) %>% 
  ungroup() %>% 
  filter(`First Author Gender` == "Female") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  summary()

prod_fa %>% 
  filter(`First Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Mean Productivity` = `Mean Productivity`/sum(`Mean Productivity`)) %>% 
  ungroup() %>% 
  filter(`First Author Gender` == "Male") %>% 
  lm(`Mean Productivity`~ `Year of Publication`, data = .) %>% 
  summary()


#prod last author absolute, gender split-------------
prod_LA_split_abs <-
  prod_la %>% 
  filter(`Last Author Gender` %in% c("Female","Male")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Mean Productivity`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))


ggsave(filename = "prod_LA_split_abs.pdf", plot = prod_LA_split_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

prod_la %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  summary()

prod_la %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  confint()

prod_la %>% 
  filter(`Last Author Gender` == "Male") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  summary()

prod_la %>% 
  filter(`Last Author Gender` == "Male") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  confint()


#prod last author absolute, sum------------
prod_LA_sum_abs <-
  prod_la %>% 
  filter(`Last Author Gender` %in% c("Sum")) %>% 
  ggplot(aes(x = `Year of Publication`, y = `Mean Productivity`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))


ggsave(filename = "prod_LA_sum_abs.pdf", plot = prod_LA_sum_abs, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
# stat_regline_equation(
#   aes(label =  paste(..eq.label.., ..adj.rr.label.., sep = "~~~~")),
#   formula = formula
# )

prod_la %>% 
  filter(`Last Author Gender` == "Sum") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  summary()

prod_la %>% 
  filter(`Last Author Gender` == "Sum") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  confint()

#prod last author relative, gender split---------------
prod_LA_split_rel <-
  prod_la %>% 
  filter(`Last Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Mean Productivity` = `Mean Productivity`/sum(`Mean Productivity`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Mean Productivity`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))

ggsave(filename = "prod_LA_split_rel.pdf", plot = prod_LA_split_rel, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

prod_la %>% 
  filter(`Last Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Mean Productivity` = `Mean Productivity`/sum(`Mean Productivity`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Female") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  summary()

prod_la %>% 
  filter(`Last Author Gender` %in% c("Female","Male")) %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Mean Productivity` = `Mean Productivity`/sum(`Mean Productivity`)) %>% 
  ungroup() %>% 
  filter(`Last Author Gender` == "Male") %>% 
  lm(`Mean Productivity` ~ `Year of Publication`, data = .) %>% 
  summary()


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

#scatter fa num----
scatter_FA_F_num <-
  fa_merged %>% 
  filter(`First Author Gender` == "Female") %>% 
  ggplot(aes(x = `Number of Unique Authors`, y = `Number of Articles`, shape = `First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12))

ggsave(filename = "scatter_FA_F_num.pdf", plot = scatter_FA_F_num, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

fa_merged %>% 
  filter(`First Author Gender` == "Female") %>%
  lm(`Number of Articles` ~ `Number of Unique Authors`, data = .) %>% 
  summary()



scatter_FA_M_num <-
  fa_merged %>% 
  filter(`First Author Gender` == "Male") %>% 
  ggplot(aes(x = `Number of Unique Authors`, y = `Number of Articles`, shape = `First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12))

fa_merged %>% 
  filter(`First Author Gender` == "Male") %>%
  lm(`Number of Articles` ~ `Number of Unique Authors`, data = .) %>% 
  summary()

ggsave(filename = "scatter_FA_M_num.pdf", plot = scatter_FA_M_num, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)


scatter_FA_sum_num <-
  fa_merged %>% 
  filter(`First Author Gender` == "Sum") %>% 
  ggplot(aes(x = `Number of Unique Authors`, y = `Number of Articles`, shape = `First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12))

ggsave(filename = "scatter_FA_sum_num.pdf", plot = scatter_FA_sum_num, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

fa_merged %>% 
  filter(`First Author Gender` == "Sum") %>%
  lm(`Number of Articles` ~ `Number of Unique Authors`, data = .) %>% 
  summary()

#scatter fa prod----
scatter_FA_F_prod <-
  fa_merged %>% 
  filter(`First Author Gender` == "Female") %>% 
  ggplot(aes(x = `Mean Productivity`, y = `Number of Articles`, shape = `First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12))

ggsave(filename = "scatter_FA_F_prod.pdf", plot = scatter_FA_F_prod, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

fa_merged %>% 
  filter(`First Author Gender` == "Female") %>%
  lm(`Number of Articles` ~ `Mean Productivity`, data = .) %>% 
  summary()



scatter_FA_M_prod <-
  fa_merged %>% 
  filter(`First Author Gender` == "Male") %>% 
  ggplot(aes(x = `Mean Productivity`, y = `Number of Articles`, shape = `First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12)) 

ggsave(filename = "scatter_FA_M_prod.pdf", plot = scatter_FA_M_prod, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

fa_merged %>% 
  filter(`First Author Gender` == "Male") %>%
  lm(`Number of Articles` ~ `Mean Productivity`, data = .) %>% 
  summary()



scatter_FA_sum_prod <-
  fa_merged %>% 
  filter(`First Author Gender` == "Sum") %>% 
  ggplot(aes(x = `Mean Productivity`, y = `Number of Articles`, shape = `First Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12)) 

ggsave(filename = "scatter_FA_sum_prod.pdf", plot = scatter_FA_sum_prod, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

fa_merged %>% 
  filter(`First Author Gender` == "Sum") %>%
  lm(`Number of Articles` ~ `Mean Productivity`, data = .) %>% 
  summary()



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

#scatter la num----
scatter_LA_F_num <-
  la_merged %>% 
  filter(`Last Author Gender` == "Female") %>% 
  ggplot(aes(x = `Number of Unique Authors`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12))

ggsave(filename = "scatter_LA_F_num.pdf", plot = scatter_LA_F_num, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

la_merged %>% 
  filter(`Last Author Gender` == "Female") %>%
  lm(`Number of Articles` ~ `Number of Unique Authors`, data = .) %>% 
  summary()



scatter_LA_M_num <-
  la_merged %>% 
  filter(`Last Author Gender` == "Male") %>% 
  ggplot(aes(x = `Number of Unique Authors`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12)) 

ggsave(filename = "scatter_LA_M_num.pdf", plot = scatter_LA_M_num, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

la_merged %>% 
  filter(`Last Author Gender` == "Male") %>%
  lm(`Number of Articles` ~ `Number of Unique Authors`, data = .) %>% 
  summary()



scatter_LA_sum_num <-
  la_merged %>% 
  filter(`Last Author Gender` == "Sum") %>% 
  ggplot(aes(x = `Number of Unique Authors`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12)) 

ggsave(filename = "scatter_LA_sum_num.pdf", plot = scatter_LA_sum_num, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

la_merged %>% 
  filter(`Last Author Gender` == "Sum") %>%
  lm(`Number of Articles` ~ `Number of Unique Authors`, data = .) %>% 
  summary()

#scatter la prod----
scatter_LA_F_prod <-
  la_merged %>% 
  filter(`Last Author Gender` == "Female") %>% 
  ggplot(aes(x = `Mean Productivity`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12)) 

ggsave(filename = "scatter_LA_F_prod.pdf", plot = scatter_LA_F_prod, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

la_merged %>% 
  filter(`Last Author Gender` == "Female") %>%
  lm(`Number of Articles` ~ `Mean Productivity`, data = .) %>% 
  summary()



scatter_LA_M_prod <-
  la_merged %>% 
  filter(`Last Author Gender` == "Male") %>% 
  ggplot(aes(x = `Mean Productivity`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12)) 

ggsave(filename = "scatter_LA_M_prod.pdf", plot = scatter_LA_M_prod, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

la_merged %>% 
  filter(`Last Author Gender` == "Male") %>%
  lm(`Number of Articles` ~ `Mean Productivity`, data = .) %>% 
  summary()



scatter_LA_sum_prod <-
  la_merged %>% 
  filter(`Last Author Gender` == "Sum") %>% 
  ggplot(aes(x = `Mean Productivity`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  geom_point() +
  ylim(0, NA) +
  theme(text = element_text(size= 12)) 

ggsave(filename = "scatter_LA_sum_prod.pdf", plot = scatter_LA_sum_prod, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

la_merged %>% 
  filter(`Last Author Gender` == "Sum") %>%
  lm(`Number of Articles` ~ `Mean Productivity`, data = .) %>% 
  summary()



#-----------

# both_sum %>% 
#   filter(`First Author Gender` == "Female") %>% 
#   collapse_others(c("Year of Publication","Last Author Gender")) %>%
#   group_by(`Year of Publication`) %>% 
#   mutate(`Number of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
#   ungroup() %>%
#   ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
#   theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
#   stat_smooth(method = "lm", 
#               formula = y ~ x, 
#               geom = "smooth") +
#   geom_point() +
#   ylim(0, NA)


  



# #----------------
# fa_sum %>% 
#   collapse_others(c("Year of Publication")) %>% 
#   lm(data = ., formula = `Number of Articles` ~ `Year of Publication`) %>% 
#   summary()
# 
# la_sum %>% 
#   collapse_others(c("Year of Publication")) %>% 
#   lm(data = ., formula = `Number of Articles` ~ `Year of Publication`) %>% 
#   summary()
