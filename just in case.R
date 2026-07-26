#first author absolute, gender split, general/gen med-------------
FA_split_abs_general <-
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
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))

ggsave(filename = "FA_split_abs_general.pdf", plot = FA_split_abs_general, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
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
FA_sum_abs_general <-
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
  scale_x_continuous(breaks = 2010:2023)+
  theme(text = element_text(size= 12))

ggsave(filename = "FA_sum_abs_general.pdf", plot = FA_sum_abs_general, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
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
FA_split_rel_general <-
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
  scale_shape_manual(values = 17) +
  theme(text = element_text(size= 12))

ggsave(filename = "FA_split_rel_general.pdf", plot = FA_split_rel_general, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

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
LA_split_abs_general <-
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
  scale_x_continuous(breaks = 2010:2023) +
  theme(text = element_text(size= 12))

ggsave(filename = "LA_split_abs_general.pdf", plot = LA_split_abs_general, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

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
LA_sum_abs_general <-
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
  scale_x_continuous(breaks = 2010:2023)+
  theme(text = element_text(size= 12))

ggsave(filename = "LA_sum_abs_general.pdf", plot = LA_sum_abs_general, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

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
LA_split_rel_general <-
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
  scale_shape_manual(values = 17) +
  theme(text = element_text(size= 12))

ggsave(filename = "LA_split_rel_general.pdf", plot = LA_split_rel_general, path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)

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


