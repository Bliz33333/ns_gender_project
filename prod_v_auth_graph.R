library("pacman")
p_load(tidyverse, readxl)
source("util_funcs.R")

# mode = "_gen"
mode = ""

load(file = paste0("./data/ratios_merged_fa", mode))
load(file = paste0("./data/raw_merged_fa", mode))
load(file = paste0("./data/ratios_merged_la", mode))
load(file = paste0("./data/raw_merged_la", mode))


log_files <- list()

#----
# prod_auth_corr_graphs <- expression({
#   temp_df <-
#     get(base_df) %>%
#     filter(.data[[shape]] == filter)
#   
#   temp_plot <-
#     temp_df %>%
#     ggplot(aes(y = .data[[y]], x = .data[[x]], shape = .data[[shape]])) +
#     theme_classic() + scale_shape_manual(values = c(
#       "Female" = 16,
#       "Male" = 17,
#       "Sum" = 15
#     )) +
#     stat_smooth(method = "lm",
#                 formula = y ~ x,
#                 geom = "smooth") +
#     geom_point() +
#     theme(text = element_text(size = 12)) +
#     
#     scale_x_continuous(breaks = 2010:2023)
#   
#   plot_finish(plot_name, temp_plot, temp_df, x, y, mode)
# })
# 
# 
# 
# 
# 
# 
# schema <- read_excel("prod_v_auth_graph_schema.xlsx")
# 
# for (i in 1:nrow(schema)) {
#   plot_name = schema$plot_name[i]
#   base_df = schema$base_df[i]
#   x = schema$x[i]
#   y = schema$y[i]
#   shape = schema$shape[i]
#   filter = schema$filter[i]
#   
#   eval(prod_auth_corr_graphs)
# }
# 
# save(log_files, file = paste0("./data/prod_auth_log_files",mode))

#log_ratios_fa----

plot_name = "log_ratios_fa"

ratios_merged_fa$name[ratios_merged_fa$name == "auth_log2ratio"] = "Unique Author Ratio"
ratios_merged_fa$name[ratios_merged_fa$name == "prod_log2ratio"] = "Productivity Ratio"

ratios_merged_fa <-
  ratios_merged_fa %>% 
  mutate(`Publication Gap Contributors` = name) %>% 
  mutate(ratio = 2^log2ratio)

log_ratios_fa <-
  ggplot(ratios_merged_fa, aes(x = `Year of Publication`, y = ratio, fill = `Publication Gap Contributors`)) + 
  geom_col(position = "stack") +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(seq(2010,2023,1))) +
  scale_y_continuous(transform = "log2") +
  ylab("Male-to-Female Ratio, First Authors")

ggsave(filename = paste0(plot_name,mode, ".pdf"), plot = get(plot_name), path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
save(list = plot_name, file = paste0("./data/",plot_name,mode))

#log_ratios_la----

plot_name = "log_ratios_la"

ratios_merged_la$name[ratios_merged_la$name == "auth_log2ratio"] = "Unique Author Ratio"
ratios_merged_la$name[ratios_merged_la$name == "prod_log2ratio"] = "Productivity Ratio"

ratios_merged_la <-
  ratios_merged_la %>% 
  mutate(`Publication Gap Contributors` = name) %>% 
  mutate(ratio = 2^log2ratio)

log_ratios_la <-
  ggplot(ratios_merged_la, aes(x = `Year of Publication`, y = ratio, fill = `Publication Gap Contributors`)) + 
  geom_col(position = "stack") +
  theme_classic() +
  theme(text = element_text(size = 12)) +
  scale_x_continuous(breaks = c(seq(2010,2023,1))) +
  scale_y_continuous(transform = "log2") +
  ylab("Male-to-Female Ratio, Last Authors")

ggsave(filename = paste0(plot_name,mode, ".pdf"), plot = get(plot_name), path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
save(list = plot_name, file = paste0("./data/",plot_name,mode))


