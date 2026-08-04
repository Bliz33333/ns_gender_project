library("pacman")
p_load(tidyverse)
source(file = "util_funcs.R")

load(file = "./data/gendered_paper_data")

mode = ""

percentile = 0.99

#----

fa_table <- 
  table(gendered_paper_data$fa_abns_id, gendered_paper_data$fa_gender) %>% 
  as.data.frame() %>% 
  as_tibble()

colnames(fa_table) <- c("id", "gender", "freq")

#after_stat(ifelse(count >= 2, count, NA))

f_q <- 
  fa_table %>% 
  filter(gender == "female") %>% 
  filter(freq > 0) %>% 
  dplyr::select(freq) %>% 
  unlist() %>% 
  quantile(percentile)

m_q <- 
  fa_table %>% 
  filter(gender == "male") %>% 
  filter(freq > 0) %>% 
  dplyr::select(freq) %>% 
  unlist() %>% 
  quantile(percentile)

max_q = max(f_q, m_q)

hist_fa_f <-
  fa_table %>% 
  filter(gender == "female") %>%
  filter(freq > 0) %>% 
  ggplot(aes(x=freq)) +
  geom_histogram(color="#e9ecef", bins = min(30,round(f_q))) +
  theme_classic() +
  ylab("Total Number of First Authorship Credits in 2010-2023") +
  xlab("Number of Female Authors")+
  xlim(0, f_q)
plot_name = "hist_fa_f"
ggsave(filename = paste0(plot_name,mode, ".pdf"), plot = get(plot_name), path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
save(list = plot_name, file = paste0("./data/",plot_name,mode))


hist_fa_m <-
  fa_table %>% 
  filter(gender == "male") %>%
  filter(freq > 0) %>% 
  ggplot(aes(x=freq)) +
  geom_histogram(color="#e9ecef", bins = min(30,round(f_q))) +
  theme_classic() +
  ylab("Total Number of First Authorship Credits in 2010-2023") +
  xlab("Number of Male Authors")+
  xlim(0, m_q)
plot_name = "hist_fa_m"
ggsave(filename = paste0(plot_name,mode, ".pdf"), plot = get(plot_name), path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
save(list = plot_name, file = paste0("./data/",plot_name,mode))

hist_fa_overlap <-
  fa_table %>% 
  filter(freq > 0) %>% 
  ggplot(aes(x=freq, fill = gender)) +
  geom_histogram(color="#e9ecef", alpha = 0.5, position = "identity") +
  theme_classic() +
  ylab("Total Number of First Authorship Credits in 2010-2023") +
  xlab("Number of Authors")+
  xlim(0, max_q)
plot_name = "hist_fa_overlap"
ggsave(filename = paste0(plot_name,mode, ".pdf"), plot = get(plot_name), path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
save(list = plot_name, file = paste0("./data/",plot_name,mode))

#----

la_table <- 
  table(gendered_paper_data$la_abns_id, gendered_paper_data$la_gender) %>% 
  as.data.frame() %>% 
  as_tibble()

colnames(la_table) <- c("id", "gender", "freq")

f_q <- 
  la_table %>% 
  filter(gender == "female") %>% 
  filter(freq > 0) %>% 
  dplyr::select(freq) %>% 
  unlist() %>% 
  quantile(percentile)

m_q <- 
  la_table %>% 
  filter(gender == "male") %>% 
  filter(freq > 0) %>% 
  dplyr::select(freq) %>% 
  unlist() %>% 
  quantile(percentile)

max_q = max(f_q, m_q)

hist_la_f <- 
  la_table %>% 
  filter(gender == "female") %>%
  filter(freq > 0) %>% 
  ggplot(aes(x=freq)) +
  geom_histogram(color="#e9ecef", bins = min(30,round(f_q))) +
  theme_classic() +
  ylab("Total Number of Last Authorship Credits in 2010-2023") +
  xlab("Number of Female Authors") +
  xlim(0, f_q)
plot_name = "hist_la_f"
ggsave(filename = paste0(plot_name,mode, ".pdf"), plot = get(plot_name), path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
save(list = plot_name, file = paste0("./data/",plot_name,mode))


hist_la_m <-
  la_table %>% 
  filter(gender == "male") %>%
  filter(freq > 0) %>% 
  ggplot(aes(x=freq)) +
  geom_histogram(color="#e9ecef", bins = min(30,round(f_q))) +
  theme_classic() +
  ylab("Total Number of Last Authorship Credits in 2010-2023") +
  xlab("Number of Male Authors") +
  xlim(0, m_q)
plot_name = "hist_la_m"
ggsave(filename = paste0(plot_name,mode, ".pdf"), plot = get(plot_name), path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
save(list = plot_name, file = paste0("./data/",plot_name,mode))


hist_la_overlap <-
  la_table %>% 
  filter(freq > 0) %>% 
  ggplot(aes(x=freq, fill = gender)) +
  geom_histogram(color="#e9ecef", alpha = 0.5, position = "identity") +
  theme_classic() +
  ylab("Total Number of Last Authorship Credits in 2010-2023") +
  xlab("Number of Authors") +
  xlim(0, max_q)
plot_name = "hist_la_overlap"
ggsave(filename = paste0(plot_name,mode, ".pdf"), plot = get(plot_name), path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
save(list = plot_name, file = paste0("./data/",plot_name,mode))