log_name = "pretty_graph"
# log_name = "prod_auth"

# mode = "_gen"
mode = ""


load(file = paste0("./data/",log_name,"_log_files",mode))

stats_output <- 
  tibble(.rows = length(log_files)) %>% 
  mutate(plot_name = "") %>%
  mutate(adj_r2 = -1) %>% 
  mutate(pval = -1) %>%
  mutate(confint_low = -1) %>%
  mutate(confint_high = -1) %>% 
  mutate(point_prediction = -1)

for (i in 1:length(log_files)) {
  stats_output$plot_name[i] = names(log_files)[i]
  stats_output$adj_r2[i] = (log_files[[i]] %>% summary())$adj.r.squared
  stats_output$pval[i] = (log_files[[i]] %>% summary())$coefficients[,4][2]
  stats_output$confint_low[i] = (log_files[[i]] %>% confint())[2,][1]
  stats_output$confint_high[i] = (log_files[[i]] %>% confint())[2,][2]
  stats_output$point_prediction[i] = (stats_output$confint_low[i] + stats_output$confint_high[i])/2
}

save(stats_output, file = paste0("./data/",log_name,"_stats_output",mode))
write.csv(stats_output, file = paste0("./data/",log_name,"_stats_output",mode,".csv"))
