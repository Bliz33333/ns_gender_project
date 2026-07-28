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

plot_finish <- function(plot_name, temp_plot, x, y, mode)
{
  assign(plot_name, temp_plot, envir = .GlobalEnv)
  
  ggsave(filename = paste0(plot_name,mode, ".pdf"), plot = get(plot_name), path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
  
  log_files[[plot_name]] <<- 
    (
      temp_df %>% 
        lm(.[[y]] ~ .[[x]], data = .) 
    )
}
