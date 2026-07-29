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

plot_finish <- function(plot_name, temp_plot, temp_df, x, y, mode)
{
  
  assign(plot_name, temp_plot, envir = .GlobalEnv)
  
  ggsave(filename = paste0(plot_name,mode, ".pdf"), plot = get(plot_name), path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
  
  # print(temp_df)
  
  
  log_files[[plot_name]] <<- 
    (
      temp_df %>% 
        lm(.[[y]] ~ .[[x]], data = .) 
    )
}

plot_finish_both_gender <- function(plot_name, temp_plot, temp_df, x, y, mode, gend_filt)
{
  assign(plot_name, temp_plot, envir = .GlobalEnv)
  
  ggsave(filename = paste0(plot_name,mode, ".pdf"), plot = get(plot_name), path = "./plots/", width = 7.5, height = 4.5, units = "in", dpi = 320)
  
  log_files[[paste0(plot_name,"_male")]] <<- 
    (
      temp_df %>% 
        filter(.data[[gend_filt]] == "Male") %>% 
        lm(.[[y]] ~ .[[x]], data = .) 
    )
  
  log_files[[paste0(plot_name,"_female")]] <<- 
    (
      temp_df %>% 
        filter(.data[[gend_filt]] == "Female") %>% 
        lm(.[[y]] ~ .[[x]], data = .) 
    )
}
