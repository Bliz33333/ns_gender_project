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
  save(list = plot_name, file = paste0("./data/",plot_name,mode))
  
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
  save(list = plot_name, file = paste0("./data/",plot_name,mode))
  
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

basic_combine <- function(plot_name, plot1_name, plot2_name, mode = "")
{
  
  load(paste0("./data/",plot1_name,mode))
  load(paste0("./data/",plot2_name,mode))
  
  plot1 <- get(plot1_name)
  plot2 <- get(plot2_name)
  
  plot1 <-
    plot1 + 
    theme(axis.title.x=element_blank()) +
    theme(axis.title.y=element_text(margin = margin(r = 10, l = 5)))
  
  plot2 <-
    plot2 + 
    theme(axis.title.x=element_text(margin = margin(t = 10))) +
    theme(axis.title.y=element_text(margin = margin(r = 10, l = 5)))
  
  
  assign(plot_name,
    ggarrange(
      plot1,
      plot2,
      common.legend = T,
      legend = "bottom",
      nrow = 2, 
      ncol = 1,
      align = "v"
    ) +
    theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1)) +
    theme(plot.margin = margin(l=0.25,r=0.25,t= 0.5, b= 0.5,unit = "in"))
  )
 
  
  ggsave(filename = paste0(plot_name,".pdf"), plot = get(plot_name), path = "./plots/figures/", width = 8.5, height = 11, units = "in", dpi = 320)
}

single_pretty <- function(plot_name, plot1_name, mode="")
{
  
  load(paste0("./data/",plot1_name, mode))
  
  plot1 <- get(plot1_name)
  
  plot1 <-
    plot1 + 
    theme(axis.title.x=element_text(margin = margin(t = 10))) +
    theme(axis.title.y=element_text(margin = margin(r = 10, l = 5))) +
    theme(legend.position = "bottom")
  
  
  assign(plot_name,
         ggarrange(
           plot1,
           nrow = 1, 
           ncol = 1
         ) +
           theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1)) +
           theme(plot.margin = margin(l=0.25,r=0.25,t= 0.5, b= 0.5,unit = "in"))
  )
  
  
  ggsave(filename = paste0(plot_name,".pdf"), plot = get(plot_name), path = "./plots/figures/", width = 8.5, height = 11, units = "in", dpi = 320)
}
