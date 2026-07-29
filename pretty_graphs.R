library("pacman")
p_load(tidyverse, rlang, ggpubr, tools, readxl)
source(file = "util_funcs.R")

log_files = list()
mode = "_gen"

if(mode == "")
{
  load(file = "./data/analysis_data")
  load(file = "./data/auth_fa")
  load(file = "./data/auth_la")
  load(file = "./data/prod_fa")
  load(file = "./data/prod_la")
  load(file = "./data/fa_sum")
  load(file = "./data/la_sum")
  load(file = "./data/both_sum")
  load(file = "./data/fa_merged")
  load(file = "./data/la_merged")
} else if(mode == "_gen")
{
  load(file = "./data/analysis_data")
  load(file = "./data/auth_fa_gen")
  load(file = "./data/auth_la_gen")
  load(file = "./data/prod_fa_gen")
  load(file = "./data/prod_la_gen")
  
  load(file = "./data/fa_sum_gen")
  load(file = "./data/la_sum_gen")
  load(file = "./data/both_sum_gen")
  load(file = "./data/fa_merged_gen")
  load(file = "./data/la_merged_gen")
  
  analysis_data <-
    analysis_data %>%
    filter(j_type %in% c("gen", "med")) 
}





#-------------

base_pretty_graphs <- expression(
  {
    if(type == "basic")
    {
      temp_df <-
        get(base_df) %>% 
        collapse_others(c(x, shape))
    } else if(type == "time_auth_prod") {
      
      if(split_sum == "split")
      {
        temp_df <- 
          get(base_df) %>% 
          filter(.data[[shape]] %in% c("Female","Male"))
      } else if(split_sum == "sum")
      {
        temp_df <- 
          get(base_df) %>% 
          filter(.data[[shape]] %in% c("Sum"))
      }
    } else if(type == "scatter")
    {
      temp_df <-
        get(base_df)
    }
    
    
    
    if(split_sum == "sum" & type == "basic")
    {
      temp_df <-
        temp_df %>% 
        collapse_others(c(x)) %>% 
        mutate({{shape}} := "Sum")
    }
    
    temp_df <-
      temp_df %>% 
      filter(grepl(shape_filt, .data[[shape]]))
    
    graph_shape_filt <- ""
    
    if(abs_rel == "rel")
    {
      # this_transform = ".data[[y]]/sum(.data[[y]])"
      
      if(subtype=="prod_")
        {
        
        temp_df <- 
          temp_df %>% 
          group_by(.data[[x]]) %>% 
          mutate({{y}} := .data[[y]]/(sum(.data[[y]])-.data[[y]])) %>% 
          ungroup() %>% 
          mutate({{changed_y}} := .data[[y]])  
        
        # this_transform = ".data[[y]]/(sum(.data[[y]])-.data[[y]])"
      } else {
        temp_df <- 
          temp_df %>% 
          group_by(.data[[x]]) %>% 
          mutate({{y}} := .data[[y]]/sum(.data[[y]])) %>% 
          ungroup() %>% 
          mutate({{changed_y}} := .data[[y]])
        }
      

      
      graph_shape_filt = "Female"
      
      y = changed_y
    }
    
    x_breaks <- 2010:2023
    
    if(type == "scatter")
    {
      x_breaks = expression({waiver()})
    }
    
    temp_plot <-
      temp_df %>%
      filter(grepl(graph_shape_filt, .data[[shape]])) %>% 
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
      scale_x_continuous(breaks = (eval(x_breaks)))
    
    if(split_sum == "split")
    {
      plot_finish_both_gender(plot_name, temp_plot, temp_df, x, y, mode, shape)
    } else if(split_sum == "sum"){
      plot_finish(plot_name, temp_plot, temp_df, x, y, mode)
    } else if(split_sum == "sep"){
      plot_finish(plot_name, temp_plot, temp_df, x, y, mode)
    }
  }
)

#------------

#FA_func_LA_abs - FA Women ~ LA, absolute-------------




temp_df <-
  both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`First Author Gender` == "Female")

temp_plot <- 
  temp_df %>% 
  ggplot(aes(x = `Year of Publication`, y = `Number of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  coord_cartesian(xlim = (c(2010,2023)), ylim = c(0,155)) +
  geom_point() +
  ylim(0, NA) +
  scale_x_continuous(breaks = (2010:2023)) +
  scale_y_continuous(limits = c(-100,300)) +
  theme(text = element_text(size= 12))

plot_finish_both_gender("FA_func_LA_abs", temp_plot, temp_df, "Year of Publication", "Number of Articles", mode, "Last Author Gender")


#FA women ~ LA, relative---------------

temp_df <-
  both_sum %>% 
  collapse_others(c("Year of Publication","First Author Gender", "Last Author Gender")) %>% 
  filter(`First Author Gender` == "Female") %>% 
  group_by(`Year of Publication`) %>% 
  mutate(`Proportion of Articles` = `Number of Articles`/sum(`Number of Articles`)) %>% 
  ungroup()

temp_plot <- 
  temp_df %>% 
  filter(`Last Author Gender` == "Female") %>% 
  ggplot(aes(x = `Year of Publication`, y = `Proportion of Articles`, shape = `Last Author Gender`)) +
  theme_classic() +scale_shape_manual(values = c("Female" = 16, "Male" = 17, "Sum" = 15)) +
  stat_smooth(method = "lm", 
              formula = y ~ x, 
              geom = "smooth") +
  coord_cartesian(xlim = (c(2010,2023))) +
  geom_point() +
  scale_x_continuous(breaks = 2010:2023) +
  # scale_y_continuous(limits = c(-100,300)) +
  theme(text = element_text(size= 12))

plot_finish_both_gender("FA_func_LA_rel", temp_plot, temp_df, "Year of Publication", "Proportion of Articles", mode, "Last Author Gender")


#et al----
schema <- read_excel("pretty_graph_schema.xlsx")

schema[is.na(schema)] <- ""

for (i in 1:nrow(schema)) {
  
  type	= schema$type[i]
  subtype	= schema$subtype[i]
  fa_la	= schema$fa_la[i]
  abs_rel	= schema$abs_rel[i]
  split_sum	= schema$split_sum[i]
  shape_filt	= schema$shape_filt[i]
  plot_name	= schema$plot_name[i]
  base_df	= schema$base_df[i]
  x	= schema$x[i]
  y	= schema$y[i]
  changed_y	= schema$changed_y[i]
  shape = schema$shape[i]
  
  eval(base_pretty_graphs)
}

save(log_files, file = paste0("./data/prod_auth_log_files",mode))
#-------------
