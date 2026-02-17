library("pacman")
p_load(tidyverse, rlang, ggpubr, tools)
# source("pretty_graphs")

##Fig 2---- 

# FA_split_abs FA_split_rel

FA_split_abs <-
  FA_split_abs + 
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(margin = margin(r = 10, l = 5)))

FA_split_rel <-
  FA_split_rel + 
  theme(axis.title.x=element_text(margin = margin(t = 10))) +
  theme(axis.title.y=element_text(margin = margin(r = 10, l = 5)))
  

Fig_2_FA_split_abs_rel <-
  ggarrange(
  FA_split_abs,
  FA_split_rel,
  common.legend = T,
  legend = "bottom",
  nrow = 2, 
  ncol = 1,
  align = "v"
) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1)) +
  theme(plot.margin = margin(l=0.25,r=0.25,t= 0.5, b= 0.5,unit = "in"))

Fig_2_FA_split_abs_rel

ggsave(filename = "Fig_2_FA_split_abs_rel.pdf", plot = Fig_2_FA_split_abs_rel, path = "./plots/", width = 8.5, height = 11, units = "in", dpi = 320)

##Fig 3---- 

# LA_split_abs LA_split_rel

LA_split_abs <-
  LA_split_abs + 
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(margin = margin(r = 10, l = 5)))

LA_split_rel <-
  LA_split_rel + 
  theme(axis.title.x=element_text(margin = margin(t = 10))) +
  theme(axis.title.y=element_text(margin = margin(r = 10, l = 5)))


Fig_3_LA_split_abs_rel <-
  ggarrange(
    LA_split_abs,
    LA_split_rel,
    common.legend = T,
    legend = "bottom",
    nrow = 2, 
    ncol = 1,
    align = "v"
  ) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1)) +
  theme(plot.margin = margin(l=0.25,r=0.25,t= 0.5, b= 0.5,unit = "in"))

Fig_3_LA_split_abs_rel

ggsave(filename = "Fig_3_LA_split_abs_rel.pdf", plot = Fig_3_LA_split_abs_rel, path = "./plots/", width = 8.5, height = 11, units = "in", dpi = 320)
##Fig 4---- 

# FA_split_abs_general FA_split_rel_general

FA_split_abs_general <-
  FA_split_abs_general + 
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(margin = margin(r = 10, l = 5)))

FA_split_rel_general <-
  FA_split_rel_general + 
  theme(axis.title.x=element_text(margin = margin(t = 10))) +
  theme(axis.title.y=element_text(margin = margin(r = 10, l = 5)))


Fig_4_FA_split_abs_rel_general <-
  ggarrange(
    FA_split_abs_general,
    FA_split_rel_general,
    common.legend = T,
    legend = "bottom",
    nrow = 2, 
    ncol = 1,
    align = "v"
  ) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1)) +
  theme(plot.margin = margin(l=0.25,r=0.25,t= 0.5, b= 0.5,unit = "in"))

Fig_4_FA_split_abs_rel_general

ggsave(filename = "Fig_4_FA_split_abs_rel_general.pdf", plot = Fig_4_FA_split_abs_rel_general, path = "./plots/", width = 8.5, height = 11, units = "in", dpi = 320)
##Fig 5---- 

# LA_split_abs_general LA_split_rel_general

LA_split_abs_general <-
  LA_split_abs_general + 
  theme(axis.title.x=element_blank()) +
  theme(axis.title.y=element_text(margin = margin(r = 10, l = 5)))

LA_split_rel_general <-
  LA_split_rel_general + 
  theme(axis.title.x=element_text(margin = margin(t = 10))) +
  theme(axis.title.y=element_text(margin = margin(r = 10, l = 5)))


Fig_5_LA_split_abs_rel_general <-
  ggarrange(
    LA_split_abs_general,
    LA_split_rel_general,
    common.legend = T,
    legend = "bottom",
    nrow = 2, 
    ncol = 1,
    align = "v"
  ) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1)) +
  theme(plot.margin = margin(l=0.25,r=0.25,t= 0.5, b= 0.5,unit = "in"))

Fig_5_LA_split_abs_rel_general

ggsave(filename = "Fig_5_LA_split_abs_rel_general.pdf", plot = Fig_5_LA_split_abs_rel_general, path = "./plots/", width = 8.5, height = 11, units = "in", dpi = 320)
##Fig 6----

#FA_func_LA_rel

FA_func_LA_rel <-
  FA_func_LA_rel + 
  theme(axis.title.x=element_text(margin = margin(t = 10))) +
  theme(axis.title.y=element_text(margin = margin(r = 10, l = 5))) +
  theme(legend.position = "bottom")

Fig_6_FA_func_LA_rel <-
  ggarrange(
    FA_func_LA_rel,
    nrow = 1,
    ncol = 1
  ) +
  theme(panel.border = element_rect(colour = "black", fill=NA, linewidth =1)) +
  theme(plot.margin = margin(l=0.25,r=0.25,t= 0.5, b= 0.5,unit = "in"))

Fig_6_FA_func_LA_rel

ggsave(filename = "Fig_6_FA_func_LA_rel.pdf", plot = Fig_6_FA_func_LA_rel, path = "./plots/", width = 8.5, height = 6.65, units = "in", dpi = 320)
