library("pacman")
p_load(tidyverse, rlang, ggpubr, tools)
source("util_funcs.R")

#----

#Fig_2_FA_split_abs_rel
basic_combine("Fig_2_FA_split_abs_rel", "art_fa_abs_split", "art_fa_rel_split")

#Fig_3_LA_split_abs_rel
basic_combine("Fig_3_LA_split_abs_rel", "art_la_abs_split", "art_la_rel_split")

#Fig_4_FA_split_abs_rel_general
basic_combine("Fig_4_FA_split_abs_rel_general", "art_fa_abs_split", "art_fa_rel_split", mode = "_gen")

#Fig_5_LA_split_abs_rel_general
basic_combine("Fig_5_LA_split_abs_rel_general", "art_la_abs_split", "art_la_rel_split", mode = "_gen")

#Fig_6_FA_func_LA_rel
basic_combine("Fig_6_FA_func_LA_abs_rel", "FA_func_LA_abs", "FA_func_LA_rel")

#Fig_7_auth_FA_split_abs_rel
basic_combine("Fig_7_auth_FA_split_abs_rel", "auth_fa_abs_split","auth_fa_rel_split")

#Fig_8_auth_LA_split_abs_rel
basic_combine("Fig_8_auth_LA_split_abs_rel", "auth_la_abs_split","auth_la_rel_split")

#Fig_9_prod_FA_split_abs_rel
basic_combine("Fig_9_prod_FA_split_abs_rel", "prod_fa_abs_split","prod_fa_rel_split")

#Fig_10_prod_LA_split_abs_rel
basic_combine("Fig_10_prod_LA_split_abs_rel", "prod_la_abs_split","prod_la_rel_split")

#Fig_11_log_ratios
basic_combine("Fig_11_log_ratios", "log_ratios_fa", "log_ratios_la")

#Fig_12_fa_hist
basic_combine("Fig_12_fa_hist", "hist_fa_f", "hist_fa_m")

#Fig_13_la_hist
basic_combine("Fig_13_la_hist", "hist_la_f", "hist_la_m")

#Fig_14_overlap_hist
basic_combine("Fig_14_overlap_hist", "hist_fa_overlap", "hist_la_overlap")
