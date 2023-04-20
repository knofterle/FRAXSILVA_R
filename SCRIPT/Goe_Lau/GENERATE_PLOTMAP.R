############################# GENERATE PLOTMAP Goe Lau #########################
# J.Osewold
# 20.03.2023
##### DONE FOR GOE AND LAU #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------

###### REQUIRES ----------------------------------------------------------------
source(file = "SCRIPT/Goe_Lau/Load Tree Data.R")
# tree_data_ansitz, tree_data_polter
source(file = "SCRIPT/Goe_Lau/LOAD_TREES_LAU.R")
# tree_lau
source(file = "SCRIPT/Goe_Lau/GENERATE_PLOTDISTRIBUTION.R")
# plots_pos_goeans, 
# plots_pos_goepol
# plots_ref_goeans
# plots_ref_goepol
# plots_pos_lau
# plots_ref_lau

source(file = "SCRIPT/IBF/generate_plotmap.R")
# plot.all1 und 2

###### NOTES -------------------------------------------------------------------


###### GOE_Ansitz  ---------------------------------------------------------------
map_goe_ans <- plot.all2(tree_data = tree_data_ansitz, plots_pos = plots_pos_goeans,
					plots_ref = plots_ref_goeans)
ggsave(filename = "EXPORT/Goe_Lau/figures/map_goe_ans.pdf", plot = map_goe_ans, 
			 device = "pdf", units = "cm", width = 18, height = 18)
map_goe_ans
plots_ref_goeans

###### GOE_Polter  ---------------------------------------------------------------

map_goe_pol <- plot.all2(tree_data = tree_data_polter, plots_pos = plots_pos_goepol,
					plots_ref = plots_ref_goepol)
ggsave(filename = "EXPORT/Goe_Lau/figures/map_goe_pol.pdf", plot = map_goe_pol, 
			 device = "pdf", units = "cm", width = 18, height = 18)
map_goe_pol
plots_ref_goepol

###### Lauenburg  ---------------------------------------------------------------

tmp <- tree_lau 
str(tree_lau)

source(file = "SCRIPT/Goe_Lau/GENERATE_PLOTDISTRIBUTION.R")
map_lau <- plot.all2(tree_data = tmp, plots_pos = plots_pos_lau,
										 plots_ref = plots_ref_lau, labels = F)
map_lau
ggsave(filename = "EXPORT/Goe_Lau/figures/map_lau.pdf", plot = map_lau, 
			 device = "pdf", units = "cm", width = 18, height = 18)
plots_ref_lau

###### TIDY UP  ----------------------------------------------------------------
rm(plots_ref_goeans, plots_ref_goepol, map_lau, map_goe_ans, map_goe_pol, tmp)

###### OUTPUT ------------------------------------------------------------------
# plots für jede Fläche
# 



###### JUNK --------------------------------------------------------------------
# Old
# Junk
# 
# Stuff 
# I
# dont 
# need 
# anymore
# 
# But might be usefull one time
# HINT: Use STRG + SHIFT + C

