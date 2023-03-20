############################# GENERATE PLOTMAP #################################
# J.Osewold
# 12.04.22
##### DRAFT #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------
require(dplyr)
###### REQUIRES ----------------------------------------------------------------
source(file = "SCRIPT/IBF/LOAD_DATA_TREE.R")
source(file = "SCRIPT/IBF/GENERATE_PLOTDISTRIBUTION.R")

source(file = "SCRIPT/IBF/generate_plotmap.R")


###### NOTES -------------------------------------------------------------------


###### SCHOTTEN  ---------------------------------------------------------------
plot.all1(tree_data = data_tree_scho, plots_pos = plots_pos_scho,
         plots_ref = plots_ref_scho)
plots_ref_scho

###### MOLLENFELDE  ------------------------------------------------------------
plot.all1(tree_data = data_tree_mol, plots_pos = plots_pos_mol,
         plots_ref = plots_ref_mol)
plots_ref_mol

###### HUY  ------------------------------------------------------------
plot.all1(tree_data = data_tree_huy, plots_pos = plots_pos_huy,
         plots_ref = plots_ref_huy)
plots_ref_huy

###### WEISWEIL  ------------------------------------------------------------
plot.all1(tree_data = data_tree_weis, plots_pos = plots_pos_weis,
         plots_ref = plots_ref_weis)
plots_ref_weis

###### PLATTENWALD  ------------------------------------------------------------
map_platt <- plot.all2(tree_data = data_tree_platt, plots_pos = plots_pos_platt,
         plots_ref = plots_ref_platt)
ggsave(filename = "EXPORT/IBF/figures/map_plattenwald.pdf", plot = map_platt, 
       device = "pdf", units = "cm", width = 18, height = 18)
map_platt
plots_ref_platt

###### GREIFSWALD  ------------------------------------------------------------
tmp <- data_tree_grfw %>% 
	rename(nr = baum_nr, x = x_utm32, y = y_utm32)
	
map_grfw <- plot.all2(tree_data = tmp, plots_pos = plots_pos_grfw,
					plots_ref = plots_ref_grfw)
ggsave(filename = "EXPORT/IBF/figures/map_greifswald.pdf", plot = map_grfw, 
			 device = "pdf", units = "cm", width = 18, height = 18)
map_grfw
plots_ref_grfw

###### TIDY UP  ----------------------------------------------------------------
rm(map_platt, tmp, map_grfw)

###### OUTPUT ------------------------------------------------------------------
# plot
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

