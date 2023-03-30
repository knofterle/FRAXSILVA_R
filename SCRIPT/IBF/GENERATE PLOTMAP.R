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

tmp <- data_tree_scho3 %>% 
	rename(nr = baum_nr, x = x_utm32, y = y_utm32)
map_scho <- plot.all2(tree_data = tmp, plots_pos = plots_pos_scho,
											plots_ref = plots_ref_scho)

ggsave(filename = "EXPORT/IBF/figures/map_schotten.pdf", plot = map_scho, 
			 device = "pdf", units = "cm", width = 18, height = 18)
map_scho

plots_ref_scho

###### MOLLENFELDE  ------------------------------------------------------------
plot.all1(tree_data = data_tree_mol, plots_pos = plots_pos_mol,
         plots_ref = plots_ref_mol)
map_mol <- plot.all2(tree_data = data_tree_mol, plots_pos = plots_pos_mol,
										 plots_ref = plots_ref_mol)
ggsave(filename = "EXPORT/IBF/figures/map_mollenfelde.pdf", plot = map_mol, 
			 device = "pdf", units = "cm", width = 18, height = 18)

plots_ref_mol

###### HUY  ------------------------------------------------------------
plot.all1(tree_data = data_tree_huy, plots_pos = plots_pos_huy,
         plots_ref = plots_ref_huy)
map_huy <- plot.all2(tree_data = data_tree_huy, plots_pos = plots_pos_huy,
											plots_ref = plots_ref_huy)
ggsave(filename = "EXPORT/IBF/figures/map_huy.pdf", plot = map_huy, 
			 device = "pdf", units = "cm", width = 18, height = 18)

plots_ref_huy

###### WEISWEIL  ------------------------------------------------------------
plot.all1(tree_data = data_tree_weis, plots_pos = plots_pos_weis,
         plots_ref = plots_ref_weis)
map_weis <- plot.all2(tree_data = data_tree_weis, plots_pos = plots_pos_weis,
											 plots_ref = plots_ref_weis, labels = F)
map_weis
ggsave(filename = "EXPORT/IBF/figures/map_weisweil.pdf", plot = map_weis, 
			 device = "pdf", units = "cm", width = 18, height = 18)

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
					plots_ref = plots_ref_grfw, labels = F)
ggsave(filename = "EXPORT/IBF/figures/map_greifswald.pdf", plot = map_grfw, 
			 device = "pdf", units = "cm", width = 18, height = 18)
map_grfw
plots_ref_grfw

###### Stegelitz  ------------------------------------------------------------
tmp <- data_tree_steg %>% 
	rename(nr = baum_nr, x = x_utm32, y = y_utm32)

map_steg <- plot.all2(tree_data = tmp, plots_pos = plots_pos_steg,
											plots_ref = plots_ref_steg, labels = F)
map_steg
ggsave(filename = "EXPORT/IBF/figures/map_stegelitz.pdf", plot = map_steg, 
			 device = "pdf", units = "cm", width = 18, height = 18)
plots_ref_steg

###### Leutzsch  ------------------------------------------------------------
tmp <- data_tree_leu %>% 
	rename(nr = baum_nr, x = x_utm32, y = y_utm32)

map_leu <- plot.all2(tree_data = tmp, plots_pos = plots_pos_leu,
											plots_ref = plots_ref_leu, labels = F)
map_leu
ggsave(filename = "EXPORT/IBF/figures/map_leutzsch.pdf", plot = map_leu, 
			 device = "pdf", units = "cm", width = 18, height = 18)
plots_ref_leu

###### Ettersberg  ------------------------------------------------------------
tmp <- data_tree_ett %>% 
	rename(nr = baum_nr, x = x_utm32, y = y_utm32)

map_ett <- plot.all2(tree_data = tmp, plots_pos = plots_pos_ett,
										 plots_ref = plots_ref_ett, labels = F)
map_ett
ggsave(filename = "EXPORT/IBF/figures/map_ettersberg.pdf", plot = map_ett, 
			 device = "pdf", units = "cm", width = 18, height = 18)
plots_ref_ett


###### TIDY UP  ----------------------------------------------------------------
rm(map_platt, tmp, map_grfw, map_steg, map_leu, map_ett)

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

