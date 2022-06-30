############################# GENERATE PLOTMAP #################################
# J.Osewold
# 12.04.22
##### DRAFT #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------

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
plots_ref_platt

###### TIDY UP  ----------------------------------------------------------------
rm(map_platt)

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

