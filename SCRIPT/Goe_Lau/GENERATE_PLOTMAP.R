############################# GENERATE PLOTMAP Goe Lau #########################
# J.Osewold
# 20.03.2023
##### DONE FOR GOE #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------

###### REQUIRES ----------------------------------------------------------------
source(file = "SCRIPT/Goe_Lau/Load Tree Data.R")
source(file = "SCRIPT/Goe_Lau/GENERATE_PLOTDISTRIBUTION.R")

source(file = "SCRIPT/IBF/generate_plotmap.R")


###### NOTES -------------------------------------------------------------------


###### GOE_Ansitz  ---------------------------------------------------------------
plot.all2(tree_data = tree_data_ansitz, plots_pos = plots_pos_goeans,
					plots_ref = plots_ref_goeans)
plots_ref_goeans

###### GOE_Polter  ---------------------------------------------------------------

plot.all2(tree_data = tree_data_polter, plots_pos = plots_pos_goepol,
					plots_ref = plots_ref_goepol)
plots_ref_goepol

###### TIDY UP  ----------------------------------------------------------------
rm(plots_ref_goeans, plots_ref_goepol)

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

