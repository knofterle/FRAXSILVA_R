#============================ SAVE_PLOTPOSITION ===============================#
# J.Osewold
# 30.03.2023
# REWORKED
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/IBF/GENERATE_PLOTDISTRIBUTION.R", encoding = "UTF-8")
# plots_pos_xx

source(file = "SCRIPT/IBF/LOAD_NV.R", encoding = "UTF-8")
# data_nv
 
## LIBRARYS --------------------------------------------------------------------
require(dplyr)
## NOTES -----------------------------------------------------------------------


##  MERGE ALL PLOT POSITIONS ---------------------------------------------------
plots_pos_huy$ID_plot <- paste0(plots_pos_huy$nr, "Huy")
plots_pos_mol$ID_plot <- paste0(plots_pos_mol$nr, "Mol")
plots_pos_platt$ID_plot <- paste0(plots_pos_platt$nr, "Pla")
plots_pos_scho$ID_plot <- paste0(plots_pos_scho$nr, "Sch")
plots_pos_weis$ID_plot <- paste0(plots_pos_weis$nr, "Wei")
plots_pos_ett$ID_plot <- paste0(plots_pos_ett$nr, "Ett")
plots_pos_grfw$ID_plot <- paste0(plots_pos_grfw$nr, "Gre")
plots_pos_leu$ID_plot <- paste0(plots_pos_leu$nr, "Leu")
plots_pos_steg$ID_plot <- paste0(plots_pos_steg$nr, "Ste")

plots_pos <-
  rbind(plots_pos_huy,
        plots_pos_mol,
        plots_pos_platt,
        plots_pos_scho,
        plots_pos_weis,
        plots_pos_ett,
        plots_pos_grfw,
        plots_pos_leu,
        plots_pos_steg)
plots_pos <- select(.data = plots_pos, !nr)

##  MERGE NV with plot positions ---------------------------------------------------

data_nv <- left_join(data_nv, plots_pos, by = "ID_plot")

write.csv(data_nv, file = "EXPORT/IBF/tables/data_nv.csv", fileEncoding = "UTF-8")
write.csv(plots_pos, file = "EXPORT/IBF/tables/plotpositions.csv", fileEncoding = "UTF-8")



## TIDY UP  --------------------------------------------------------------------
rm(
  plots_pos_huy,
  plots_pos_mol,
  plots_pos_platt,
  plots_pos_scho,
  plots_pos_weis,
  plots_pos_ett,
  plots_pos_grfw,
  plots_pos_leu,
  plots_pos_steg
)

## OUTPUT ----------------------------------------------------------------------
# data_nv now with plotpositions 
# data_nv.csv exported
# 
# plots_pos
# plotpositionen.csv exported



## JUNK ------------------------------------------------------------------------
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

