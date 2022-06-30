#============================ SAVE_PLOTPOSITION ===============================#
# J.Osewold
# 29.06.22
# NEW
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/IBF/GENERATE PLOTMAP.R")
source(file = "SCRIPT/IBF/AGGREGATED TABLES NV.R")
# data_nv_plots
# plots_pos_xx
# 
## LIBRARYS --------------------------------------------------------------------
library(dplyr)
## NOTES -----------------------------------------------------------------------


##  MERGE ALL PLOT POSITIONS ---------------------------------------------------
plots_pos_huy$ID_plot <- paste0(plots_pos_huy$nr, "Huy")
plots_pos_mol$ID_plot <- paste0(plots_pos_mol$nr, "Mol")
plots_pos_platt$ID_plot <- paste0(plots_pos_platt$nr, "Pla")
plots_pos_scho$ID_plot <- paste0(plots_pos_scho$nr, "Sch")
plots_pos_weis$ID_plot <- paste0(plots_pos_weis$nr, "Wei")

plots_pos <-
  rbind(plots_pos_huy,
        plots_pos_mol,
        plots_pos_platt,
        plots_pos_scho,
        plots_pos_weis)
plots_pos <- select(.data = plots_pos, !nr)

data_nv_plots <- merge(data_nv_plots, plots_pos, by = "ID_plot", all = T)

write.csv(data_nv_plots, file = "EXPORT/IBF/tables/data_nv_plots_with_coord.csv")

## TIDY UP  --------------------------------------------------------------------
rm(
  plots_pos_huy,
  plots_pos_mol,
  plots_pos_platt,
  plots_pos_scho,
  plots_pos_weis,
  plots_pos
)

## OUTPUT ----------------------------------------------------------------------
# 



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

