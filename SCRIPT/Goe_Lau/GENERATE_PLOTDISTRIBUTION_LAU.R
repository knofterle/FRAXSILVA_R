################ GENERATE PLOT DISTRIBUTION LAU  ##################################
# J.Osewold
# 21.06.22
##### DRAFT #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------

###### REQUIRES ----------------------------------------------------------------
source(file = "SCRIPT/IBF/generate_plot_distribution.R")


###### NOTES -------------------------------------------------------------------
# Instead of the messy script I originally used for Goe Ansitz and Goe Polter 
# I use the improved one from the IBF+ in the case of Lauenburg. 
# 

####### LAUENBURG ---------------------------------------------------------------
# Offenbar habe ich nur bis hier kurz ein bisschen was angelegt aber noch nicht
# weiter gearbeitet.
parameters <-
  list (
    integer = c(
      plotdistance = 9.93,
      plotangle_gon = 388.5,
      rectangular = 1,
      stretch_x = 0.99,
      stretch_y = 1
    ),
    bool = c(mirror = F)
  )

zero_ref <- "ref14"

# Some plots were measured in the field from trees and are therefore "exact 
# values", the goal is to align the plot map with the tree map. 
reference <- read.csv(
  file = "DATA/RAW/Schotten/Plotverteilung Schotten/Reference points.csv",
  header = T, stringsAsFactors = F)
row.names(reference) <- reference$nr

plot_coord_rel <- generate.rel.coord(
  file = "DATA/RAW/Goe_Lau/Plot Distribution/Lau.csv")
# ggplot(data = plot_coord_rel, aes(x = -x, y = y, label = nr)) +
#   geom_point() +
#   geom_label()

plots_pos_scho <- generate.abs.coord(
  plot_coord_rel = plot_coord_rel, reference = reference, zero_ref = zero_ref,
  parameters = parameters)

plots_ref_scho <- check.reference(plot_coord_abs = plots_pos_scho, 
                             reference = reference)  

###### TIDY UP  ----------------------------------------------------------------
rm(parameters, zero_ref, plot_coord_rel)

###### OUTPUT ------------------------------------------------------------------
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

# 
# 
# 
# 
# # DEFINTION OF THE DISTANCES
# # The naming is a bit misleading but I was not able to find a better one
# plotdistance_y1 <-   cos(plotangle_a) * plotdistance
# plotdistance_x1 <- - sin(plotangle_a) * plotdistance
# plotdistance_y2 <-   cos(plotangle_b) * plotdistance
# plotdistance_x2 <- - sin(plotangle_b) * plotdistance
# 
# 
# 
# # CALCULACTION
# plot_coord_abs$x <- zero_x +  (plot_coord_rel$x - zero_x_rel) *
#   plotdistance_x1
# plot_coord_abs$y <- zero_y +  (plot_coord_rel$x - zero_x_rel) *
#   plotdistance_y1 * 1
# plot_coord_abs$x <- plot_coord_abs$x + (plot_coord_rel$y - zero_y_rel) *
#   plotdistance_x2
# plot_coord_abs$y <- plot_coord_abs$y + (plot_coord_rel$y - zero_y_rel) *
#   plotdistance_y2 * 1
# 
# # No adjustment was needed here
# plot_coord_abs$x <-  plot_coord_abs$x * 1
# plot_coord_abs$y <-  plot_coord_abs$y * 1
# 
# ###### INCLUDE CURRENT PLOT POSITION TO REFERENCE TABLE ------------------------
# 
# nr <- gsub( "ref", "", reference$nr)
# reference <- rbind (reference, plot_coord_abs[ c(nr), c( "nr", "x", "y")]) 
# 
# # CHECK DISTANCE BETWEEN REFERENCE AND PLOT
# check <- reference["ref106",2:3] - plot_coord_abs[ "106", 2:3]
# check <- sqrt( sum(check**2))
