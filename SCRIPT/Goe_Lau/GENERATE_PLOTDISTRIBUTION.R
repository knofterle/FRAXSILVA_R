################ GENERATE PLOT DISTRIBUTION Goe and Lau  #######################
# J.Osewold
# 20.03.2023
##### GOE fertig, Lau braucht noch Daten #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------

###### REQUIRES ----------------------------------------------------------------
source(file = "SCRIPT/IBF/generate_plot_distribution.R")


###### NOTES -------------------------------------------------------------------
# Instead of the messy script I originally used for Goe Ansitz and Goe Polter 
# I use the improved one from the IBF+. 
# 

####### GOE_ANS ---------------------------------------------------------------
# 
parameters <-
	list (
		integer = c(
			plotdistance = 5,
			plotangle_gon = 85,
			rectangular = 1,
			stretch_x = 1,
			stretch_y = 1.01
		),
		bool = c(mirror = F)
	)

zero_ref <- "ref8817"

# Some plots were measured in the field from trees and are therefore "exact 
# values", the goal is to align the plot map with the tree map. 
reference <- read.csv2(
	file = "DATA/RAW/Goe_Lau/Plot Distribution/Reference points Ansitz.csv",
	header = T, stringsAsFactors = F)
row.names(reference) <- reference$nr

plot_coord_rel <- generate.rel.coord(
	file = "DATA/RAW/Goe_Lau/Plot Distribution/Goe_Ansitz.csv")
# ggplot(data = plot_coord_rel, aes(x = -x, y = y, label = nr)) +
#   geom_point() +
#   geom_label()

# Die Plots beginnen nicht mit 1 oder 0, aber ich hatte zum Glück schonmal eine 
# Funktion dafür geschrieben.
plots_pos_goeans <- generate.abs.coord(
	plot_coord_rel = plot_coord_rel, reference = reference, zero_ref = zero_ref,
	parameters = parameters, plotnumber1 = 8701)

plots_ref_goeans <- check.reference(plot_coord_abs = plots_pos_goeans, 
																	reference = reference)  
plots_ref_goeans

####### GOE_POL ---------------------------------------------------------------
# 
parameters <-
	list (
		integer = c(
			plotdistance = 5,
			plotangle_gon = 213,
			rectangular = 1,
			stretch_x = 1,
			stretch_y = .96
		),
		bool = c(mirror = T)
	)

zero_ref <- "ref8913"

# Some plots were measured in the field from trees and are therefore "exact 
# values", the goal is to align the plot map with the tree map. 
reference <- read.csv2(
	file = "DATA/RAW/Goe_Lau/Plot Distribution/Reference points Polter.csv",
	header = T, stringsAsFactors = F)
row.names(reference) <- reference$nr

plot_coord_rel <- generate.rel.coord(
	file = "DATA/RAW/Goe_Lau/Plot Distribution/Goe_Polter.csv")
# ggplot(data = plot_coord_rel, aes(x = -x, y = y, label = nr)) +
#   geom_point() +
#   geom_label()

# Die Plots beginnen nicht mit 1 oder 0, aber ich hatte zum Glück schonmal eine 
# Funktion dafür geschrieben.
plots_pos_goepol <- generate.abs.coord(
	plot_coord_rel = plot_coord_rel, reference = reference, zero_ref = zero_ref,
	parameters = parameters, plotnumber1 = 8901)

plots_ref_goepol <- check.reference(plot_coord_abs = plots_pos_goepol, 
																		reference = reference)  
plots_ref_goepol

####### Lauenburg ------------------------------------------------------------
parameters <-
	list (
		integer = c(
			plotdistance = 4.95,
			plotangle_gon = 391.5,
			rectangular = 1,
			stretch_x = 1,
			stretch_y = 0.99
		),
		bool = c(mirror = T)
	)

zero_ref <- "ref308"

# Some plots were measured in the field from trees and are therefore "exact 
# values", the goal is to align the plot map with the tree map. 
reference <- read.csv(
	file = "DATA/RAW/Goe_Lau/Plot Distribution/Reference_points_Lau.csv",
	header = T, stringsAsFactors = F)
row.names(reference) <- reference$nr

plot_coord_rel <-
	generate.rel.coord(file = "DATA/RAW/Goe_Lau/Plot Distribution/Plotverteilung_Lau.csv")

plots_pos_lau <- generate.abs.coord(
	plot_coord_rel = plot_coord_rel, reference = reference, zero_ref = zero_ref,
	parameters = parameters)

plots_ref_lau <- check.reference(plot_coord_abs = plots_pos_lau, 
																 reference = reference)  



###### TIDY UP  ----------------------------------------------------------------
rm(parameters, zero_ref, plot_coord_rel, reference)

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
