################ GENERATE PLOT DISTRIBUTION TABLE POLTER #######################
# J.Osewold
# 21.10.2021
##### DRAFT #####
################################################################################

###### NOTES -------------------------------------------------------------------
# First step was to write the layout into a csv, logic: in ROW x the plots reach
# from begin to end, the units are "plotdistances". That was done with excel
# 
# The reference points measured in the field were calculated using excel
# 
# Afterwards a table with the absolute coordinates is calculated. The challenge 
# was to find the perfect settings to align the two maps. It was done by 
# manipulating the two angles (its not exactly 90° between x and y anymore)
# 
###### LIBRARY -----------------------------------------------------------------

###### REQUIRES ----------------------------------------------------------------

###### LOAD DATABASE -----------------------------------------------------------
plot_coord_csv <- read.csv("DATA/RAW/Goe_Lau/Plot Distribution/Goe_Polter.csv", header = T)

###### CREATE TABLE WITH RELATIVE COORDINATES ----------------------------------

# Each plot is a row and the distances are still RELATIVE and dimensionless
# "plotdistances"
plot_coord_rel <- data.frame(matrix(data = NA, ncol = 3, nrow = 117))
colnames(plot_coord_rel) <- c("nr", "x", "y")
plot_coord_rel$nr <- c(8901:9017)
row.names(plot_coord_rel) <- plot_coord_rel$nr

# Empty table for the ABSLOLUTE coordinates
plot_coord_abs <- plot_coord_rel
plot_coord_abs[,2:3] <- 0
row.names(plot_coord_abs) <- plot_coord_abs$nr

# This loop writes the relative coordinates for each plot
line <- 0
for (i in 1: nrow(plot_coord_csv)) {
  for (k in c(plot_coord_csv$begin[i]:plot_coord_csv$end[i])) {
    line <- line+1
    plot_coord_rel$x[line] <- -1 * k
    plot_coord_rel$y[line] <- -1 * plot_coord_csv$row[i]
  }
}

# the relative coordinates are shifted to put the reference plot (8806) at 0|0  
plot_coord_rel$x <- plot_coord_rel$x + 1

###### CREATE TABLE WITH ABSOLUTE COORDINATES ----------------------------------

# Four plots were measured in the field from trees and are therefore "exact 
# values", the goal is to align the plot map with the tree map. 
reference <- read.csv2(
  file = "DATA/RAW/Goe_Lau/Plot Distribution/Reference points Polter.csv",
  header = T, stringsAsFactors = F)
row.names(reference) <- reference$nr

zero_x <- reference$x [reference$nr == "ref8906"]
zero_y <- reference$y [reference$nr == "ref8906"]

####### ADJUST THESE VALUES ------------------------------------------------------
plotdistance <- -5 
plotangle_a <- 80 * pi/180
plotangle_b <- plotangle_a * 0.98 + pi/2
#######

# DEFINTION OF THE DISTANCES
# The naming is a bit misleading but I was not able to find a better one
plotdistance_y1 <-   cos(plotangle_a) * plotdistance
plotdistance_x1 <-   sin(plotangle_a) * plotdistance
plotdistance_y2 <-   cos(plotangle_b) * plotdistance
plotdistance_x2 <-   sin(plotangle_b) * plotdistance

# CALCULACTION
plot_coord_abs$x <- zero_x + plot_coord_abs$x + 
  plot_coord_rel$x * plotdistance_x1
plot_coord_abs$y <- zero_y + plot_coord_abs$y + 
  plot_coord_rel$x * plotdistance_y1 * 1
plot_coord_abs$x <- plot_coord_abs$x + plot_coord_rel$y * plotdistance_x2
plot_coord_abs$y <- plot_coord_abs$y + plot_coord_rel$y * plotdistance_y2 * 1

# No adjustment was needed here
plot_coord_abs$x <-  plot_coord_abs$x * 1
plot_coord_abs$y <-  plot_coord_abs$y * 1

# CHECK DISTANCE BETWEEN REFERENCE AND PLOT
check <- reference["ref8913",2:3] - plot_coord_abs[ "8913", 2:3]
sqrt( sum(check**2))

###### TIDY UP -----------------------------------------------------------------
nvplots_polter <- plot_coord_abs
ref_polter <- reference
rm(i, k, line, plotdistance, plotdistance_x1, plotdistance_x2, plotdistance_y1,
   plotdistance_y2, plotangle_a, plotangle_b, plot_coord_csv, 
   plot_coord_rel, check, plot_coord_abs, reference)

###### OUTPUT ------------------------------------------------------------------
# stammv_polter, baum_polter, stammv_ansitz, baum_ansitz
# nvplots_polter, ref_polter
