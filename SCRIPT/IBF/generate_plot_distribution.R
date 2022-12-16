############################# generate_plot_distribution #######################
# J.Osewold
# 31.03.2022
##### DRAFT #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------

###### REQUIRES ----------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here

###### NOTES -------------------------------------------------------------------
# First step was to write the layout into a csv, logic: in ROW x the plots reach
# from begin to end, the units are "plotdistances". That was done with excel
# 
# The reference points measured in the field were calculated using excel
# 
# Afterwards a table with the absolute coordinates is calculated. The challenge 
# was to find the perfect settings to align the two maps. Now different parameters
# collected in the list "parameters" can change the fitting. There is:
# plotdistance
# plotangle_gon
# rectangular
# stretch_x
# stretch_y
# mirror
# 
# parameters <-
# 	list (
# 		integer = c(
# 			plotdistance = 9.93,
# 			plotangle_gon = 388.5,
# 			rectangular = 1,
# 			stretch_x = 0.99,
# 			stretch_y = 1
# 		),
# 		bool = c(mirror = F)
# 	)

# This Script only defines functions which are later applied to all trial areas


###### CREATE TABLE WITH RELATIVE COORDINATES ----------------------------------
generate.rel.coord <- function(file) {
  
  plot_coord_csv <- read.csv(file = file, 
                           header = T)
  
  # Empty table for the RELATIVE coordinates
  # Each plot is a row and the distances are still RELATIVE and dimensionless
  # "plotdistances"
  plot_coord_rel <- data.frame(matrix(data = NA, ncol = 3, nrow = 500))
  colnames(plot_coord_rel) <- c("nr", "x", "y")
  
  # This loop writes the relative coordinates for each plot
  line <- 0
  for (i in 1: nrow(plot_coord_csv)) {
    for (k in c(plot_coord_csv$begin[i]:plot_coord_csv$end[i])) {
      line <- line+1
      plot_coord_rel$x[line] <- 1 * k
      plot_coord_rel$y[line] <- 1 * plot_coord_csv$row[i]
    }
  }
  
  #  Remove all empty lines
  plot_coord_rel <- plot_coord_rel[ !is.na(plot_coord_rel$x) , ] 
  
  # Naming of the plots
  plot_coord_rel$nr <- c(1: nrow(plot_coord_rel))
  
  return(plot_coord_rel)
}

###### CREATE TABLE WITH ABSOLUTE COORDINATES ----------------------------------
generate.abs.coord <-
  function(plot_coord_rel, reference, parameters, zero_ref, plotnumber1 = 1)  {
    
  #### PREPROCESS THE PARAMETERS -----------------------------------------------
  
  # MIRROR
  # means to flip the position of the microplots. I just swapped the x 
  # and y values of the !relative! plotpositions before any other alignment is 
  # done.
  #  
  if (parameters$bool ["mirror"] == T) {
    y <- plot_coord_rel$x
    plot_coord_rel$x <- plot_coord_rel$y
    plot_coord_rel$y <- y
  }
  
  # If the plotnumbers do not start at 1 they can be shifted with the option:
  # plotnumber1
  plot_coord_rel$nr <- plot_coord_rel$nr + plotnumber1 - 1
  row.names(plot_coord_rel) <- plot_coord_rel$nr
  	
  	
  # Position of zero (Absolute Plotposition)
  zero_x <- reference[ zero_ref, "x"]
  zero_y <- reference[ zero_ref, "y"]
  # Position of zero (Relative Plotposition)
  zero_x_rel <- plot_coord_rel[ gsub("ref", "", zero_ref), "x"]
  zero_y_rel <- plot_coord_rel[ gsub("ref", "", zero_ref), "y"]
  
  plotdistance <- parameters$integer["plotdistance"] 
  
  plotangle_a <- parameters$integer["plotangle_gon"] / 200 * pi
  plotangle_b <- plotangle_a  + pi/2 * parameters$integer["rectangular"]
  
  # Empty table for the ABSLOLUTE coordinates
  plot_coord_abs <- plot_coord_rel
  plot_coord_abs[,2:3] <- 0
  
  
  #### CALCULACTION ------------------------------------------------------------
  # The naming is a bit misleading but I was not able to find a better one
  # I plan to draw an explanation 
  plotdistance_y1 <-   cos(plotangle_a) * plotdistance
  plotdistance_x1 <- - sin(plotangle_a) * plotdistance
  plotdistance_y2 <-   cos(plotangle_b) * plotdistance
  plotdistance_x2 <- - sin(plotangle_b) * plotdistance
  
  # Any final stretches?
  plotdistance_x2 <-  plotdistance_x2 * parameters$integer["stretch_x"]
  plotdistance_x1 <-  plotdistance_x1 * parameters$integer["stretch_x"]
  plotdistance_y2 <-  plotdistance_y2 * parameters$integer["stretch_y"]
  plotdistance_y1 <-  plotdistance_y1 * parameters$integer["stretch_y"]
  
  # Calculation
  plot_coord_abs$x <- zero_x +  (plot_coord_rel$x - zero_x_rel) *
    plotdistance_x1
  plot_coord_abs$y <- zero_y +  (plot_coord_rel$x - zero_x_rel) *
    plotdistance_y1 * 1
  plot_coord_abs$x <- plot_coord_abs$x + (plot_coord_rel$y - zero_y_rel) *
    plotdistance_x2
  plot_coord_abs$y <- plot_coord_abs$y + (plot_coord_rel$y - zero_y_rel) *
    plotdistance_y2 * 1
  
  return(plot_coord_abs)
}

#### INCLUDE CALCULATED PLOT POSITIONS TO REFERENCE TABLE --------------------
check.reference <- function(plot_coord_abs, reference) {
  reference$ref <- "ref"
  temp_nr <- gsub( "ref", "", reference$nr)
  temp_ref <- plot_coord_abs[ c(temp_nr), c( "nr", "x", "y")]
  temp_ref$ref <- "calc"
  reference <- rbind (reference, temp_ref) 
  
  # Check distances between ref and calculated plots
  reference$distance <- 
   (reference$x[reference$ref == "ref"] - 
     reference$x[reference$ref == "calc"]) **2 + 
       (reference$y[reference$ref == "ref"] - 
         reference$y[reference$ref == "calc"]) **2
  reference$distance <- sqrt(reference$distance)
  reference$distance [reference$ref == "ref"] <- 0
  return (reference)
}
    
 
###### TIDY UP  ----------------------------------------------------------------
rm()

###### OUTPUT ------------------------------------------------------------------
# generate.rel.coord
# generate.abs.coord
# check.reference



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

