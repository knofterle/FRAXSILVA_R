############################# generate_plotmap #################################
# J.Osewold
# 11.04.2022
##### DRAFT #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------
library(ggplot2)

###### REQUIRES ----------------------------------------------------------------
# tree_data with the following columns: $nr $x $y 
# plots_pos with the following columns: $nr $x $y  
# plots_ref with the following columns: $nr $x $y  

###### NOTES -------------------------------------------------------------------


###### DRAW TREES AND PLOTS WITH BASE R ----------------------------------------

plot.all1 <- function(tree_data = NA, plots_pos, plots_ref) {
	
	### MERGE ALL DATA -----------------------------------------------------------
	if (is.na(tree_data)) {
		all_points <- data.frame(
			nr = c(coord_an_abs$nr, ref_an_check$nr),
			x = c(coord_an_abs$x, ref_an_check$x),
			y = c(coord_an_abs$y, ref_an_check$y),
			type = c(
				rep("plot", nrow(coord_an_abs)),
				rep("ref_target", nrow(ref_an_check) / 2),
				rep("ref_current", nrow(ref_an_check) / 2)
			)
		)
	} else	{
		all_points <- data.frame(
			nr = c(tree_data$nr, plots_pos$nr, plots_ref$nr),
			x = c(tree_data$x, plots_pos$x, plots_ref$x),
			y = c(tree_data$y, plots_pos$y, plots_ref$y),
			type = c(
				rep("tree", nrow(tree_data)),
				rep("plot", nrow(plots_pos)),
				rep("ref_target", nrow(plots_ref) / 2),
				rep("ref_current", nrow(plots_ref) / 2)
			)
		)
	}
	
  ### SET COLOURS --------------------------------------------------------------
  color <- all_points$type
  color[color == "tree"] <- "gray"
  color[color == "plot"] <- "black"
  color[color == "ref_target"] <- "red"
  color[color == "ref_current"] <- "green"

  ### PLOT ---------------------------------------------------------------------
  plot <- plot(all_points$x, all_points$y, col = color
     #, xlim = c(0,70), ylim = c(0,80)
    )
  return(plot)
}

###### DRAW TREES AND PLOTS WITH GGPLOT  ---------------------------------------

plot.all2 <- function(tree_data = NA, plots_pos, plots_ref) {
  
	### MERGE ALL DATA -----------------------------------------------------------
	if (is.na(tree_data)) {
		all_points <- data.frame(
			nr = c(coord_an_abs$nr, ref_an_check$nr),
			x = c(coord_an_abs$x, ref_an_check$x),
			y = c(coord_an_abs$y, ref_an_check$y),
			type = c(
				rep("plot", nrow(coord_an_abs)),
				rep("ref_target", nrow(ref_an_check) / 2),
				rep("ref_current", nrow(ref_an_check) / 2)
			)
		)
	} else	{
		all_points <- data.frame(
			nr = c(tree_data$nr, plots_pos$nr, plots_ref$nr),
			x = c(tree_data$x, plots_pos$x, plots_ref$x),
			y = c(tree_data$y, plots_pos$y, plots_ref$y),
			type = c(
				rep("tree", nrow(tree_data)),
				rep("plot", nrow(plots_pos)),
				rep("ref_target", nrow(plots_ref) / 2),
				rep("ref_current", nrow(plots_ref) / 2)
			)
		)
	}
	
  ### SET COLOURS --------------------------------------------------------------
  color <- c("tree" = "gray", "plot" = "black" , "ref_target" = "red", 
              "ref_current" = "green")
  
  ### PLOT ---------------------------------------------------------------------
  plot <-
    ggplot (all_points, aes(x = x, y = y, color = type)) +
    geom_point (shape = 1, size = 2) +
    scale_color_manual (values = color) +
    coord_fixed (ratio = 1)
    
  
  return(plot)
}

  

###### TIDY UP  ----------------------------------------------------------------
rm()

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

