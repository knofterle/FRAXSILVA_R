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

plot.all2 <- function(tree_data = NA, plots_pos, plots_ref, labels = F) {
   
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
  
  if (labels == T) {
  	plot <- plot +
  		geom_label(
  			data = all_points %>%
  				filter(type %in% c("ref_target", "ref_current")),
  			aes(x = x, y = y, label = nr),
  			show.legend = F
  		)
  } else {
  }
  	
  						 
  						 
    
  
  return(plot)
}

###### DRAW TREES AND PLOTS WITH GGPLOT PLUS VALUES FOR PLOTS ------------------
# Notes 
# gedrehte Quadrate für die Verjüngungsplots sind sehr schwer zu erreichen...
# Ich schreibe die Funktion erstmal für Goe und Lau
# 
# Die Bäume im Graphen können mit NA abgeschaltet werden, 
# mit NA bei der Baumfarbe werden die Bäume grau,
# color und size adapt sind jeweils exponenten transformieren also die 
# Verteilung ein bisschen. Size to tree verändet die Einheit die die Plotsize 
# bestimmt aber nur mit multiplikation, das musste sein weil sonst die Werte
# von treesize und plotsize extrem weit auseinander liegen können und daher 
# eins von beiden nicht mehr richtig dargestellt wird.
# generalsize setzt die maximale range der size werte fest. Wird als Vector 
# eingegeben
# Wenn keine plotsize gewünscht ist kann das durch eine konkrete Zahl 
# eingestellt werden.


plot.all3 <-
	function(tree_data,
					 nv_plots,
					 flaeche,
					 plotcolor,
					 plotcoloradapt = 1,
					 plotsize,
					 plotsizeadapt = 1,
					 plotsizetotree = 100,
					 treesize = "d",
					 treecolor = "art",
					 general_size = c(2, 30)) {
		output <- ggplot() +
			scale_size(range = general_size) +
			scale_color_gradient (low = "blue", high = "red") +
			theme_void()
		
		if (identical(nv_plots, NA)) {
			# keine NV_plots gewünscht
		} else  {
			plots <- nv_plots %>%
				filter(location == flaeche)
			plots[[plotcolor]] <- plots[[plotcolor]] ** (1 / plotcoloradapt)
			
			if (is.character(plotsize)) {
				plots[[plotsize]] <- plots[[plotsize]] ** (1 / plotsizeadapt)
				plots[[plotsize]] <- plots[[plotsize]] * plotsizetotree
				
				output <- output +
					geom_point(data = plots,
										 aes(
										 	x = x,
										 	y = y,
										 	color = .data[[plotcolor]],
										 	size = .data[[plotsize]]
										 	# diese .data[[]] waren notwendig weil die Variablen nur als "text"
										 	# transportiert werden und ggplot2 damit nicht umgehen kann. So scheint
										 	# es zu funktionieren und ist auch so empfohlen.
										 ),
										 shape = 15)  # nv_plots
			} else {
				# keine nv_plotsize gewünscht
				output <- output +
					geom_point(
						data = plots,
						aes(
							x = x,
							y = y,
							color = .data[[plotcolor]]
						),
						shape = 15,
						size = plotsize
					)  # nv_plots
				
			}
		}
		if (identical(tree_data, NA)) {
			
		} else  {
			tree <- tree_data # oder halt andere
			tree$art <- as.factor(tree$art)
			
			if (identical(treecolor, NA)) {
				output <- output + geom_point(
					data = tree,
					aes(x = x,
							y = y,
							size = .data[[treesize]]),
					shape = 21,
					alpha = .1,
					fill = "gray"
				) + # Baumkrone
					geom_point(
						data = tree,
						aes(x = x, y = y),
						shape = 21,
						size = 2,
						fill = "gray"
					) # Baumstamm
			} else {
				output <- output +
					geom_point(
						data = tree,
						aes(
							x = x,
							y = y,
							fill = .data[[treecolor]],
							size = .data[[treesize]]
						),
						shape = 21,
						alpha = .1,
					) + # Baumkrone
					geom_point(
						data = tree,
						aes(x = x, y = y, fill = .data[[treecolor]]),
						shape = 21,
						size = 2
					)  # Baumstamm
			}
		}
		output <-
			output + guides(colour = "colorbar",
											fill = "legend",
											size = FALSE)
		output
	}

# BEISPIEL
 # plot.all3(tree_data = tree_lau, nv_plots = nv_plots, flaeche = "lau", 
 # 					plotcolor = "height_median_2022", plotcoloradapt = 4, plotsize = "DSF",
 # 					plotsizeadapt = 1, plotsizetotree = 100, treesize = "d", 
 # 					general_size = c(2,30), treecolor = NA )


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

