#============================ TITLE ===========================================#
# J.Osewold
# 30.03.2023
# STATUS = SAMMLUNG
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here
source(file = "SCRIPT/Goe_Lau/Combine_NV_2021_2022.R")
# Eine ganze Menge Dateien

## LIBRARYS --------------------------------------------------------------------
require(ggplot2)
require(dplyr)
## NOTES -----------------------------------------------------------------------
# Hier werden Funktionen gesammelt, die der Auswertung dienen. 

## GRAPHS ----------------------------------------------------

vergleiche_zuwachs <- function(Plotnumber = 1, nv_data_2021 = nv_2021, nv_data_2022 = nv_2022,
															 color_2021 = "blue", color_2022 = "red", exclude_Saemlinge = F) {
	
	tmp1 <- nv_2021[nv_2021$Plotnummer == Plotnumber, ]
	tmp1 <- tmp1 [order(tmp1$Hoehe),]
	
	# if (exclude_Saemlinge == T) {
	# 	tmp1 <- tmp1 [tmp1$Einjaehriger.Saemling != T, ]
	# 	tmp1 <- tmp1 [!is.na(tmp1$Einjaehriger.Saemling), ]
	# }
	# plot(tmp1$Hoehe)
	# 
	# Ich war so dumm bei den 2021 Daten auch die Sämlinge auszusortieren. Das ist 
	# jetzt auskommentiert. 
	# 
	tmp1$index <- 1:nrow(tmp1)
	
	tmp2 <- nv_2022[nv_2022$Plotnummer == Plotnumber, ]
	tmp2 <- tmp2 [order(tmp2$Hoehe),]
	
	if (exclude_Saemlinge == T) {
		tmp2 <- tmp2 [tmp2$Einjaehriger.Saemling != T, ]
		tmp2 <- tmp2 [!is.na(tmp2$Einjaehriger.Saemling), ]
	}
	# plot(tmp2$Hoehe)
	tmp2$index <- (nrow(tmp1) - nrow(tmp2) + 1 ) : nrow(tmp1)
	
	plot <- ggplot() +
		geom_point(data = tmp1, aes(x = index, y = Hoehe), color = color_2021) +
		geom_point(data = tmp2, aes(x = index, y = Hoehe), color = color_2022)
	
	return(plot)
}

## TIDY UP  --------------------------------------------------------------------
rm()

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


