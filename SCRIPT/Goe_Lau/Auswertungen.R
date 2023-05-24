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
# source(file = "SCRIPT/Goe_Lau/Combine_NV_2021_2022.R")
# Eine ganze Menge Dateien

## LIBRARYS --------------------------------------------------------------------
require(ggplot2)
require(dplyr)
## NOTES -----------------------------------------------------------------------
# Hier werden Funktionen gesammelt, die der Auswertung dienen. 

## GRAPHS ----------------------------------------------------

# Das hier stellt die Zuwächse für einen konkreten Plot dar, das soll bei der 
# Fehlersuche helfen. 
# Die funktion wird so ähnlich nochmal weiter unten verwendet, nur erheblich
# erweitert, um den zuwachs pro Plot zu berechnen.
# 
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

## CALCULATIONS ----------------------------------------------------------------
# Diese Funktion wird in Combine_2021_2022 mehrmals angewendet, daher schien es 
# sinnvoll das hier unterzubringen statt mehrmals komplett auszuschreiben. 

berechne_zuwachs <-
	function(Baumart = "GEs",
					 nv_daten_2021 = nv_2021,
					 nv_daten_2022 = nv_2022,
					 plot_daten = nv_plots,
					 anteil = 0.3,
					 exclude_saemlinge = T,
					 median = T) {
		
		if (!is.na(Baumart)) {
			nv_daten_2021 <- nv_daten_2021 %>%
				filter(Baumart_kurz == Baumart)
			nv_daten_2022 <- nv_daten_2022 %>%
				filter(Baumart_kurz == Baumart)
			colname <- paste0("Zuwachs", Baumart)
		} else {
			colname <- paste0("Zuwachs")
		}
		
		plotnumbers_temp <-
			intersect(unique(nv_daten_2021$Plotnummer),
								unique(nv_daten_2022$Plotnummer))
		
		for (i in plotnumbers_temp) {
			# Die Daten werden nach Plotnummer gefiltert und nach Hoehe soriert
			tmp1 <- nv_daten_2021[nv_daten_2021$Plotnummer == i,]
			tmp1 <- tmp1 [order(tmp1$Hoehe), ]
			tmp1$index <- 1:nrow(tmp1)
			tmp2 <- nv_daten_2022[nv_daten_2022$Plotnummer == i,]
			tmp2 <- tmp2 [order(tmp2$Hoehe), ]
			
			if (exclude_saemlinge == T) {
				# Einjährige Sämlinge der 2022 Daten werden gelöscht. Und zur Sicherheit auch
				# irgendwelche NAs
				tmp2 <- tmp2 [tmp2$Einjaehriger.Saemling != T,]
				tmp2 <- tmp2 [!is.na(tmp2$Einjaehriger.Saemling),]
			}
			
			# Die jeweils höchsten der beiden Gruppen bekommen nun den selben Index
			tmp2$index <- (nrow(tmp1) - nrow(tmp2) + 1):nrow(tmp1)
			
			# Damit ich auch die anderen Parameter weiterhin verwenden kann werden hier
			# die beiden tabellen komplett gejoint.
			tmp <-
				full_join(
					x = tmp1,
					y = tmp2,
					by  = "index",
					suffix = c("2021", "2022")
				)
			
			tmp$Zuwachs <- tmp$Hoehe2022 - tmp$Hoehe2021
			
			# Die Anzahl der höchsten Bäume (== Anteil) wird durch einen Grenzwert im Index
			# festgelegt. Wenn der Datensatz 2022 mehr Bäume als 2021 hat wird durch
			# diese Berechnung nur die 30% der Anzahl von 2021 verwendet. Aber das ist
			# vermutlich fast immer egal.
			upper <- max(tmp$index) - max(tmp$index) * anteil
			
			tmp <- tmp[tmp$index > upper,]
			
			if (median == T) {
				# An diser Stelle bin ich mir nicht sicher ob es mehr Sinn ergibt einen mean
				# oder einen Median zu verwenden. Der Mean ist nicht sonderlich erfolgreich,
				# daher versuche ich mal median
				plot_daten[[colname]] [plot_daten$Plotnummer == i] <-
					median(tmp$Zuwachs)
			} else {
				plot_daten[[colname]] [plot_daten$Plotnummer == i] <-  mean(tmp$Zuwachs)
			}
		}
		return(plot_daten)
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


