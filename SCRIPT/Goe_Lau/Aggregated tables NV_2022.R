#============================ Aggregated tables 2022 ==========================#
# J.Osewold
# 08.03.2023
# Copied and adapted
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/Goe_Lau/Load and Clean NV 2022.R", echo = T, encoding = "UTF-8")
# plotnumbers_with_empty_2022
# nv_2022
# nv_2022_with_empty

## NOTES -----------------------------------------------------------------------

## EXPORT NV  ---------------------------------------------
write.csv(x = nv_2022, file = "EXPORT/Goe_Lau/tables/NV_2022.csv", 
					fileEncoding = "UTF-8")

## PRODUCE AGGREGATED PLOTS TABLE  ---------------------------------------------
nv_2022_plots <- data.frame("Plotnummer" = plotnumbers_with_empty_2022)

for (i in 1:nrow(nv_2022_plots)) {
	temp_plotnummer <- nv_2022_plots$Plotnummer[i]
	temp_rows <- (nv_2022$Plotnummer == temp_plotnummer)
	temp_rows2 <- (nv_2022_with_empty$Plotnummer == temp_plotnummer)
	
	nv_2022_plots$n_trees[i] <- sum(temp_rows, na.rm = T)
	nv_2022_plots$n_species[i] <- length(unique(nv_2022$Baumart_kurz[temp_rows]))
	nv_2022_plots$n_ash[i] <- sum(nv_2022$Baumart_kurz[temp_rows] == "GEs", na.rm = T)
	
	# # The location can be taken from the plotnumber, each is unique
	# if (temp_plotnummer %in% 1:315) {
	#   nv_2022_plots$location[i] <- "Lau_Steinhorst"
	# } else if (temp_plotnummer %in% 8701:8815) {
	#   nv_2022_plots$location[i] <- "Goe_Ansitz"
	# } else if (temp_plotnummer %in% 8900:9017) {
	#   nv_2022_plots$location[i] <- "Goe_Polter"
	# } 
	# Obsolet weil ich die Flaeche bereits früher eingetragen habe. Aber es 
	# funktiniert auf die alte Weise besser, weil die leeren Plots ja bereits 
	# entfernt wurden
	nv_2022_plots$location[i] <- nv_2022_with_empty$Flaeche[temp_rows2] [1]
	
	# The empty plots are missing in the nv_2022 table, but instead of NA, max() etc
	# returns inf+ or NaN, therefore a check for emptiness had to be implemented
	if (length(nv_2022$Hoehe[temp_rows]) != 0) {
		nv_2022_plots$height_mean[i] <- mean(nv_2022$Hoehe[temp_rows])
		nv_2022_plots$height_median[i] <- median(nv_2022$Hoehe[temp_rows])
		nv_2022_plots$height_max[i] <- max(nv_2022$Hoehe[temp_rows])
		nv_2022_plots$height_min[i] <- min(nv_2022$Hoehe[temp_rows])
	} else {
		nv_2022_plots$height_mean[i] <- NA
		nv_2022_plots$height_median[i] <- NA
		nv_2022_plots$height_max[i] <- NA
		nv_2022_plots$height_min[i] <- NA
	}
	
	# How many ashes do either have nekroses OR (|) have just died?
	temp_ets_new <-
		nv_2022$ETS.abgestorben.frisch[temp_rows] != 0 |
		nv_2022$ETS.lebend[temp_rows] != 0
	temp_ets_total <-
		nv_2022$ETS[temp_rows]
	
	nv_2022_plots$n_ets_new[i] <- sum(temp_ets_new, na.rm = T)
	nv_2022_plots$n_ets_old[i] <-
		sum(nv_2022$ETS.abgestorben.alt[temp_rows] != 0, na.rm = T)
	nv_2022_plots$n_ets_total[i] <- sum(temp_ets_total, na.rm = T)
	
	# In most cases the first comment is related to the whole plot
	nv_2022_plots$comment[i] <- (nv_2022_with_empty$Bemerkungen[temp_rows2])[1]
	
	# Die Daten zu Zaun oder Ausschluss werden erst später aus der 2021 Tabelle 
	# uebertragen
	# nv_2022_plots$Zaun[i] <- nv_2022_with_empty$Zaun[temp_rows2] [1]
	# nv_2022_plots$Ausschluss[i] <-
	# 	nv_2022_with_empty$Ausgeschlossen.Rand.Zaun[temp_rows2] [1]
	
	# Die ausgeschlossenen Plots hatten natürlich eine unbekannte Anzahl von 
	# Baeumen und Baumarten. Aber auch das folgt erst spaeter wenn die beiden 
	# Tabellen kombiniert werden.
	# if (nv_2022_plots$Ausschluss[i] == T) {
	# 	nv_2022_plots$n_trees [i] <- NA
	# 	nv_2022_plots$n_species [i] <- NA
	# 	nv_2022_plots$n_ash [i] <- NA
	# 	nv_2022_plots$n_ets_new [i] <- NA
	# 	nv_2022_plots$n_ets_old [i] <- NA
	# 	nv_2022_plots$n_ets_total [i] <- NA
	# }
}

# Now the information about logging trails from the empty plots is needed too
# 
for (i in 1:nrow(nv_2022_plots)) {
	temp_plotnummer <- nv_2022_plots$Plotnummer[i]
	nv_2022_plots$Rueckegasse [i] <-
		unique(nv_2022_with_empty$Rueckegasse[nv_2022_with_empty$Plotnummer 
																					== temp_plotnummer])
}

nv_2022_plots$height_mean <- round(nv_2022_plots$height_mean, 2) 
write.csv(x = nv_2022_plots, file = "EXPORT/Goe_Lau/tables/NV_2022_Plots.csv",
					fileEncoding = "UTF-8")

## PRODUCE AGGREGATED SPECIES TABLE  -------------------------------------------

nv_2022_species <- data.frame("species" = unique(nv_2022$Baumart_kurz))
for (i in 1:nrow(nv_2022_species)) {
	temp_species <- nv_2022_species$species[i]
	nv_2022_species$n[i] <- length(nv_2022$Baumart_kurz[nv_2022$Baumart_kurz == temp_species])
	nv_2022_species$n_plots[i] <-
		length(unique(nv_2022$Plotnummer[nv_2022$Baumart_kurz == temp_species]))
	nv_2022_species$height_mean[i] <- mean(nv_2022$Hoehe[nv_2022$Baumart_kurz == temp_species])
	nv_2022_species$height_median[i] <- median(nv_2022$Hoehe[nv_2022$Baumart_kurz == temp_species])
	nv_2022_species$height_max[i] <- max(nv_2022$Hoehe[nv_2022$Baumart_kurz == temp_species])
	nv_2022_species$height_min[i] <- min(nv_2022$Hoehe[nv_2022$Baumart_kurz == temp_species])
}
nv_2022_species$height_mean <- round(nv_2022_species$height_mean, 2)
nv_2022_species <- nv_2022_species[order(nv_2022_species$n, decreasing = T), ]



## TIDY UP  --------------------------------------------------------------------
rm(i, temp_ets_new, temp_ets_total, temp_plotnummer,
	 temp_rows, temp_species, temp_rows2)

## OUTPUT ----------------------------------------------------------------------
# nv_2022
# nv_2022_plots
# nv_2022_species
# nv_2022_with_empty
# plotnumbers_with_empty_2022



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

