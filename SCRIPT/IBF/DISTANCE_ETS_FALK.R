#==================== DISTANCE ASH _ ETS FALK =================================#
# J.Osewold
# 03.07.2023
# DRAFT
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here
source(file = "SCRIPT/IBF/AGGREGATED TABLES NV.R", encoding = "UTF-8")


## LIBRARYS --------------------------------------------------------------------
require(dplyr)
require(ggplot2)
## NOTES -----------------------------------------------------------------------
# Das Skript habe ich schnell und unsauber geschrieben um zu testen ob mein 
# Ansatz für Falks Fragestellung funktionieren kann und ob etwas heraus kommt.
# 
# Ich habe in den For Schleifen verschiedene Formeln getestet und die alten 
# teilweise im Code belassen, alles in allem ziemlich messy
# 
# Weil die Formel ja auch mehr oder weniger für eine Art Überschirmung genutzt 
# werden kann, habe ich das ganze auch auf alle Baumarten angewendet.


## INDEX ERSTELLEN  ---------------------------------------------------------------------
# Diese Zeilen waren nur für Falk wenn er die Daten einlesen muss
# data_nv_plots <- read.csv(file = "EXPORT/IBF/tables/data_nv_plots_with_coord.csv",
# 													fileEncoding = "UTF-8")
# data_tree_ibf <- read.csv(file = "DATA/RAW/ibf_alltrees_final_10-2022.csv")


tmp <- data_nv_plots %>% 
	mutate(ets_ratio = n_ets_total / n_ash)

tmp_tree <- data_tree_ibf %>% 
	mutate(flaeche = recode(ibf_id,
													"BB_1" = "Stegelitz",
													"BW_1" = "Plattenwald",
													"BW_2" = "Weisweil",
													"BY_1" = "gsfnbukj",
													"BY_2" = "hsfjk",
													"BY_3" = "sfhunkl",
													"HE_1" = "Schotten",
													"MV_1" = "Greifswald",
													"NI_1" = "Mollenfelde",
													"SN_1" = "Leutzsch",
													"SN_2" = "ghudsef",
													"ST_1" = "Huy",
													"TH_1" = "Ettersberg"
	))
### INDEX ESCHE  ---------------------------------------------------------------------

for (i in 1:nrow(tmp)) {
	area <- tmp$Flaeche[i]
	ash_trees <- tmp_tree %>% 
		filter(flaeche == area) %>% 
		filter(id_baumart == 120)
	ash_trees$dist_x <- ash_trees$x_utm32 - tmp$x[i]
	ash_trees$dist_y <- ash_trees$y_utm32 - tmp$y[i]
	ash_trees$dist <- sqrt(ash_trees$dist_x^2 + ash_trees$dist_y^2)
	ash_trees <- ash_trees %>% 
		arrange(dist)
	nearest <- ash_trees [1:10,]
	nearest <- nearest %>% 
		mutate(value1  = 1/(dist / BHD_CM)) %>% 
		mutate(value2  = dist) %>% 
		mutate(value3 = 1/(dist / BHD_CM))
	value1 <- sum(nearest$value1)
	value2 <- sum(nearest$value2[1:5])
	value3 <- sum(nearest$value3[1:3])
	tmp$value1[i] <- value1
	tmp$value2[i] <- value2
	tmp$value3[i] <- value3
}

### INDEX ALLE BAUMARTEN  ---------------------------------------------------------------------

for (i in 1:nrow(tmp)) {
	area <- tmp$Flaeche[i]
	ash_trees <- tmp_tree %>% 
		filter(flaeche == area) 
	#filter(id_baumart == 120)
	ash_trees$dist_x <- ash_trees$x_utm32 - tmp$x[i]
	ash_trees$dist_y <- ash_trees$y_utm32 - tmp$y[i]
	ash_trees$dist <- sqrt(ash_trees$dist_x^2 + ash_trees$dist_y^2)
	ash_trees <- ash_trees %>% 
		arrange(dist)
	nearest <- ash_trees [1:5,]
	nearest <- nearest %>% 
		mutate(value1  = 1/(dist / BHD_CM)) %>% 
		mutate(value2  = dist) %>% 
		mutate(value3 = 1/(dist / BHD_CM))
	value1 <- sum(nearest$value1)
	value2 <- sum(nearest$value2[1:5])
	value3 <- sum(nearest$value3[1:3])
	tmp$light_value[i] <- value1
	tmp$light_value2[i] <- value2
	tmp$light_value3[i] <- value3
}

### DARSTELLUNG  ---------------------------------------------------------------------

tmp2 <- tmp %>% 
	filter(n_ash != 0) %>% 
	filter(ets_ratio <= .5) %>% 
	filter(value >= 0.01) %>%
	filter(value <= 100) %>% 
	filter(ets_ratio != 0) 
str(tmp2)
ggplot(data = tmp2) +
	geom_point(aes(x = value, y = ets_ratio, size = n_ash, alpha = n_ash))

tmp2 <- tmp %>% 
	filter(n_ash != 0) %>% 
	filter(ets_ratio <= .5) %>% 
	# filter(value >= 0.01) %>%
	filter(value2 <= 500) %>% 
	filter(ets_ratio != 0) 
str(tmp2)
ggplot(data = tmp2) +
	geom_point(aes(x = value2, y = ets_ratio, size = n_ash, alpha = n_ash))

tmp2 <- tmp %>% 
	filter(n_ash != 0) %>% 
	filter(ets_ratio <= .5) %>% 
	filter(value3 >= 0.01) %>%
	filter(value3 <= 30) %>% 
	filter(ets_ratio != 0) 
str(tmp2)
ggplot(data = tmp2, aes(x = value3, y = ets_ratio, size = n_ash, alpha = n_ash)) +
	geom_point() 
#smooth()

tmp2 <- tmp %>% 
	filter(light_value >= 0.1, light_value <= 100)
ggplot(data = tmp2, aes(x = sqrt(light_value), y = n_trees)) +
	geom_point() +
	geom_smooth()


tmp2 <- tmp %>% 
	#	filter(n_ash != 0) %>% 
	#	filter(ets_ratio <= .5) %>% 
	filter(value1 >= 0.01) %>%
	filter(value1 <= 75) #%>% 
#	filter(ets_ratio != 0) 
str(tmp2)
ggplot(data = tmp2, aes(x = value1, y = n_ash)) +
	geom_point() +
	geom_smooth()
#smooth()



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






# data_nv_plots <- read.csv(file = "EXPORT/IBF/tables/data_nv_plots_with_coord.csv",
# 													fileEncoding = "UTF-8")
# data_tree_ibf <- read.csv(file = "DATA/RAW/ibf_alltrees_final_10-2022.csv")
# 


