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


## GRAPHS ----------------------------------------------------

nv_plots %>% 
	arrange(n_ash_2021) %>% 
	mutate(index = row_number()) %>% 
	ggplot(data = ., aes(x = index)) +
	geom_point(aes(y = n_ash_2021), color = "blue") +
	geom_smooth(aes(y = n_ash_2021), color = "blue") +
	geom_point(aes(y = n_ash_2022), color = "red") +
	geom_smooth(aes(y = n_ash_2022), color = "red")

nv_plots %>% 
	arrange(height_mean_2021) %>% 
	filter(height_mean_2021 < 80) %>% 
	mutate(index = row_number()) %>% 
	ggplot(data = ., aes(x = index)) +
	geom_point(aes(y = height_mean_2021), color = "blue") +
	geom_smooth(aes(y = height_mean_2021), color = "blue") +
	geom_point(aes(y = height_mean_2022), color = "red") +
	geom_smooth(aes(y = height_mean_2022), color = "red")


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


