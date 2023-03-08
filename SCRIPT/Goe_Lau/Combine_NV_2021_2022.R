#===================== Combine NV 2021 and 2022 ===============================#
# J.Osewold
# 08.03.2023
# NEW
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/Goe_Lau/Aggregated tables NV_2021.R")
source(file = "SCRIPT/Goe_Lau/Aggregated tables NV_2022.R")

# nv_2021 and 2022
# nv_2021_plots and 2022
# nv_2021_with_empty and 2022
 
## LIBRARYS --------------------------------------------------------------------
require(dplyr)

## NOTES -----------------------------------------------------------------------


## CHANGE COLNAMES  ---------------------------------------------------------------------
names(nv_2021_plots) <- paste0(names(nv_2021_plots), "_2021")
names(nv_2022_plots) <- paste0(names(nv_2022_plots), "_2022")
names(nv_2021_plots)[1] <- "Plotnummer"
names(nv_2022_plots)[1] <- "Plotnummer"

nv_plots <-
	inner_join(x = nv_2021_plots,
						 y = nv_2022_plots,
						 by = "Plotnummer")
names(nv_plots)

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

