#============================ structure tree data =============================#
# J.Osewold
# 14.07.22
# MESS
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/IBF/LOAD_DATA_TREE.R")

## LIBRARYS --------------------------------------------------------------------
library(dplyr)
library(sf)
library(sfheaders)
## NOTES -----------------------------------------------------------------------
# Dieses Skript soll ein paar Basisinformationen aus den Baumdaten der IBF holen
# 
# Vermutlich wird jede Fläche einen eigenes Kapitel bekommen


## BB Steglitz  ---------------------------------------------------------------------
str(data_tree_steg)
plot(data_tree_steg$x, data_tree_steg$y, )
table(data_tree_steg$Bemerkung)

data_tree_steg %>% 
#	filter(Bemerkung == "Probebäume") %>% 
#	select(x,y) %>% 
	ggplot(aes(x = x, y = y, color = Bemerkung, label = nr)) +
	geom_point() + 
	geom_label()

# 13 14 15 18 19  exclude
# 46 149 219 266 124 corners a
# 77 113 178 261 123 corners b

borders <- st_sfc(sf::st_polygon(
	list(
		cbind(
		data_tree_steg$x[c(46, 149, 219, 266, 124, 46)],
		data_tree_steg$y[c(46, 149, 219, 266, 124, 46)])
	)
)
)

points <-
	sfheaders::sf_multipoint(sort(data_tree_steg),
													 x = "x",
													 y = "y",
													 multipoint_id = "nr",
													 keep = T)
points <- st_as_sf(data_tree_steg, coords = c("x", "y"), dim = "XY")
class(points)
points
points <- st_combine(points)	
rest <- st_intersection(borders, points)
rest <- st_contains(borders, points)
points[rest[[1]]]
st_join(borders, points)
rest <-  points[borders,] # DAS HIER FUNKTIONIERT; ES IST SOOO DUMMM!!!!!

View(rest)
plot(rest)
class(borders)
class(rest)
str(rest)
rest
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

