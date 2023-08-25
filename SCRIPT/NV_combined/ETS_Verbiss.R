#============================ ETS ~ Verbiss ===================================#
# J.Osewold
# 26.07.22
# BRANDNEW
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/NV_combined/Combine_NV.R")
# nv_all
# 

## LIBRARYS --------------------------------------------------------------------
library(dplyr)
## NOTES -----------------------------------------------------------------------
# Das hier dient dazu den Verbiss auf allen Flaechen ein bisschen besser kennen zu
# lernen. 

## STEP 1  ---------------------------------------------------------------------
str(nv_all)
tmp <- nv_all %>% 
	filter(Baumart_kurz == "GEs")

tmp$Verbiss <-
	apply(subset(x = tmp, select = c(Verbiss.tot, Verbiss.lebend)),
				1,
				sum,
				na.rm = T)

tmp$Verbiss.OI <- tmp$Verbiss != 0

table(tmp$Verbiss.OI, tmp$ETS) 
# 			FALSE  TRUE
# FALSE 12072   767 0.064
# TRUE    697   56
#       0.058			
# Die Kombination aus beidem kommt ziemlich exakt so oft vor wie zu erwarten 
# gewesen wäre. 12072*0.058*0.064 ergibt 45. Und wir liegen bei 56, das ist 
# relativ nah dran. 

table(tmp$ETS)
table(tmp$Verbiss.OI)

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

