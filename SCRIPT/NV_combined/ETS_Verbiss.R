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


## STEP 1  ---------------------------------------------------------------------
str(nv_all)

nv_all$Verbiss <-
	apply(subset(x = nv_all, select = c(Verbiss.tot, Verbiss.lebend)),
				1,
				sum,
				na.rm = T)

nv_all$Verbiss.OI <- nv_all$Verbiss != 0

table(nv_all$Verbiss.OI, nv_all$ETS) 
# 			FALSE  TRUE
# FALSE 22920   758 0.039
# TRUE    788   143
#       0.038				
# Die Kombination aus beiden kommt auffaellig haeufig vor, etwa 4 mal so haeufig
# theoretisch wäre etwa 35 zu erwarten. Aber naja was heißt das schon.

table(nv_all$ETS)
table(nv_all$Verbiss.)

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

