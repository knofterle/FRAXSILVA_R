############################# load mdb linux sep ###############################
# J.Osewold
# 02.03.2022
##### NEW #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------
library(Hmisc) 

###### REQUIRES ----------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here

###### NOTES -------------------------------------------------------------------
# This Script should be applicable to the processing of all data derived from 
# "WachstumskundeDB". Means it can be used for 1000ETS, Lau/Goe, IBF and 
# something more?
# 
# This Script defines s function that can be used to call any of the separate
# tables in a mdb file.
#
# An other script will define a function that can call all separate ones and 
# merge the data.
# 
# Each function is dependent on a :/PATH and the "NAME" of the table and exports 
# all columns available

###### LOAD SEP  --------------------------------------------------------------
load.mdb.linux.sep <- function(path, name) {
	sep <- mdb.get(file = path, tables = name, 
									stringsAsFactors = F)
	return(sep)
}


###### TIDY UP  ----------------------------------------------------------------
rm()

###### OUTPUT ------------------------------------------------------------------
# load.mdb.linux.sep :/PATH "NAME"



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

