############################# load_mdb_windows_sep #############################
# J.Osewold
# 30.03.2022
##### Beginning #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------
library(RODBC) 

###### REQUIRES ----------------------------------------------------------------
#

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

###### DEFINE FUNCTION  --------------------------------------------------------
load.mdb.windows.sep <- function (path, name) {
  channel <- odbcConnectAccess(access.file = path)
  sep <- sqlFetch(channel = channel, sqtable = name)
  odbcCloseAll()
  return(sep)
}
  


###### TIDY UP  ----------------------------------------------------------------
rm()

###### OUTPUT ------------------------------------------------------------------
# load.mdb.windows.sep ("/PATH", "name")



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

