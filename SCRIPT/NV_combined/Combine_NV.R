#============================ Combine_NV ======================================#
# J.Osewold
# 26.07.22
# NEW AND SHORT
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/Goe_Lau/Combine_NV_2021_2022.R")
source(file = "SCRIPT/IBF/LOAD_NV.R")
# 
# data_nv
# nv 
# 
## LIBRARYS --------------------------------------------------------------------
library(dplyr)

## NOTES -----------------------------------------------------------------------
# Dieses Skript kombiniert erstmal nur die beiden Datensätze, sodass ich das 
# nicht mehrfach machen muss.

## HARMONIZE DATESET  ----------------------------------------------------------
nv_gl <- nv_2022
nv_gl$Hoehe <- nv_gl$Hoehe * 10 # cm to mm

nv_ibf <- data_nv
nv_gl$Versuch <- "Goe_Lau"
nv_ibf$Versuch <- "IBF"
nv_all <- bind_rows(nv_gl, nv_ibf) 
# This is similar to rbind() but all missing columns are filled with NA

# count(nv_all) 24609



## TIDY UP  --------------------------------------------------------------------
rm(data_nv, nv, nv_with_empty, plotnumbers_with_empty)

## OUTPUT ----------------------------------------------------------------------
# nv_all
# nv_gl
# nv_ibf



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

