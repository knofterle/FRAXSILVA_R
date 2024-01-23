#============================ TITLE ===========================================#
# J.Osewold
# DATE
# STATUS
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here

## LIBRARYS --------------------------------------------------------------------
require(dplyr)
## NOTES -----------------------------------------------------------------------


## FIND  ---------------------------------------------------------------------
B1_1 <- read.csv(file = "DATA/RAW/Schotten/Logger/Bodenfeuchte/Logger1Schatten.csv",
								 skip = 2)
colnames(B1_1) <- c("time", "seconds", "FlachT", "FlachW1", "FlachW2", "FlachW3",
										"TiefT", "TiefW1", "TiefW2", "TiefW3")
B1_1 <- B1_1[-1:-43,]
B1_1$time <- as.POSIXct(B1_1$time, tz = "CET", format = "%Y/%m/%d %I:%M %p")
str(B1_1)

B1_2 <- read.csv(file = "DATA/RAW/Schotten/Logger/Bodenfeuchte/Logger1Schatten_1.csv",
								 skip = 2)
colnames(B1_2) <- c("time", "seconds", "FlachT", "FlachW1", "FlachW2", "FlachW3",
										"TiefT", "TiefW1", "TiefW2", "TiefW3")
B1_2 <- B1_2[-1:-43,]
B1_2$time <- as.POSIXct(B1_2$time, tz = "CET", format = "%Y/%m/%d %I:%M %p")
str(B1_2)

B3_1 <- read.csv(file = "DATA/RAW/Schotten/Logger/Bodenfeuchte/Logger3Halbschat.csv",
								 skip = 2)
colnames(B3_1) <- c("time", "seconds", "FlachT", "FlachW1", "FlachW2", "FlachW3",
										"TiefT", "TiefW1", "TiefW2", "TiefW3")
B3_1 <- B3_1[-1:-43,]
B3_1$time <- as.POSIXct(B3_1$time, tz = "CET", format = "%Y/%m/%d %I:%M %p")
str(B3_1)

B3_2 <- read.csv(file = "DATA/RAW/Schotten/Logger/Bodenfeuchte/Logger3Halbschat_1.csv",
								 skip = 2)
colnames(B3_2) <- c("time", "seconds", "FlachT", "FlachW1", "FlachW2", "FlachW3",
										"TiefT", "TiefW1", "TiefW2", "TiefW3")
B3_2 <- B3_2[-1:-43,]
B3_2$time <- as.POSIXct(B3_2$time, tz = "CET", format = "%Y/%m/%d %I:%M %p")
str(B3_2)

B4_1 <- read.csv(file = "DATA/RAW/Schotten/Logger/Bodenfeuchte/Logger4Halbschat.csv",
								 skip = 2)
colnames(B4_1) <- c("time", "seconds", "FlachT", "FlachW1", "FlachW2", "FlachW3",
										"TiefT", "TiefW1", "TiefW2", "TiefW3")
B4_1 <- B4_1[-1:-43,]
B4_1$time <- as.POSIXct(B4_1$time, tz = "CET", format = "%Y/%m/%d %I:%M %p")
str(B4_1)

B4_2 <- read.csv(file = "DATA/RAW/Schotten/Logger/Bodenfeuchte/Logger4Halbschat_2.csv",
								 skip = 2)
colnames(B4_2) <- c("time", "seconds", "FlachT", "FlachW1", "FlachW2", "FlachW3",
										"TiefT", "TiefW1", "TiefW2", "TiefW3")
B4_2 <- B4_2[-1:-43,]
B4_2$time <- as.POSIXct(B4_2$time, tz = "CET", format = "%Y/%m/%d %I:%M %p")
str(B4_2)

B5_1 <- read.csv(file = "DATA/RAW/Schotten/Logger/Bodenfeuchte/Logger5Freiflae.csv",
								 skip = 2)
colnames(B5_1) <- c("time", "seconds", "FlachT", "FlachW1", "FlachW2", "FlachW3",
										"TiefT", "TiefW1", "TiefW2", "TiefW3")
B5_1 <- B5_1[-1:-43,]
B5_1$time <- as.POSIXct(B5_1$time, tz = "CET", format = "%Y/%m/%d %I:%M %p")
str(B5_1)

B5_2 <- read.csv(file = "DATA/RAW/Schotten/Logger/Bodenfeuchte/Logger5Freiflae_1.csv",
								 skip = 2)
colnames(B5_2) <- c("time", "seconds", "FlachT", "FlachW1", "FlachW2", "FlachW3",
										"TiefT", "TiefW1", "TiefW2", "TiefW3")
B5_2 <- B5_2[-1:-43,]
B5_2$time <- as.POSIXct(B5_2$time, tz = "CET", format = "%Y/%m/%d %I:%M %p")
str(B5_2)

B6_1 <- read.csv(file = "DATA/RAW/Schotten/Logger/Bodenfeuchte/Logger6Freiflae.csv",
								 skip = 2)
colnames(B6_1) <- c("time", "seconds", "FlachT", "FlachW1", "FlachW2", "FlachW3",
										"TiefT", "TiefW1", "TiefW2", "TiefW3")
B6_1 <- B6_1[-1:-43,]
B6_1$time <- as.POSIXct(B6_1$time, tz = "CET", format = "%Y/%m/%d %I:%M %p")
str(B6_1)

B6_2 <- read.csv(file = "DATA/RAW/Schotten/Logger/Bodenfeuchte/Logger6Freiflae_2.csv",
								 skip = 2)
colnames(B6_2) <- c("time", "seconds", "FlachT", "FlachW1", "FlachW2", "FlachW3",
										"TiefT", "TiefW1", "TiefW2", "TiefW3")
B6_2 <- B6_2[-1:-43,]
B6_2$time <- as.POSIXct(B6_2$time, tz = "CET", format = "%Y/%m/%d %I:%M %p")
str(B6_2)


## STEP 2  --------------------------------------------------------------------
B1_1$time[c(1, nrow(B1_1))]
B1_2$time[c(1, nrow(B1_2))]

B3_1$time[c(1, nrow(B3_1))]
B3_2$time[c(1, nrow(B3_2))]
B4_1$time[c(1, nrow(B4_1))]
B4_2$time[c(1, nrow(B4_2))]
B5_1$time[c(1, nrow(B5_1))]
B5_2$time[c(1, nrow(B5_2))]
B6_1$time[c(1, nrow(B6_1))]
B6_2$time[c(1, nrow(B6_2))]





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

















































