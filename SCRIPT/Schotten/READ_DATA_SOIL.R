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
source(file = "SCRIPT/Schotten/PROCESS.R")

## LIBRARYS --------------------------------------------------------------------
require(dplyr)
require(ggplot2)
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

## STEP 2  --------------------------------------------------------------------
# Ich verwende nur die Daten vom 5.5.22 bis zum 15.08.22 alle anderen sind
# nicht vollständig (nicht in allen messgeräten vertreten).
B1 <- B1_2
B3 <- B3_2
B4 <- B4_1
B5 <- B5_2
B6 <- B6_1

ggplot(data = B1)+
	geom_point(aes(x = time, y = TiefW1), color = "red") +
	geom_point(aes(x = time, y = TiefW2), color = "blue") +
	geom_point(aes(x = time, y = TiefW3), color = "green")

ggplot(data = B3)+
	geom_point(aes(x = time, y = TiefW1), color = "red") +
	geom_point(aes(x = time, y = TiefW2), color = "blue") +
	geom_point(aes(x = time, y = TiefW3), color = "green")

ggplot(data = B4)+
	geom_point(aes(x = time, y = TiefW1), color = "red") +
	geom_point(aes(x = time, y = TiefW2), color = "blue") +
	geom_point(aes(x = time, y = TiefW3), color = "green")

ggplot(data = B5)+
	geom_point(aes(x = time, y = TiefW1), color = "red") +
	geom_point(aes(x = time, y = TiefW2), color = "blue") +
	geom_point(aes(x = time, y = TiefW3), color = "green")

ggplot(data = B6)+
	geom_point(aes(x = time, y = TiefW1), color = "red") +
	geom_point(aes(x = time, y = TiefW2), color = "blue") +
	geom_point(aes(x = time, y = TiefW3), color = "green")

B1$device <- "B1"
B3$device <- "B3"
B4$device <- "B4"
B5$device <- "B5"
B6$device <- "B6"

## STEP 3  --------------------------------------------------------------------

B <- tibble(
	time = c(B1$time, B3$time, B4$time, B5$time, B6$time),
	TiefW = c(B1$TiefW3, B3$TiefW1, B4$TiefW1, B5$TiefW3, B6$TiefW1),
	device = c(B1$device, B3$device, B4$device, B5$device, B6$device)
)

ggplot(data = B, aes(x = time, y = TiefW, color = device)) +
	geom_point()

## STEP 4  --------------------------------------------------------------------

slice <- as.POSIXct("2022-06-15 18:00:00", tz = "UTC")
window <- 3600 * 24*40
slice <- c(slice - window, slice + window) 
S_tmp2 <- S_tmp %>% 	
	filter(time > slice[1] , time < slice[2]) %>% 
	filter(device %in% c("S1", "S3", "S6", "S2"))

require(zoo)
S_tmp2$humid_mean <- rollmax(S_tmp2$humid_mean, k = 144, fill = NA)

S_tmp2 %>% 
	ggplot(aes(x = time, y = humid_mean)) +
	geom_point() +
#	geom_point(aes(x = time, y = diff_humid*2, color = device)) +
	geom_hline(yintercept = 0) +
	# scale_x_datetime(date_breaks = "2 hours") +
	scale_y_continuous(sec.axis = sec_axis(~./2), n.breaks = 15) +
	# theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	geom_point(data = B, aes(x = time, y = TiefW/3, color = device))

## STEP 5  --------------------------------------------------------------------
# Ich verwende nur die Daten vom 5.5.22 bis zum 15.08.22 alle anderen sind
# nicht vollständig (nicht in allen messgeräten vertreten).

ggplot(data = B1)+
	geom_point(aes(x = time, y = FlachW1), color = "red") +
	geom_point(aes(x = time, y = FlachW2), color = "blue") +
	geom_point(aes(x = time, y = FlachW3), color = "green")

ggplot(data = B3)+
	geom_point(aes(x = time, y = FlachW1), color = "red") +
	geom_point(aes(x = time, y = FlachW2), color = "blue") +
	geom_point(aes(x = time, y = FlachW3), color = "green")

ggplot(data = B4)+
	geom_point(aes(x = time, y = FlachW1), color = "red") +
	geom_point(aes(x = time, y = FlachW2), color = "blue") +
	geom_point(aes(x = time, y = FlachW3), color = "green")

ggplot(data = B5)+
	geom_point(aes(x = time, y = FlachW1), color = "red") +
	geom_point(aes(x = time, y = FlachW2), color = "blue") +
	geom_point(aes(x = time, y = FlachW3), color = "green")

ggplot(data = B6)+
	geom_point(aes(x = time, y = FlachW1), color = "red") +
	geom_point(aes(x = time, y = FlachW2), color = "blue") +
	geom_point(aes(x = time, y = FlachW3), color = "green")

B$FlachW <- c(B1$FlachW3, B3$FlachW2, B4$FlachW3, B5$FlachW3, B6$FlachW3)

S_tmp2 %>% 
	ggplot(aes(x = time, y = humid_mean)) +
	geom_point() +
	#	geom_point(aes(x = time, y = diff_humid*2, color = device)) +
	geom_hline(yintercept = 0) +
	# scale_x_datetime(date_breaks = "2 hours") +
	scale_y_continuous(sec.axis = sec_axis(~./2), n.breaks = 15) +
	# theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	geom_point(data = B, aes(x = time, y = FlachW/3, color = device))

S_tmp2 %>% 
	ggplot(aes(x = time, y = temp_mean)) +
	geom_point() +
	#	geom_point(aes(x = time, y = diff_humid*2, color = device)) +
	geom_hline(yintercept = 0) +
	# scale_x_datetime(date_breaks = "2 hours") +
	scale_y_continuous(sec.axis = sec_axis(~./2), n.breaks = 15) +
	# theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
	geom_point(data = B, aes(x = time, y = TiefW/10, color = device))


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

















































