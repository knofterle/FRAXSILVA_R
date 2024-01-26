#============================ PROCESS ===========================================#
# J.Osewold
# DATE
# STATUS
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/Schotten/READ_DATA.R")
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here

## LIBRARYS --------------------------------------------------------------------
require(lubridate)
require(dplyr)
require(ggplot2)
## NOTES -----------------------------------------------------------------------


## STEP 1  ---------------------------------------------------------------------
 S <- S[order(S$time),]
 S_tmp <- S
 S_tmp$time <- round_date(S_tmp$time, "10 mins")
 # mean <- tibble(time = unique(S_tmp$time), temp =  13)
 # mean$n <- 0
 # mean$humid <- 0
 # 
 # for (i in 1:nrow(mean)) {
 # 	# time <- mean$time[i]
 # 	# selection <- which(S$time == time)
 # 	# selection <- which(S$time == mean$time[i])
 # 	# mean$temp[i] <- mean(S$temp[selection])
 # 	mean$temp[i] <- mean(S_tmp$temp[S_tmp$time == mean$time[i]])
 # 	mean$humid[i] <- mean(S_tmp$humid[S_tmp$time == mean$time[i]])
 # 	mean$n[i] <- sum(S_tmp$time == mean$time[i])
 # 	print(i)
 # }
 # 
# write.csv(mean, file = "EXPORT/Schotten/mean.csv", fileEncoding = "UTF-8")

## STEP 2  ---------------------------------------------------------------------
mean <- read.csv(file = "EXPORT/Schotten/mean.csv", fileEncoding = "UTF-8")
str(mean)
mean$time <- as.POSIXct(mean$time, tz = "UTC", format = c("%Y-%m-%d %H:%M:%OS"))

S_tmp <- merge(S_tmp, mean, by = "time", all.x = T, all.y = F)

colnames(S_tmp)
colnames(S_tmp) <- c("time", "ID", "temp", "humid", "dewpoint", "file", "device",
										 "ID2", "temp_mean", "n", "humid_mean")
S_tmp <- S_tmp %>% select(ID, time, temp_mean, humid_mean, n, temp, humid, dewpoint, file, device)
S_tmp$diff_temp <- S_tmp$temp - S_tmp$temp_mean
S_tmp$diff_humid <- S_tmp$humid - S_tmp$humid_mean

## STEP 3  ---------------------------------------------------------------------
require(zoo)
S_tmp <- S_tmp[order(S_tmp$device),]
S_tmp$diff_temp_smooth <- rollmean(S_tmp$diff_temp, k = 8, fill = NA)
S_tmp$diff_humid_smooth <- rollmean(S_tmp$diff_humid, k = 8, fill = NA)
S_tmp <- S_tmp[order(S_tmp$time),]

## STEP 4  ---------------------------------------------------------------------


slice <- as.POSIXct("2022-08-11 18:00:00", tz = "UTC")
window <- 3600 * 48
slice <- c(slice - window, slice + window) 

S_tmp %>% 
	filter(time > slice[1] , time < slice[2]) %>% 
	filter(device %in% c("S4", "S5", "S1", "S2")) %>% 
	ggplot(aes(x = time, y = temp_mean)) +
	geom_line() +
	geom_point(aes(x = time, y = diff_temp*2, color = device)) +
	geom_hline(yintercept = 0) +
	scale_x_datetime(date_breaks = "2 hours") +
	scale_y_continuous(sec.axis = sec_axis(~./2), n.breaks = 15) +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

S_tmp %>% 
	filter(time > slice[1] , time < slice[2]) %>% 
	filter(device %in% c("S1", "S2", "S3", "S4", "S5", "S6")) %>% 
	ggplot(aes(x = time, y = temp_mean)) +
	geom_line() +
	geom_point(aes(x = time, y = diff_temp_smooth*2, color = device)) +
	geom_hline(yintercept = 0) +
	scale_x_datetime(date_breaks = "2 hours") +
	scale_y_continuous(sec.axis = sec_axis(~./2), n.breaks = 15) +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

S_tmp %>% 
	filter(time > slice[1] , time < slice[2]) %>% 
	filter(device %in% c("S1", "S2", "S3", "S4", "S5", "S6")) %>% 
	ggplot(aes(x = time, y = humid_mean)) +
	geom_line() +
	geom_point(aes(x = time, y = diff_humid_smooth*2, color = device)) +
	geom_hline(yintercept = 0) +
	scale_x_datetime(date_breaks = "2 hours") +
	scale_y_continuous(sec.axis = sec_axis(~./2), n.breaks = 15) +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

S_tmp %>% 
	filter(time > slice[1] , time < slice[2]) %>% 
	filter(device %in% c("S1", "S3", "S6", "S2")) %>% 
	ggplot(aes(x = time, y = humid_mean)) +
	geom_line() +
	geom_point(aes(x = time, y = diff_humid*2, color = device)) +
	geom_hline(yintercept = 0) +
	scale_x_datetime(date_breaks = "2 hours") +
	scale_y_continuous(sec.axis = sec_axis(~./2), n.breaks = 15) +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))



unique(round_date(S_tmp %>% filter(device == "S2") %>% pull(time), "1 month"))

## STEP 5  ---------------------------------------------------------------------


slice <- as.POSIXct("2022-04-20 18:00:00", tz = "UTC")
window <- 3600 * 48
slice <- c(slice - window, slice + window) 

S_tmp %>% 
	filter(time > slice[1] , time < slice[2]) %>% 
	filter(device %in% c("S4", "S5", "S1", "S2")) %>% 
	ggplot(aes(x = time, y = temp_mean)) +
	geom_line() +
	geom_point(aes(x = time, y = diff_temp*2, color = device)) +
	geom_hline(yintercept = 0) +
	scale_x_datetime(date_breaks = "2 hours") +
	scale_y_continuous(sec.axis = sec_axis(~./2), n.breaks = 15) +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

S_tmp %>% 
	filter(time > slice[1] , time < slice[2]) %>% 
	filter(device %in% c("S1", "S2", "S3", "S4", "S5", "S6")) %>% 
	ggplot(aes(x = time, y = temp_mean)) +
	geom_line() +
	geom_point(aes(x = time, y = diff_temp_smooth*2, color = device)) +
	geom_hline(yintercept = 0) +
	scale_x_datetime(date_breaks = "2 hours") +
	scale_y_continuous(sec.axis = sec_axis(~./2), n.breaks = 15) +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

S_tmp %>% 
	filter(time > slice[1] , time < slice[2]) %>% 
	filter(device %in% c("S1", "S2", "S3", "S4", "S5", "S6")) %>% 
	ggplot(aes(x = time, y = humid_mean)) +
	geom_line() +
	geom_point(aes(x = time, y = diff_humid_smooth*2, color = device)) +
	geom_hline(yintercept = 0) +
	scale_x_datetime(date_breaks = "2 hours") +
	scale_y_continuous(sec.axis = sec_axis(~./2), n.breaks = 15) +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))

S_tmp %>% 
	filter(time > slice[1] , time < slice[2]) %>% 
	filter(device %in% c("S1", "S3", "S6", "S2")) %>% 
	ggplot(aes(x = time, y = humid_mean)) +
	geom_line() +
	geom_point(aes(x = time, y = diff_humid*2, color = device)) +
	geom_hline(yintercept = 0) +
	scale_x_datetime(date_breaks = "2 hours") +
	scale_y_continuous(sec.axis = sec_axis(~./2), n.breaks = 15) +
	theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1))


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
# 






