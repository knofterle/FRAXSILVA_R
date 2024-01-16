#============================ PROCESS ===========================================#
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
require(lubridate)
## NOTES -----------------------------------------------------------------------


## STEP 1  ---------------------------------------------------------------------
S <- S[order(S$time),]
S_tmp <- S
S_tmp$time <- round_date(S_tmp$time, "10 mins")
mean <- tibble(time = unique(S_tmp$time), temp =  13)
mean$n <- 0
mean$humid <- 0

for (i in 1:nrow(mean)) {
	# time <- mean$time[i]
	# selection <- which(S$time == time)
	# selection <- which(S$time == mean$time[i])
	# mean$temp[i] <- mean(S$temp[selection])
	mean$temp[i] <- mean(S_tmp$temp[S_tmp$time == mean$time[i]])
	mean$humid[i] <- mean(S_tmp$humid[S_tmp$time == mean$time[i]])
	mean$n[i] <- sum(S_tmp$time == mean$time[i])
	print(i)
}

write.csv(mean, file = "EXPORT/Schotten/mean.csv", fileEncoding = "UTF-8")

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

