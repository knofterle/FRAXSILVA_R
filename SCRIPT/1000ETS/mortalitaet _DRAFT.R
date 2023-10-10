

head(mort_status)
head(data_1000)

data_1000 <- data_1000[3,]
ETSStufen_1000 <- ETSStufen_1000 [3,]
ETSStufen_1000 [53,]
Tote_1000 <- Tote_1000 [3,]
Tote_1000 [53,]

count_48 <- 0

count_18 <- 0

count_9_ <- 0

count_NA <- 0

count_52 <- 0

count_live <- 0

source(file = "SCRIPT/1000ETS/load_data_1000.R")

ETSStufen_1000 


# Die Position in pos ist folgendermaßen codiert: 1 = 2013, 10 = 2022
# 2023 wurden keine Daten mehr aufgenommen. 2013 gibt es zwar 5er aber keine 
# Spalte tot_2013. D.h. alle Eschen die 2013 mit 5 gefunden wurden sind maximal das
# Jahr danach gestorben. Aber auch das häufig nicht. Die Datenaufnahme war beschissen.
# 
# 
# Die count_XX wurden eingerichtet um einen Überblick zu bekommen wie oft welcher 
# Fall vorkommt.

########################

mort_status <- data.frame(matrix(nrow = nrow(data_1000), ncol = 6))
colnames(mort_status) <- c("edvid", "bnr", "age2013", "age", 
													 "observ_time", "status")
error <- c()

for (row in 1:nrow(data_1000)) {
	ets <- ETSStufen_1000[row,]
	dead <- Tote_1000 [row,]
	pos <- NA
	lock <- 0
	if (8 %in% ets) { 
		pos <- first(which(ets == 8))
		if (ets[pos-1] %in% c(4,5)) { # 3 2 1 4 8 NA NA             ## HIER
			mort_status$status[row] <- 1
			lock <- lock +1
			#############
			count_48 <- count_48 +1
			#############
		}
		if (ets[pos-1] %in% c(1:3,17)) { # 3 1 2 1 8 NA NA           ## HIER
			mort_status$status[row] <- 0
			lock <- lock +1
			#############
			count_18 <- count_18 + 1
			#############
		}
	} 
	if (9 %in% ets) { # 3 1 3 9 9 9 NA NA 
		pos <- first(which(ets == 9))
		mort_status$status[row] <- 0
		lock <- lock +1
		#############
		count_9_ <- count_9_ + 1
		#############
	} 
	if (2 %in% dead) { # 3 1 3 5 5.2 NA 
		pos <- first(which(dead == 2)) + 1
		mort_status$status[row] <- 1
		lock <- lock +1
		#############
		count_52 <- count_52 + 1
		#############
	} 
	# Hier werden alle gefiltert, die irgendwann NAs enthalten, wenn davor keine 
	# 5 oder 8 enthalten ist, wurde der Baum in dieser Zeit nicht mehr beobachtet
	# und wird entsprechend des Alters zu der Zeit zensiert. 
	# Alle anderen die keine NAs enthalten und vorher noch keinen status bekommen 
	# haben entsprechend dort auch nur NA enthalten muessen folglich im letzten 
	# Jahr immer noch leben. 
	# 
	# NA zwischendurch gibt es nicht, die wurden manuell alle durch 17 ersetzt, 
	# glaube ich
	if (any(is.na(ets))) {
		if (ets[first(which(is.na(ets)))-1] %in% c(5,8)) { # 3 1 3 2 5.2 NA
			#############
			# Suche die erste Stelle die NA hat und schau in die Position davor, ist 
			# der Inhalt gleich 5 oder 8? -> Do nothing
			#############
		} else { # 3 1 2 NA NA NA 
			pos <- first(which(is.na(ets)))
			mort_status$status[row] <- 0
			lock <- lock +1
			#############
			count_NA <- count_NA + 1
			#############
		}
	} else { # 3 1 2 4 1 3 2 
		if (is.na(mort_status$status[row])) {
			pos <- 10 # fuer das jahr 2022        #################
			mort_status$status[row] <- 0
			lock <- lock +1
			#############
			count_live <- count_live + 1
			#############
		}
	}
	
	mort_status$edvid[row] <- data_1000$edv_id[row]
	mort_status$bnr[row]   <- data_1000$bnr[row]
	mort_status$baum_id[row]   <- data_1000$baum_id[row]
	mort_status$age[row]   <- data_1000$alt_ets13[row] + pos -1
	mort_status$age2013 [row] <- data_1000$alt_ets13 [row]
	mort_status$observ_time[row] <- pos
	
	# Dies dient lediglich der einfacheren Fehlersuche, die Zeilen werden 
	# angezeigt und es wird nach doppelten Fällen gefiltert
	print(pos)
	print(row)
	if (lock > 1) {
		error <- c(error,row)
	}
	if (lock == 0) {
		error <- c(error,row)
	}
}

count_cases <- data.frame(
	case = c("count_52", "count_48", "count_18", "count_9_", "count_NA", "count_live"),
	n = c(count_52, count_48, count_18, count_9_, count_NA, count_live))
count_cases

rm(error, pos, row, lock, dead)