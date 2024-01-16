#============================ TITLE ===========================================#
# J.Osewold
# 08.01.2024
# STATUS
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here

## LIBRARYS --------------------------------------------------------------------
require(readxl)
require(dplyr)
require(chron)

## NOTES -----------------------------------------------------------------------
# Ich habe die Excel Tabellen und csv pro Fläche manuell zusammen gefasst damit
# ich hier nicht 24 Dateien einzeln einlesen muss. Allerdings sind dadurch die 
# Daten durcheinander und teilweise doppelt.
# 
# Schotten 6 beinhaltet leider nur 3 von 4 Dateien. Das Problem war, dass eine 
# der csv Dateien komplettt scheiße formatiert war. Das wird deshalb noch extra 
# bearbeitet. Das trifft vermutlich auf alle Dateien von Mai 2022 zu.
# 
# Schotten 2 war außerdem kaputt, da ist ständig die Batterie leer gewesen.
# 
# Außerdem hatte ich keine Lust auf den Nerv die alle nochmal in csv umzuwandeln
# deshalb gibt es hier außnahmsweise mal ein readxlsx
# 
# Tja wie sich herausstellte ist es ein absoluter Krampf die Dateien miteinander
# zu verbinden. Hauptprobleme: Zeitumstellung (!), Dezimalstellen, Zeitformate,
# Rundungen und Sekundenabweichungen.
# 

## S1 --------------------------------------------------------------------------
### READ  ----------------------------------------------------------------------
S1_ex <- read_xlsx(path = "DATA/RAW/Schotten/Logger/HOBO/Schotten 1 2023-06-16 14_10_25 CET (Data CET).xlsx")
# HIer fehlt am 26. März 2023 keine Stunde... 
# Aber weil ich so spät abgelesen habe gibt es keine Überschneidung mit den 
# anderen Dateien, insofern weiß nicht ob die Zeitverschiebung passt und es 
# interssiert mich auch nicht.

S1_ex2 <- read_xlsx(path = "DATA/RAW/Schotten/Logger/HOBO/Schotten 1 2022-08-15 18_12_59 CEST (Data CEST).xlsx")
# Die Daten reichen nicht bis in den Herbst aber im 27. März fehlt eine Stunde

S1_Rest <- read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 1 2022-05-05 12_10_33 CEST (Data CEST).csv", 
										skip = 1, header = F, 
										colClasses = c(rep("character", times = 8)))
# Uhr wird umgestellt es gibt am 31. okt doppelte Werte
# Trotzdem sind ex2 und Rest die ganze Zeit um eine Stunde verschoben, machen aber
# beide um 0200 die Uhrumstellung im März mit. Ich komme absolut nichzt dahinter 
# wie das sein kann aber mir fällt nicht so richtig was besseres ein als ex2 
# eine Stunde abzuziehen. Das mache ich jetzt einfach.
# Außerdem werden die führenden Nullen bei "Rest" gelöscht wenn die Spalte als
# Nummer eingelesen wird. Tja das ist lange nicht aufgefallen. Aus 1,06 wird dann 
# 1,6. Ich verstehe nicht warum das bei Rest 2 nicht passiert aber an diesem
# Punkt höre ich auf zu fragen.

S1_Rest2 <-  read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 1 2021-12-07 12_17_47 CET (Data CET)(1).csv", 
											skip = 1, header = F)
# Uhr wird umgestellt es gibt am 31. okt doppelte Werte
# Rest2 und Rest sind manchmal aber nicht immer um eine Sekunde verschoben. Noch
# so ein Fall in dem es mich nun wirklich nicht mehr interessiert warum. Die 
# bleiben dann beim doppelte löschen erhalten. Pech gehabt.

### REPARE REST  ---------------------------------------------------------------------
# Der Zeitcharacter wird in time und date geteilt und anschließend mit chron
# in ein Zeitformat gewandelt. Die anderen Dateien werden als POSIX gelesen daher
# müssen die auch nochmal umgewandelt werden. 
t <- strsplit(S1_Rest$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m.d.y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S1_Rest$V2 <- t
# Eventuell liegt ein Fehler auch in der "CET" angabe hier. Aber es funktioniert 
# jetzt und wird nicht mehr angerührt, ne leider geht das nicht. Die verschiedenen
# Flächen müssen ja auch zusammen passen...
# Naja verändern tue ich ja später ex2 und trotzdem passen Rest und REst2 zusammen
# wahrscheinlich macht die CET angabe hier gar keinen Unterschied...

# Hier wurde die csv scheiße gebaut sowohl decimalpoint als auch separator war ein 
# Komma... Daher ein bisschen aufräumarbeit.
colnames(S1_Rest) <- c("ID", "time", "temp", "humid", "dewpoint", "V6", "V7", "V8")
S1_Rest$temp <- as.numeric(paste0(S1_Rest$temp,".",S1_Rest$humid))
S1_Rest$humid <- as.numeric(paste0(S1_Rest$dewpoint,".",S1_Rest$V6))
S1_Rest$dewpoint <- as.numeric(paste0(S1_Rest$V7,".",S1_Rest$V8))
S1_Rest <- as_tibble(S1_Rest[,1:5])

### REPARE REST2  ---------------------------------------------------------------------
t <- strsplit(S1_Rest2$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m/d/y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S1_Rest2$V2 <- t # -1 # minus eine sekunde, doch nicht bzw scheint egal zu sein
colnames(S1_Rest2) <- c("ID", "time", "temp", "humid", "dewpoint", "V6", "V7")
S1_Rest2 <- as_tibble(S1_Rest2[ ,1:5])

### REPARE ex2 & ex ---------------------------------------------------------------------
# Ex2 ist so weit ich weiß um eine Stunde versetzt was mir immer noch ein 
# ziemliches Rätsel ist aber naja hier wirds geändert
colnames(S1_ex2) <- c("ID", "time", "temp", "humid", "dewpoint")
S1_ex2$time <- (S1_ex2$time -3600)
S1_ex2[,3:5] <- round(S1_ex2[ ,3:5], 2)

S1_ex[,3:5] <- round(S1_ex[ ,3:5], 2)

### MERGE BOTH  ---------------------------------------------------------------------
colnames(S1_ex) <- c("ID", "time", "temp", "humid", "dewpoint")

# Es gibt durch die Zeitverschiebung in zwei Tabellen Doppelte Werte im Oktober
# die werden jetzt einfach gelöscht!
S1_ex <- S1_ex[!duplicated(S1_ex$time),]
S1_ex$file <- "ex_20230616"
S1_ex2 <- S1_ex2[!duplicated(S1_ex2$time),]
S1_ex2$file <- "ex2_20220815"
S1_Rest <- S1_Rest[!duplicated(S1_Rest$time),]
S1_Rest$file <- "Rest_20220505"
S1_Rest2 <- S1_Rest2[!duplicated(S1_Rest2$time),]
S1_Rest2$file <- "Rest2_20211207"

S1 <- rbind(S1_ex, S1_ex2, S1_Rest, S1_Rest2)

### CLEAN  ---------------------------------------------------------------------
# Es wurden einige Zeilen mit dem Zeitstempel des auslesens gemacht. Die werden 
# hier gelöscht.
selection <- which(is.na(S1$temp))
S1 <- S1[-selection, ]
S1 <- S1[order(S1$time), ]

# Hier läuft der Test ob es noch doppelte Zeiten gibt deren Temperaturen nicht 
# gleich sind.
double <- which(duplicated(S1$time))
notokay <- 0
for (i in double) {
	if(!(S1$temp[i] == S1$temp[i-1])) {
		notokay <- c(notokay, i)
	}
}
notokayID <- S1$ID[notokay]

# Hier werden die doppelten nach Prüfung entfernt:

S1 <- S1[-double, ]

# S2 --------------------------------------------------------------------------
### READ ------------------------------------------------------------------------
S2_1 <- read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 2 2021-12-07 12_02_33 CET (Data CET)(1).csv",
								 skip = 1, header = F)
S2_2 <- read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 2 2022-05-19 11_34_22 CEST (Data CEST).csv",
								 skip = 1, header = F, 
								 colClasses = c(rep("character", times = 8)))
### REPARE ---------------------------------------------------------------------
# Repare S2_2 Time and Date
t <- strsplit(S2_2$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m.d.y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S2_2$V2 <- t
# Hier wurde die csv scheiße gebaut sowohl decimalpoint als auch separator war ein 
# Komma... Daher ein bisschen aufräumarbeit.
colnames(S2_2) <- c("ID", "time", "temp", "humid", "dewpoint", "V6", "V7", "V8")
S2_2$temp <- as.numeric(paste0(S2_2$temp,".",S2_2$humid))
S2_2$humid <- as.numeric(paste0(S2_2$dewpoint,".",S2_2$V6))
S2_2$dewpoint <- as.numeric(paste0(S2_2$V7,".",S2_2$V8))
S2_2 <- as_tibble(S2_2[,1:5]) 

# Repare S2_1
t <- strsplit(S2_1$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m/d/y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S2_1$V2 <- t

### MERGE ----------------------------------------------------------------------
colnames(S2_1) <- c("ID", "time", "temp", "humid", "dewpoint")
S2_1 <- as_tibble(S2_1[ ,1:5])
S2_1$ID <- as.numeric(S2_1$ID)

S2_1 <- S2_1[!duplicated(S2_1$time), ]
S2_1$file <- "2021-12-07"
S2_2 <- S2_2[!duplicated(S2_2$time), ]
S2_2$file <- "2022-05-19"

S2 <- rbind(S2_1, S2_2)

### CLEAN ----------------------------------------------------------------------
# Es wurden einige Zeilen mit dem Zeitstempel des auslesens gemacht. Die werden 
# hier gelöscht.
selection <- which(is.na(S2$temp))
S2 <- S2[-selection, ]
S2 <- S2[order(S2$time), ]

# Hier läuft der Test ob es noch doppelte Zeiten gibt deren Temperaturen nicht 
# gleich sind.
double <- which(duplicated(S2$time))
# double ist in diesem Fall leer
notokay <- 0
for (i in double) {
	if(!(S2$temp[i] == S2$temp[i-1])) {
		notokay <- c(notokay, i)
	}
}
notokayID <- S2$ID[notokay]

## S3 --------------------------------------------------------------------------
### READ ---------------------------------------------------------------------
S3_1 <- read_xlsx(path = "DATA/RAW/Schotten/Logger/HOBO/Schotten 3 2022-08-15 17_31_58 CEST (Data CEST).xlsx")
S3_2 <- read_xlsx(path = "DATA/RAW/Schotten/Logger/HOBO/Schotten 3 2023-06-16 13_31_08 CET (Data CET).xlsx")
S3_3 <- read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 3 2022-05-04 17_54_04 CEST (Data CEST).csv",
								 skip = 1, header = F,
								 colClasses = c(rep("character", times = 8)))
S3_4 <- read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 3 2021-12-07 11_55_09 CET (Data CET).csv", 
										skip = 1, header = F)

### REPARE ---------------------------------------------------------------------
colnames(S3_1) <- c("ID", "time", "temp", "humid", "dewpoint")
S3_1$time <- (S3_1$time -3600)
S3_1[,3:5] <- round(S3_1[ ,3:5], 2)

colnames(S3_2) <- c("ID", "time", "temp", "humid", "dewpoint")
S3_2[,3:5] <- round(S3_2[ ,3:5], 2)

t <- strsplit(S3_3$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m.d.y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S3_3$V2 <- t
# Hier wurde die csv scheiße gebaut sowohl decimalpoint als auch separator war ein 
# Komma... Daher ein bisschen aufräumarbeit.
colnames(S3_3) <- c("ID", "time", "temp", "humid", "dewpoint", "V6", "V7", "V8")
S3_3$temp <- as.numeric(paste0(S3_3$temp,".",S3_3$humid))
S3_3$humid <- as.numeric(paste0(S3_3$dewpoint,".",S3_3$V6))
S3_3$dewpoint <- as.numeric(paste0(S3_3$V7,".",S3_3$V8))
S3_3 <- as_tibble(S3_3[,1:5]) 

t <- strsplit(S3_4$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m/d/y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S3_4$V2 <- t

### MERGE ----------------------------------------------------------------------
S3_1 <- S3_1[!duplicated(S3_1$time), ]
S3_1$file <- "xlsx 2022-08-15"

S3_2 <- S3_2[!duplicated(S3_2$time), ]
S3_2$file <- "xlsx 2023-06-16"

S3_3 <- S3_3[!duplicated(S3_3$time), ]
S3_3$file <- "csv 2022-05-04"

S3_4 <- S3_4[ ,1:5]
colnames(S3_4) <- c("ID", "time", "temp", "humid", "dewpoint")
S3_4 <- as_tibble(S3_4)
S3_4 <- S3_4[!duplicated(S3_4$time), ]
S3_4$file <- "csv 2021-12-07"

S3 <- rbind(S3_1, S3_2, S3_3, S3_4)

### CLEAN ----------------------------------------------------------------------
# Es wurden einige Zeilen mit dem Zeitstempel des auslesens gemacht. Die werden 
# hier gelöscht.
selection <- which(is.na(S3$temp))
S3 <- S3[-selection, ]
S3 <- S3[order(S3$time), ]

# Hier läuft der Test ob es noch doppelte Zeiten gibt deren Temperaturen nicht 
# gleich sind.
double <- which(duplicated(S3$time))
# double ist in diesem Fall leer
notokay <- 0
for (i in double) {
	if(!(S3$temp[i] == S3$temp[i-1])) {
		notokay <- c(notokay, i)
	}
}
notokayID <- S3$ID[notokay]

# Hier werden die doppelten nach Prüfung entfernt:
S3 <- S3[-double, ]

## S4 --------------------------------------------------------------------------
### READ ---------------------------------------------------------------------
S4_1 <- read_xlsx(path = "DATA/RAW/Schotten/Logger/HOBO/Schotten 4 2022-08-15 16_57_32 CEST (Data CEST).xlsx")
S4_2 <- read_xlsx(path = "DATA/RAW/Schotten/Logger/HOBO/Schotten 4 2023-06-16 11_40_15 CET (Data CET).xlsx")
S4_3 <- read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 4 2022-05-03 17_16_54 CEST (Data CEST).csv",
								 skip = 1, header = F,
								 colClasses = c(rep("character", times = 8)))
S4_4 <- read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 4 2021-12-07 11_35_19 CET (Data CET).csv", 
								 skip = 1, header = F)

### REPARE ---------------------------------------------------------------------
colnames(S4_1) <- c("ID", "time", "temp", "humid", "dewpoint")
S4_1$time <- (S4_1$time -3600)
S4_1[,3:5] <- round(S4_1[ ,3:5], 2)

colnames(S4_2) <- c("ID", "time", "temp", "humid", "dewpoint")
S4_2[,3:5] <- round(S4_2[ ,3:5], 2)

t <- strsplit(S4_3$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m.d.y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S4_3$V2 <- t
# Hier wurde die csv scheiße gebaut sowohl decimalpoint als auch separator war ein 
# Komma... Daher ein bisschen aufräumarbeit.
colnames(S4_3) <- c("ID", "time", "temp", "humid", "dewpoint", "V6", "V7", "V8")
S4_3$temp <- as.numeric(paste0(S4_3$temp,".",S4_3$humid))
S4_3$humid <- as.numeric(paste0(S4_3$dewpoint,".",S4_3$V6))
S4_3$dewpoint <- as.numeric(paste0(S4_3$V7,".",S4_3$V8))
S4_3 <- as_tibble(S4_3[,1:5]) 

t <- strsplit(S4_4$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m/d/y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S4_4$V2 <- t

### MERGE ----------------------------------------------------------------------
S4_1 <- S4_1[!duplicated(S4_1$time), ]
S4_1$file <- "xlsx 2022-08-15"

S4_2 <- S4_2[!duplicated(S4_2$time), ]
S4_2$file <- "xlsx 2023-06-16"

S4_3 <- S4_3[!duplicated(S4_3$time), ]
S4_3$file <- "csv 2022-05-04"

S4_4 <- S4_4[ ,1:5]
colnames(S4_4) <- c("ID", "time", "temp", "humid", "dewpoint")
S4_4 <- as_tibble(S4_4)
S4_4 <- S4_4[!duplicated(S4_4$time), ]
S4_4$file <- "csv 2021-12-07"

S4 <- rbind(S4_1, S4_2, S4_3, S4_4)

### CLEAN ----------------------------------------------------------------------
# Es wurden einige Zeilen mit dem Zeitstempel des auslesens gemacht. Die werden 
# hier gelöscht.
selection <- which(is.na(S4$temp))
S4 <- S4[-selection, ]
S4 <- S4[order(S4$time), ]

# Hier läuft der Test ob es noch doppelte Zeiten gibt deren Temperaturen nicht 
# gleich sind.
double <- which(duplicated(S4$time))
# double ist in diesem Fall leer
notokay <- 0
for (i in double) {
	if(!(S4$temp[i] == S4$temp[i-1])) {
		notokay <- c(notokay, i)
	}
}
notokayID <- S4$ID[notokay]

# Hier werden die doppelten nach Prüfung entfernt:
S4 <- S4[-double, ]


## S5 --------------------------------------------------------------------------
### READ ---------------------------------------------------------------------
S5_1 <- read_xlsx(path = "DATA/RAW/Schotten/Logger/HOBO/Schotten 5 2022-08-15 17_43_35 CEST (Data CEST).xlsx")
S5_2 <- read_xlsx(path = "DATA/RAW/Schotten/Logger/HOBO/Schotten 5 2023-06-16 13_52_07 CET (Data CET).xlsx")
S5_3 <- read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 5 2022-05-04 15_37_21 CEST (Data CEST).csv",
								 skip = 1, header = F,
								 colClasses = c(rep("character", times = 8)))
S5_4 <- read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 5 2021-12-07 11_49_55 CET (Data CET).csv", 
								 skip = 1, header = F)

### REPARE ---------------------------------------------------------------------
colnames(S5_1) <- c("ID", "time", "temp", "humid", "dewpoint")
S5_1$time <- (S5_1$time -3600)
S5_1[,3:5] <- round(S5_1[ ,3:5], 2)

colnames(S5_2) <- c("ID", "time", "temp", "humid", "dewpoint")
S5_2[,3:5] <- round(S5_2[ ,3:5], 2)

t <- strsplit(S5_3$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m.d.y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S5_3$V2 <- t
# Hier wurde die csv scheiße gebaut sowohl decimalpoint als auch separator war ein 
# Komma... Daher ein bisschen aufräumarbeit.
colnames(S5_3) <- c("ID", "time", "temp", "humid", "dewpoint", "V6", "V7", "V8")
S5_3$temp <- as.numeric(paste0(S5_3$temp,".",S5_3$humid))
S5_3$humid <- as.numeric(paste0(S5_3$dewpoint,".",S5_3$V6))
S5_3$dewpoint <- as.numeric(paste0(S5_3$V7,".",S5_3$V8))
S5_3 <- as_tibble(S5_3[,1:5]) 

t <- strsplit(S5_4$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m/d/y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S5_4$V2 <- t

### MERGE ----------------------------------------------------------------------
S5_1 <- S5_1[!duplicated(S5_1$time), ]
S5_1$file <- "xlsx 2022-08-15"

S5_2 <- S5_2[!duplicated(S5_2$time), ]
S5_2$file <- "xlsx 2023-06-16"

S5_3 <- S5_3[!duplicated(S5_3$time), ]
S5_3$file <- "csv 2022-05-04"

S5_4 <- S5_4[ ,1:5]
colnames(S5_4) <- c("ID", "time", "temp", "humid", "dewpoint")
S5_4 <- as_tibble(S5_4)
S5_4 <- S5_4[!duplicated(S5_4$time), ]
S5_4$file <- "csv 2021-12-07"

S5 <- rbind(S5_1, S5_2, S5_3, S5_4)

### CLEAN ----------------------------------------------------------------------
# Es wurden einige Zeilen mit dem Zeitstempel des auslesens gemacht. Die werden 
# hier gelöscht.
selection <- which(is.na(S5$temp))
S5 <- S5[-selection, ]
S5 <- S5[order(S5$time), ]

# Hier läuft der Test ob es noch doppelte Zeiten gibt deren Temperaturen nicht 
# gleich sind.
double <- which(duplicated(S5$time))
# double ist in diesem Fall leer
notokay <- 0
for (i in double) {
	if(!(S5$temp[i] == S5$temp[i-1])) {
		notokay <- c(notokay, i)
	}
}
notokayID <- S5$ID[notokay]

# Hier werden die doppelten nach Prüfung entfernt:
S5 <- S5[-double, ]

## S6 --------------------------------------------------------------------------
### READ ---------------------------------------------------------------------
S6_1 <- read_xlsx(path = "DATA/RAW/Schotten/Logger/HOBO/Schotten 6 2022-08-15 17_19_00 CEST (Data CEST).xlsx")
S6_2 <- read_xlsx(path = "DATA/RAW/Schotten/Logger/HOBO/Schotten 6 2023-06-16 12_25_16 CET (Data CET).xlsx")
S6_3 <- read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 6 2022-05-04 13_11_04 CEST (Data CEST).csv",
								 skip = 1, header = F,
								 colClasses = c(rep("character", times = 8)))
S6_4 <- read.csv(file = "DATA/RAW/Schotten/Logger/HOBO/Schotten 6 2021-12-07 11_41_22 CET (Data CET).csv", 
								 skip = 1, header = F)

### REPARE ---------------------------------------------------------------------
colnames(S6_1) <- c("ID", "time", "temp", "humid", "dewpoint")
S6_1$time <- (S6_1$time -3600)
S6_1[,3:5] <- round(S6_1[ ,3:5], 2)

colnames(S6_2) <- c("ID", "time", "temp", "humid", "dewpoint")
S6_2[,3:5] <- round(S6_2[ ,3:5], 2)

t <- strsplit(S6_3$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m.d.y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S6_3$V2 <- t
# Hier wurde die csv scheiße gebaut sowohl decimalpoint als auch separator war ein 
# Komma... Daher ein bisschen aufräumarbeit.
colnames(S6_3) <- c("ID", "time", "temp", "humid", "dewpoint", "V6", "V7", "V8")
S6_3$temp <- as.numeric(paste0(S6_3$temp,".",S6_3$humid))
S6_3$humid <- as.numeric(paste0(S6_3$dewpoint,".",S6_3$V6))
S6_3$dewpoint <- as.numeric(paste0(S6_3$V7,".",S6_3$V8))
S6_3 <- as_tibble(S6_3[,1:5]) 

t <- strsplit(S6_4$V2,' ')
t <- as_tibble(matrix(unlist(t), ncol = 2, byrow = T), column_name = c("V1", "V2"))
t <- chron(
	dates = t$V1,
	times = t$V2,
	format = c('m/d/y', 'h:m:s')
)
t <- as.POSIXlt(t, tz = "CET")
S6_4$V2 <- t

### MERGE ----------------------------------------------------------------------
S6_1 <- S6_1[!duplicated(S6_1$time), ]
S6_1$file <- "xlsx 2022-08-15"

S6_2 <- S6_2[!duplicated(S6_2$time), ]
S6_2$file <- "xlsx 2023-06-16"

S6_3 <- S6_3[!duplicated(S6_3$time), ]
S6_3$file <- "csv 2022-05-04"

S6_4 <- S6_4[ ,1:5]
colnames(S6_4) <- c("ID", "time", "temp", "humid", "dewpoint")
S6_4 <- as_tibble(S6_4)
S6_4 <- S6_4[!duplicated(S6_4$time), ]
S6_4$file <- "csv 2021-12-07"

S6 <- rbind(S6_1, S6_2, S6_3, S6_4)

### CLEAN ----------------------------------------------------------------------
# Es wurden einige Zeilen mit dem Zeitstempel des auslesens gemacht. Die werden 
# hier gelöscht.
selection <- which(is.na(S6$temp))
S6 <- S6[-selection, ]
S6 <- S6[order(S6$time), ]

# Hier läuft der Test ob es noch doppelte Zeiten gibt deren Temperaturen nicht 
# gleich sind.
double <- which(duplicated(S6$time))
# double ist in diesem Fall leer
notokay <- 0
for (i in double) {
	if(!(S6$temp[i] == S6$temp[i-1])) {
		notokay <- c(notokay, i)
	}
}
notokayID <- S6$ID[notokay]

# Hier werden die doppelten nach Prüfung entfernt:
S6 <- S6[-double, ]

## MERGE FINAL -----------------------------------------------------------------
S1$Messgeraet <- "S1"
S2$Messgeraet <- "S2"
S3$Messgeraet <- "S3"
S4$Messgeraet <- "S4"
S5$Messgeraet <- "S5"
S6$Messgeraet <- "S6"
S <- rbind(S1, S2, S3, S4, S5, S6)
slice_sample(S,n = 10)

## TIDY UP  --------------------------------------------------------------------
rm(t, S1_ex, S1_ex2, S1_Rest, S1_Rest2, S2_1, S2_2, S3_1, S3_2, S3_3, S3_4,
	 S4_1, S4_2, S4_3, S4_4, S5_1, S5_2, S5_3, S5_4, S6_1, S6_2, S6_3, S6_4,
	 double, i, notokay, notokayID, selection)

## OUTPUT ----------------------------------------------------------------------
# S1 - S6
# S



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

# head(S1_ex2)
# S1_ex2$time[1] -3600
# as.character(S1_ex2[1,3])
# head(S1_ex2)
# 



