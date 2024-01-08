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

## READ  ---------------------------------------------------------------------
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

## REPARE REST  ---------------------------------------------------------------------
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

## REPARE REST2  ---------------------------------------------------------------------
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

## REPARE ex2 & ex ---------------------------------------------------------------------
# Ex2 ist so weit ich weiß um eine Stunde versetzt was mir immer noch ein 
# ziemliches Rätsel ist aber naja hier wirds geändert
colnames(S1_ex2) <- c("ID", "time", "temp", "humid", "dewpoint")
S1_ex2$time <- (S1_ex2$time -3600)
S1_ex2[,3:5] <- round(S1_ex2[ ,3:5], 2)

S1_ex[,3:5] <- round(S1_ex[ ,3:5], 2)

## MERGE BOTH  ---------------------------------------------------------------------
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

## CLEAN  ---------------------------------------------------------------------
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
# write.csv(S1, file = "TEMP/dshj.csv")
## TIDY UP  --------------------------------------------------------------------
rm(t)

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

# head(S1_ex2)
# S1_ex2$time[1] -3600
# as.character(S1_ex2[1,3])
# head(S1_ex2)
# 



