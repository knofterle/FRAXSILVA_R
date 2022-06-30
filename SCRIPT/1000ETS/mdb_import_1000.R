################ MDB_IMPORT_GENERAL ############################################
# J.Osewold
# 11.05.22
##### DONE #####
################################################################################

###### LIBRARYS -------------------------------------------------------------------

###### REQUIRES -------------------------------------------------------------------
# This runs all previously necessary scripts
source(file = "SCRIPT/1000ETS/load_data_1000.R")
# 
# FlaeID
# 
###### NOTES -------------------------------------------------------------------
# Die Konkurrenz (c66) hat Susanne Sprauer bereits in .mdb Archiven berechnet
# 

###### DECIDE FOR LINUX OR WINDOWS  --------------------------------------------

if (.Platform$OS.type == "windows") {
  source("SCRIPT/1000ETS/mdb_Import_Windows.R", echo = T)
} else {
  source("SCRIPT/1000ETS/mdb_Import_Linux.R", echo = T)
}

###### CLEAN THE DATA  ---------------------------------------------------------

Baum$r[is.na(Baum$r)] <- "" 
# Es war eine Mischung aus NA und ""; die Spalte identfiziert die Randbaeume

Baum$nr <- gsub(" ","", Baum$nr) 
# Alle Baumnummern werden ihrer Leerzeichen entledigt

Konkurrenz <- Konkurrenz[Konkurrenz$typ == "c66",]
# Der Konkurrenztyp "KKL" wird ignoriert

### TIDY UP  -------------------------------------------------------------------
rm()

###### OUTPUT ------------------------------------------------------------------
# Konkurrenz
# Baum
# Aufnahmen



###### JUNK ------------------------------------------------------------------

# zunaechst werden 2 Konkurrenz indices in allen Aufnahmen aller Eschen geladen, 
# anschliessend kann sortiert werden welche Aufnahme zu welchem Zeitpunkt 
# relevant war. Es werden nur c66 und davon "k" und "kxy" gewaehlt
# der eine wird für die Randbäume interessant der andere für alle anderen

# Konkurrenzc66 <- Konkurrenz[Konkurrenz$typ == "c66",]

# Konkurrenzc66$edvid[Konkurrenzc66$edvid == "K15302KF"] <- "K1530200"
# Konkurrenzc66$edvid[Konkurrenzc66$edvid == "K16302KF"] <- "K1630200"

#  Die KF Parzellen lassen sich auf diese Weise besser korrigieren als sie in der 
# ursprünglichen Datenbank zu aendern, denn da ist nicht klar zu welcher Parzelle
# sie gehoeren und werden im schlimmsten Fall nicht gefunden.
# Ich habe händisch überprüft dass es in den Parzellen K15302KF und K1530200 
# (und K16...) keine Baeume mit gleicher ID gibt.


# Aufnahmen$edvid [Aufnahmen$edvid == "K15302KF"] <- "K1530200"
# Aufnahmen$edvid [Aufnahmen$edvid == "K16302KF"] <- "K1630200"

# Baum$edvid [Baum$edvid == "K15302KF"] <- "K1530200"
# Baum$edvid [Baum$edvid == "K16302KF"] <- "K1630200"
#  Es gibt zwischen K16302Kf und K1630200 doch gleiche Baumnummern, daher wurden 
# die edvids bei der Baum Tabelle nicht geändert, das wird später bestimmt 
# nochmal hässlich...

#  UPDATE: In der Wachstumskunde Datenbank gibt keine Bäume mit edvid K1530200
# alle Baeume der Flaeche heißen K15302KF. Anders bei K16 dort gibt es viele 
# (nicht alle) Baeume doppelt, einmal als edvid K1630200 und K16302KF. In der 
# ETS-Datenbank habe ich alle K15er zu K15302KF geändert, alle K16 sind gleich 
# geblieben. Es scheint gelöst!


# Backup <- list(Konkurrenzc66 = Konkurrenzc66, Konkurrenz = Konkurrenz, 
#                data = data, Baum = Baum)
