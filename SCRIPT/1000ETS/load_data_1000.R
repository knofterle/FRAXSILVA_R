################ LOAD DATA #####################################################
# J.Osewold
# 11.05.2022
##### LOOKS GOOD #####
################################################################################

###### LIBRARYS -------------------------------------------------------------------

###### REQUIRES -------------------------------------------------------------------

###### NOTES -------------------------------------------------------------------
# Ich ueberlege noch ob die Durchmesser und Toten und ETS Stufen exporte anders
# geloest werden sollten.

### 1. EINLESEN DER DATEN ------------------------------------------------------
data_1000 <- read.csv2(file = "DATA/RAW/1000ETS/daten_veraendert_20211217_osw.csv",
                  stringsAsFactors = F) 

### 2. BEREINIGUNG -------------------------------------------------------------
# 
# 
# selection <- data$ETS..problematisch
# data <- data[!selection,]   
# Es wurden manuell fehlerhafte ETS-Verlaeufe markiert und nun in einem separaten
# Datensatz gespeichert. Obwohl es fuer viele Analysen okay ist wenn die ETS 
# Verlaeufe nicht ganz korrekt sind, ist die Zahl doch so gering, dass sie 
# einfach fuer alle entfernt werden koennen. Ich war mir nicht mehr sicher wieso
# die Verlaeufe unplausibel waren, daher habe ich sie erstmal doch belassen

### 3. AUSGABE DER FLAECHEN UND PARZELLEN IDS -----------------------------------
ParzID <- levels(as.factor(data_1000$edv_id_00)) # length 35
FlaeID <- data_1000$edv_id
FlaeID <- levels(as.factor(FlaeID)) # length 33

#  Die FlaechenIDs werden immer mal wieder benoetigt um Tabellen zu beschriften. 
# Außerdem dienen sie als Liste um bei for-Schleifen alle Flaechen zu bearbeiten.
# Sie entsprechen den Dateinamen der mdb-Archive von der Waldwachstumskunde 
# Datenbank. Daher mussten drei FlaeIDs geaendert werden. Aber manuell in den 
# Rohdaten.
#  Wo werden die ParzIDs benoetigt? In der Verbindung von Konkurrenz aus den 
# mdb Dateien und der ETS Daten

### 4. EINE BAUM ID WIRD ERSTELLT  ---------------------------------------------

ETSBaumIDgeneriert <- paste0(data_1000$edv_id_00, data_1000$bnr, "311") 
data_1000$baum_id <- ETSBaumIDgeneriert 
# Diese IDs ist nuetzlich um Abfragen mit der  Waldwachtumskunde Datenbank zu 
# machen, denn dort werden die IDs genauso gebildet

### 5. EINZELNE WERTE ALS SEPARATE TABELLEN ------------------------------------
ETSStufen_1000 <- data.frame("2013" = data_1000$ets_stufe13, 
                        "2014" = data_1000$ets_stufe14, 
                        "2015" = data_1000$ets_stufe15, 
                        "2016" = data_1000$ets_stufe16, 
                        "2017" = data_1000$ets_stufe17, 
                        "2018" = data_1000$ets_stufe_18, 
                        "2019" = data_1000$ets_stufe_19, 
                        "2020" = data_1000$ets_stufe_20,
                        "2021" = data_1000$ets_stufe_21,
                        check.names = F)

# The following table can be used for average ets level and so on
ETSStufen_1000_clean <- ETSStufen_1000
ETSStufen_1000_clean[ETSStufen_1000_clean == 8] <- NA
ETSStufen_1000_clean[ETSStufen_1000_clean == 9] <- NA
ETSStufen_1000_clean[ETSStufen_1000_clean == 17] <- NA

Tote_1000 <- data_1000[, c("tot_14","tot_15","tot_16","tot_17","tot_18","tot_19",
                 "tot_20", "tot_21")]
Tote_1000[is.na (Tote_1000)] <- 0

#  Einige Durchmesser sind durch Messfehler oder Baumverwechslungen unplausibel, 
# diese wurden vorher markiert und werden jetzt aussortier
Durchmesser_1000 <- data_1000[, c("d_14", "d_15", "d_16", "d_17", "d_18", "d_19", "d_20",
                        "d_21")]
selection <- data_1000$Durchmesser.problematisch 
selection[is.na(selection)] <- FALSE
Durchmesser_1000 <- Durchmesser_1000[!selection, ] # nrow 984




### TIDY UP  -------------------------------------------------------------------
rm(selection, ETSBaumIDgeneriert)

###### OUTPUT ------------------------------------------------------------------
# data_1000 beinhaltet die ETS Rohdaten sehr haesslich
# Durchmesser: beinhaltet nur die Durchmesser
# ETSStufen
# Tote
# FlaeID
# ParzID
