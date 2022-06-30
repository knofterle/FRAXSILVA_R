################ KONKURRENZ_ETSSTUFE ################################
# J.Osewold
# 11.05.22
##### REWORK BUT GOOD #####
################################################################################

###### LIBRARYS -------------------------------------------------------------------

###### REQUIRES -------------------------------------------------------------------
# This runs all previously necessary scripts
source("SCRIPT/1000ETS/mdb_import_1000.R")
# 
# ParzID
# Aufnahmen
# data


###### NOTES -------------------------------------------------------------------


###### AUSWAHL DER RICHTIGEN AUFNAHME  -----------------------------------------

#  Zunaechst eine Tabelle die für jede Flaeche und jedes Jahr die dazugehoerige
# Aufnahmennummer wiedergibt. Dabei werden bleiben die alten Aufnahmen so lange
# gueltig bis neue erhoben sind, es werden keine Mittelwerte oder aehnliches 
# gebildet.

edvid <- levels(as.factor(Aufnahmen$edvid))
AufnahmenummerProJahr  <-
  data.frame(matrix(nrow = length(edvid), ncol = 10,
    dimnames = list(c(1:length(edvid)),
                    c("edvid", paste(2013:2021)))
  ), check.names = F)

AufnahmenummerProJahr$edvid <-  edvid
error <- 0

for (i in 1:length(edvid)) {
  TempAufnahme <- Aufnahmen[Aufnahmen$edvid == edvid[i], ]
  for (k in 2013:2021) {
    AlleVorherigenAufnahmen <- 
      (TempAufnahme$saison[TempAufnahme$saison <= k] - k)
    # Fuer jedes ETS Jahr wird der zeitliche Abstand zwischen allen 
    # Aufnahmen und diesem Jahr berechnet. Da die Aufnahmen immer vor dem 
    # aktuellen Jahr liegen sollen ist der Wert negativ. Daher auch der 
    # which.max, wir suchen den Wert am nächsten an null

    LetzteVorherigeAufnahme <- which.max(AlleVorherigenAufnahmen) 
    # which.max: Bei mehreren Parzellen wird die erste automatisch gewaehlt, da 
    # wir davon ausgehen, dass alle Parzellen auf einmal aufgenommen wurden 
    # sollte das gehen
    
    AufnahmenummerProJahr[i, k - 2011] <-
      TempAufnahme$auf[LetzteVorherigeAufnahme]
    
    if (length(LetzteVorherigeAufnahme) != 1) {
      error <- rbind(error, k , i)
    }
  }
}
error # NULL

###### EXTRAHIEREN DER KONKURRENZ FUER JEDE ESCHE ------------------------------


KonkBaumIDgeneriert <- paste0(Konkurrenz$edvid, gsub(" ","", Konkurrenz$nr), 
                              Konkurrenz$art)
ETSBaumIDgeneriert <- data_1000$baum_id
KonkAlleEschen <- data.frame(
  matrix(nrow = length(ETSBaumIDgeneriert), ncol = 19,
         dimnames = list(c(1:length(ETSBaumIDgeneriert)),
                         c("BaumID", paste0(2013:2021,"c66xy"),
                           paste0(2013:2021,"c66")))),
  check.names = F)

error1 <- 0
error2 <- 0
error3 <- 0
TempKonkLine <- 0

for (i in 1:length(ETSBaumIDgeneriert)) {
  print(i)
  TempKonk <- Konkurrenz[KonkBaumIDgeneriert == ETSBaumIDgeneriert[i], ]
  TempFlaeche <- which(data_1000$edv_id_00[i] == AufnahmenummerProJahr$edvid)
  TempAufnahmen <- unlist(AufnahmenummerProJahr[TempFlaeche, 2:10])
  KonkAlleEschen$BaumID[i] <- data_1000$baum_id[i]
  if (nrow(TempKonk) != 0){
    for (k in 2013:2021) {
      if (any(TempKonk$auf  == TempAufnahmen[as.character(k)])) {
        TempKonkLine[(k-2012)] <- which(TempKonk$auf 
                                        == TempAufnahmen[as.character(k)])
      } else {
        error3 <- c(error3, data_1000$baum_id[i],k, i)
      }
    }
  } else {
    error2 <- c(error2,data_1000$baum_id[i])
  }
  
  KonkAlleEschen[i,2:10] <- TempKonk$kxy[TempKonkLine]
  KonkAlleEschen[i,11:19] <- TempKonk$k[TempKonkLine]
  if (nrow(TempKonk)==0) error1 <- c(error1,i)
}

write.csv2(Konkurrenz, file = "EXPORT/1000ETS/TABLES/Konkurrenz")
write.csv2(KonkAlleEschen, file = "EXPORT/1000ETS/TABLES/KonkAlleEschen.csv")

#  error3 enthält noch Eschen die bereits enfernt wurden und die daher bei der
# letzten Aufnahme nicht mehr dabei waren. Ob aber alle Jahre in denen sie noch
# existiert haben von c66 Daten abgedeckt werden gilt es zu pruefen.

# Es wurden teilweise Baeume bei den Ertragskundlichen Aufnahmen vergessen. Zum 
# Beispiel K20 913 



### TIDY UP  -------------------------------------------------------------------
rm(TempAufnahme, AlleVorherigenAufnahmen, LetzteVorherigeAufnahme)
rm(TempAufnahmen, TempFlaeche, TempKonkLine, error1, error2,
   error3)
rm(ETSBaumIDgeneriert, TempKonk, edvid,
   i, k, KonkBaumIDgeneriert, error)


###### OUTPUT ------------------------------------------------------------------
# 



###### JUNK ------------------------------------------------------------------
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
