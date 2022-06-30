################ mdb_Import_Windows.R ################################
# J.Osewold
# 11.05.22
##### DONE #####
################################################################################

###### LIBRARYS -------------------------------------------------------------------
library(RODBC) 

###### REQUIRES -------------------------------------------------------------------
# 
# FlaeID

###### NOTES -------------------------------------------------------------------


###### SET EMPTY TABLES  -------------------------------------------------------

DefaultWorkingDirectory <- getwd()
mdbWorkingDirectory <- "DATA/RAW/1000ETS/mdb von Susanne/"
setwd(mdbWorkingDirectory)
Konkurrenz <- data.frame()
Aufnahmen <- data.frame()
Baum <- data.frame()
stammv <- data.frame()

###### LOOP  -------------------------------------------------------------------

for (i in 1:length(FlaeID)) {
  AktuelleFlaeche <- paste0(FlaeID[i], ".mdb")
  print(AktuelleFlaeche)
  channel <- odbcConnectAccess(AktuelleFlaeche)
  KonkAktuelleFlaeche <- sqlFetch(channel,"Konk")
  Konkurrenz <- rbind(Konkurrenz, KonkAktuelleFlaeche)
  AufAktuelleFlaeche <- sqlFetch(channel,"Auf")
  Aufnahmen <- rbind(Aufnahmen, AufAktuelleFlaeche)
  BaumAktuelleFlaeche <- sqlFetch(channel,"Baum")
  Baum <- rbind (Baum, BaumAktuelleFlaeche)
  StammvAktuelleFlaeche <- sqlFetch(channel,"stammv")
  stammv <- rbind (stammv, BaumAktuelleFlaeche)
  odbcCloseAll()
  print(nrow(KonkAktuelleFlaeche))
}

### TIDY UP  -------------------------------------------------------------------
setwd(DefaultWorkingDirectory)

rm(AktuelleFlaeche, channel, KonkAktuelleFlaeche, AufAktuelleFlaeche, 
   BaumAktuelleFlaeche, i, mdbWorkingDirectory, DefaultWorkingDirectory, 
   StammvAktuelleFlaeche)

###### OUTPUT ------------------------------------------------------------------
# Konkurrenz
# Aufnahmen
# Baum

