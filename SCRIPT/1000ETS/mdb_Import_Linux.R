################ mdb_Import_Linux.R ################################
# J.Osewold
# 21.12.21
##### DONE #####
################################################################################

###### LIBRARYS -------------------------------------------------------------------
library(Hmisc) 

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

###### LOOP  -------------------------------------------------------------------

for (i in 1:length(FlaeID)) {
  AktuelleFlaeche <- paste0(FlaeID[i], ".mdb")
  print(AktuelleFlaeche)
  KonkAktuelleFlaeche <- 
    mdb.get(file = AktuelleFlaeche, tables = "Konk", stringsAsFactors = F)
  Konkurrenz <- rbind(Konkurrenz, KonkAktuelleFlaeche)
  AufAktuelleFlaeche <- 
    mdb.get(file = AktuelleFlaeche, tables = "Auf", stringsAsFactors = F)
  Aufnahmen <- rbind(Aufnahmen, AufAktuelleFlaeche)
  BaumAktuelleFlaeche <- 
    mdb.get(file = AktuelleFlaeche, tables = "Baum", stringsAsFactors = F)
  Baum <- rbind (Baum, BaumAktuelleFlaeche)
  print(nrow(KonkAktuelleFlaeche))
}

### TIDY UP  -------------------------------------------------------------------
setwd(DefaultWorkingDirectory)

rm(AktuelleFlaeche, channel, KonkAktuelleFlaeche, AufAktuelleFlaeche, 
   BaumAktuelleFlaeche, i, mdbWorkingDirectory, DefaultWorkingDirectory)

###### OUTPUT ------------------------------------------------------------------
# Konkurrenz
# Aufnahmen
# Baum
