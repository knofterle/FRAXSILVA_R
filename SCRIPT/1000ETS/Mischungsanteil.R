#============================ TITLE ===========================================#
# J.Osewold
# 11.05.22
# Rework
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------

## REQUIRES --------------------------------------------------------------------
source (file = "SCRIPT/1000ETS/Konkurrenz_data.R")

## NOTES -----------------------------------------------------------------------


## BERECHNUNG DER MISCHUNGSANTEILE ---------------------------------------------

MischungProJahr <- data.frame(
  matrix(nrow = length(ParzID), ncol = 10,
         dimnames = list(c(1:length(ParzID)), 
                         c("ParzID", paste(2013:2021)))),
  check.names = F)
MischungProJahr$ParzID <- ParzID
BaumClean <- Baum[Baum$nr != "nurH" & Baum$nr != "cm", ] 
# Aussortieren aller Strichlisten Messungen und Hoehen 
# ausserhalb der Parzelle

for (i in 1:length(ParzID)) {
  for (k in 2:10) {                    # 2013:2021
    TempD <- BaumClean$d[BaumClean$edvid == ParzID[i] 
                         & BaumClean$auf == AufnahmenummerProJahr[i,k]  
                         & BaumClean$r == "" ]
    TempDEsche <- BaumClean$d[BaumClean$edvid == ParzID[i] & BaumClean$art == 311
                              & BaumClean$auf == AufnahmenummerProJahr[i,k]  
                              & BaumClean$r == "" ]
    TempNr <- BaumClean$nr[BaumClean$edvid == ParzID[i] 
                           & BaumClean$auf == AufnahmenummerProJahr[i,k]  
                           & BaumClean$r == "" ]
    print(i)
    print(ParzID[i])
    print(length(levels(as.factor(TempNr))) == length(TempNr))
    # Sicherheitstest, dass wirklich nur eine Aufnahme summiert wird
    MischungProJahr[i,k] <- (sum((TempDEsche/2)^2*pi) / sum((TempD/2)^2*pi))
  }
}
rm(BaumClean, TempD, TempDEsche, TempNr)

#### VERSUCH A60 WIRD ZUSAMMENGEFASST
# Die Größen der Parzellen betragen alle 0.2 ha, koennen also gleichwertig 
# gemittelt werden
TempA60misch <- MischungProJahr[MischungProJahr$ParzID %in% c("A6021301", "A6021302", 
                                                              "A6021303"), ]
TempA60misch[4,2:9] <- apply(TempA60misch[,2:9], 2, mean)
TempA60misch[4,1] <- "A6021300"
MischungProJahr[MischungProJahr$ParzID == "A6021301",] <- TempA60misch[4,]
MischungProJahr <- MischungProJahr[! MischungProJahr$ParzID %in% 
                                     c("A6021302", "A6021303"),]

rm(TempA60misch)

#### GRAFISCHE DARSTELLUNG -----------------------------------------------------
EtsStufenTemp <- ETSStufen_1000_clean

EtsStufeProBestandJahr <- data.frame(
  matrix(nrow = length(ParzID), ncol = 10,
         dimnames = list(c(1:length(ParzID)), 
                         c("ParzID", paste(2013:2021)))),
  check.names = F)
EtsStufeProBestandJahr$ParzID <- ParzID
EtsStufenTemp$edvid <- as.vector(data_1000$edv_id_00)

for (i in 1:length(ParzID)) {
  BestandTemp <- EtsStufenTemp [
    EtsStufenTemp$edvid == ParzID[i], ]
  for (k in 1:9) {
    EtsStufeProBestandJahr[i,k+1] <- mean(BestandTemp[,k], na.rm = T)
  }
}

TempA60ets <- EtsStufeProBestandJahr[EtsStufeProBestandJahr$ParzID %in% 
                                       c("A6021301", "A6021302", "A6021303"), ]
TempA60ets[4,2:9] <- apply(TempA60ets[,2:9], 2, mean)
TempA60ets[4,1] <- "A6021300"
EtsStufeProBestandJahr[EtsStufeProBestandJahr$ParzID == "A6021301",] <- TempA60ets[4,]
EtsStufeProBestandJahr <- EtsStufeProBestandJahr[
  ! EtsStufeProBestandJahr$ParzID %in% c("A6021302", "A6021303"),]

rm(EtsStufenTemp, BestandTemp, TempA60ets)
#  Die ETS Stufe wird für jedes Jahr pro Bestand gemittelt. Damit haben die Daten 
# das gleiche Format wie die Mischungsanteile die sich zwischen den Jahren 
# ebenfalls veraendern, allerdings gibt es Bestaende die nach in den letzten 
# Jahren gar keine Eschen mehr haben... NaN

EtsStufeProBestandJahr$mean <- apply (EtsStufeProBestandJahr[, 2:9], 1, mean, 
                                      na.rm = T)
MischungProJahr$mean <- apply (MischungProJahr[, 2:9], 1, mean, na.rm = T)


pdf(file = "EXPORT/1000ETS/FIGURES/Mischung_ETS_Stufe.pdf",
    width=10, height=7, paper='special')
# par("mar" = c(4,4,1.5,1))
plot(MischungProJahr$mean, EtsStufeProBestandJahr$mean, 
     main = "Zusammenhang ETS-Stufe und Mischungsanteil pro Bestand",
     ylab = "Mittlere ETS-Stufe 2013-2021", 
     xlab = "Mischungsanteil Esche 2013-2021")
par(opar)
dev.off()

pdf(file = "EXPORT/1000ETS/FIGURES/Mischung_ETS_Stufe_labeled.pdf",
    width=10, height=7, paper='special')
# par("mar" = c(4,4,1.5,1))
plot(MischungProJahr$mean, EtsStufeProBestandJahr$mean, 
     main = "Zusammenhang ETS-Stufe und Mischungsanteil pro Bestand",
     ylab = "Mittlere ETS-Stufe 2013-2021", 
     xlab = "Mischungsanteil Esche 2013-2021")
text(MischungProJahr$mean, EtsStufeProBestandJahr$mean, 
     labels = MischungProJahr$ParzID, cex=0.9, font=2, pos = 4)
dev.off()


## TIDY UP  --------------------------------------------------------------------
rm(i, k, MischungProJahr)

## OUTPUT ----------------------------------------------------------------------
# two plots about mixture



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

