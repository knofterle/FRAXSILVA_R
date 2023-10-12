#============================ BASIC OVERVIEW ==================================#
# J.Osewold
# 11.05.22
# COPY FROM OLDER CODE
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/1000ETS/load_data_1000.R")
# 
## NOTES -----------------------------------------------------------------------
# Das sind lauter Diagramme die alle nicht wirklich eine Aussage haben, aber 
# zumindest funktionieren. Die stammen noch aus der explorativen Phase.

## ETS-STUFEN PRO JAHR --------------------------------------------------------

pdf(file = "EXPORT/1000ETS/FIGURES/ETS Stufen pro Jahr.pdf",
    width=10, height=5, paper='special')
boxplot(ETSStufen_1000_clean, names = c(as.character(2013:2022)), 
        main = "ETS Stufen pro Jahr", ylab = "ETS-Stufe", 
        xlab = "Jahr der Aufnahme")
dev.off()

## ETS-STUFEN VERAENDERUNG PRO JAHR ----------------------------------------

rows <- nrow(data_1000) 
Stufenveraenderung <- matrix(nrow = rows, ncol = 8)

for (i in 1:8) {
  Stufenveraenderung[ , i] <- 
    ETSStufen_1000_clean[[i]] - ETSStufen_1000_clean[[i+1]]
}

pdf(file = "EXPORT/1000ETS/FIGURES/ETS-Stufenveraenderung pro Jahr.pdf",
    width=10, height=5, paper='special')
boxplot(Stufenveraenderung, main = "ETS-Stufenveraenderung pro Jahr", 
        names = c(as.character(2014:2021)), ylab = "ETS-Stufen Veraenderung")
dev.off()

rm(rows, Stufenveraenderung, i)

## ZUWACHS PRO JAHR-------------------------------------------------------------

rows <- nrow(Durchmesser_1000)
Zuwachs <- matrix(nrow = rows, ncol = 7)
for (i in 1:6) {
  Zuwachs[ , i] <- (Durchmesser_1000[ , i+1]-Durchmesser_1000[ , i])
}
colnames (Zuwachs) <- c("2015", "2016", "2017", "2018", "2019", "2020", "2021")

pdf(file = "EXPORT/1000ETS/FIGURES/Durchmesserzuwachs pro Jahr.pdf",
    width=10, height=5, paper='special')
boxplot (Zuwachs, main = "Durchmesserzuwachs pro Jahr", 
         ylab = "Durchmesserzuwachs (mm)")
dev.off()

rm(rows, Zuwachs, i)


## ANZAHL UND ALTER PRO BESTAND ------------------------------------------------

Flaechenuebersicht <- data_1000[,c("edv_id", "alt_ets13")]
Flaechenuebersicht$alt_ets13 <- Flaechenuebersicht$alt_ets13 + 8
colnames(Flaechenuebersicht)[2] <- "Alter 2021"
Flaechenuebersicht <- unique(Flaechenuebersicht)

EtsStufenEdvid <- ETSStufen_1000_clean
EtsStufenEdvid$edvid <- data_1000$edv_id
EtsStufenEdvid

count <- function(x) sum(!is.na(x))
AnzahlBestand <- by(EtsStufenEdvid$`2021`, EtsStufenEdvid$edvid, count)
AnzahlBestand <- as.vector(AnzahlBestand)
AnzahlBestand <- data.frame(edv_id = levels(as.factor(data_1000$edv_id)),
                            Anzahl = AnzahlBestand)

Flaechenuebersicht <- merge(Flaechenuebersicht, AnzahlBestand, by = "edv_id")



## TIDY UP  --------------------------------------------------------------------
rm(AnzahlBestand)

## OUTPUT ----------------------------------------------------------------------
# Flaechenuebersicht



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





