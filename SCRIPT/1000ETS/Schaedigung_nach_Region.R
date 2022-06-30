#============================ TITLE ===========================================#
# J.Osewold
# DATE
# STATUS
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/1000ETS/load_data_1000.R")

## NOTES -----------------------------------------------------------------------




## BEARBEITUNG DES DATENSATZES -------------------------------------------------

# Fuer jede Region wird eine eigene Matrix erstellt 
# und zu einer Liste kombiniert
EtsStufenRegio  <- matrix(data = NA, nrow = 6, ncol = 9)
rownames(EtsStufenRegio) <- c(as.character(seq(1, 5)), "Summe")
colnames(EtsStufenRegio) <- c(2013:2021) 
AnzEtsStufen <- list(EtsStufenRegio, EtsStufenRegio, EtsStufenRegio,
                     EtsStufenRegio, EtsStufenRegio, EtsStufenRegio, 
                     EtsStufenRegio, EtsStufenRegio) 
names(AnzEtsStufen) <- c("Goettingen", "Elm", "Lauenburg",
                         "Schotten", "Goettingen Ratio", 
                         "Elm Ratio", "Lauenburg Ratio",
                         "SChotten Ratio")


#  Im Folgenden bearbeitet die innere Schleife die ETS-Stufen und die
# Aeussere die Jahre, sum() dient wieder zum zaehlen aller TRUE und am Ende 
# folgen noch ein paar Schritte um von absolut auf ratio pro Jahr zu kommen

for (h in 1:4) {
  for (i in c(1:9)) {
    for (j in c(1:5)) {
      AnzEtsStufen[[h]][j, i] <- sum (ETSStufen_1000_clean[data_1000$region==h, i] == j, na.rm = T)
    }
    AnzEtsStufen[[h]][6, i] <- sum (AnzEtsStufen[[h]][1:5, i])
    AnzEtsStufen[[h+4]][6, i] <- AnzEtsStufen[[h]][6, i]
    AnzEtsStufen[[h+4]][1:5, i] <- AnzEtsStufen[[h]][1:5, i]/AnzEtsStufen[[h]][6, i]
  }
}

## DARSTELLUNG SCHAEDIGUNG NACH REGION UND JAHR -----------------------------

# RELATIVE WERTE, jaehrliche Verringerung des Samples nicht sichtbar

pdf(file = "EXPORT/1000ETS/FIGURES/Schaedigung nach Region_relativ.pdf",
    width=10, height=5, paper='special')

PlotLayout <- layout(matrix(c(1:36), 4, 9, byrow = TRUE), 
                     widths = c(5,3,3,3,3,3,3,3,3), 
                     height = c(4,4,4,5), TRUE)
LayoutFirstPlot <- c(2,5,3,1)
LayoutOtherPlot <- c(2,1,3,1)
layout.show(PlotLayout)
for (h in 1:4) {
  par(mar = LayoutFirstPlot)
  barplot(AnzEtsStufen[[h+4]][1:5,1], ylim = c(0,0.6), 
          main = names(AnzEtsStufen)[h], ylab = "Anteil relativ", xlab = "2013")
  par(mar = LayoutOtherPlot)
  for (i in c(2:9)) {
    barplot(AnzEtsStufen[[h+4]][1:5,i], axes = F, ylim = c(0,0.6), main = "", 
            xlab =  as.character(2012+i))
  }
}
dev.off()

# ABSOLUTE WERTE, jaehrliche Verringerung des Samples sichtbar
#  Hier nochmal das gleiche aber fuer die absoluten Werte, dadurch ist der die 
# Verkleinerung des Samples erkennbar, die Y-Achsen wurden fuer alle Plots einer 
# Region vereinheitlicht

pdf(file = "EXPORT/1000ETS/FIGURES/Schaedigung nach Region_absolut.pdf",
     width=10, height=5, paper='special')
PlotLayout <- layout(matrix(c(1:36),4,9,byrow = TRUE), 
                     widths = c(5,3,3,3,3,3,3,3,3), 
                     height = c(4,4,4,6), TRUE)
layout.show(PlotLayout)

for (h in 1:4) {
  par(mar = LayoutFirstPlot)
  barplot(AnzEtsStufen[[h]][1:5,1], ylim = c(0,160), 
          main = names(AnzEtsStufen)[h], ylab = "Anteil absolut", xlab = "2013")
  par(mar = LayoutOtherPlot)
  for (i in c(2:9)) {
    barplot(AnzEtsStufen[[h]][1:5,i], axes = F, ylim = c(0,150), main = "", 
            xlab =  as.character(2012+i))
  }
}
dev.off()

## TIDY UP  --------------------------------------------------------------------
rm(AnzEtsStufen, EtsStufenRegio, h, i, j, LayoutFirstPlot, LayoutOtherPlot, 
   PlotLayout)

## OUTPUT ----------------------------------------------------------------------
# two plots



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
