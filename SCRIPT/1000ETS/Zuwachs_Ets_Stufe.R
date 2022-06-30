### 8. Zuwachs und ETS Stufe -------------------------------------------------

Zuwachs <- matrix(nrow = nrow(Durchmesser), ncol = 6)
for (i in 1:6) {
  Zuwachs[ , i] <- (Durchmesser[ , i+1]-Durchmesser[ , i])
}
colnames (Zuwachs) <- c("2015", "2016", "2017", "2018", "2019", "2020")
selection <- data$Durchmesser.problematisch 
selection[is.na(selection)] <- FALSE
EtsStufen999 <- EtsStufen[!selection, ]
EtsStufen999 <- as.matrix(EtsStufen999)
rm(selection)
EtsStufen999[EtsStufen999 == 8] <- NA
EtsStufen999[EtsStufen999 == 9] <- NA
EtsMean <- apply(EtsStufen999,1, mean, na.rm = T)
ZuwachsMean <- apply(Zuwachs, 1, mean, na.rm = T)

pdf(file = "tables_and_figures/Zusammenhang ETS-Stufe und Zuwachs pro Baum.pdf",
    width=10, height=5, paper='special')
#par("mar" = c(4,4,1.5,1))
plot(EtsMean, ZuwachsMean, main = "Zusammenhang ETS-Stufe und Zuwachs pro Baum",
     ylab = "Mittlerer Zuwachs 2013-2020", xlab = "Mittlere ETS-Stufe 2013-2020")
par(opar)
dev.off()

rm(EtsMean, ZuwachsMean, EtsStufen999)