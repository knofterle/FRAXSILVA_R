### 7. Zusammenhang zwischen Nekrose und ETS-Stufe ---------------------------

EtsStufen_18_20 <- unname(unlist(data[ , c("ets_stufe_18", "ets_stufe_19", 
                                           "ets_stufe_20")]))
EtsStufen_18_20[EtsStufen_18_20 == 9] <- NA
EtsStufen_18_20[EtsStufen_18_20 == 8] <- NA
UmfangNekrose    <- unname(unlist(data[ , c("U.Nek..", "U.Nek...1", 
                                            "U.Nek...2")]))
length(UmfangNekrose) == length(EtsStufen_18_20)

pdf(file = "tables_and_figures/Verteilung der Nekrosengroessen zu den ETS-Stufen.pdf",
    width=10, height=5, paper='special')
plot(jitter(EtsStufen_18_20, 1), jitter(UmfangNekrose,2),  
     pch = 16, col="#00000050", 
     ylab = "Umfang der Nekrose (%)", xlab = "ETS-Stufen", 
     main = "Verteilung der Nekrosengroessen zu den ETS-Stufen")
dev.off()
rm(EtsStufen_18_20, UmfangNekrose)


nekrose <-  data.frame(data$baum_id ,data$ets_stufe_18, data$ets_stufe_19, 
                       data$ets_stufe_20, data$Nek_N,data$Nek_N.1, data$Nek_N.2, 
                       data$U.Nek.., data$U.Nek...1, data$U.Nek...2,
                       data$Hmax.Nek, data$Hmax.Nek.1, data$Hmax.Nek.2)
colnames(nekrose) <- c("baum_id", "ets_stufe_18", "ets_stufe_19", "ets_stufe_20", 
                       "Nek_18", "Nek_19", "Nek_20", "U_Nek_18", "U_Nek_19", 
                       "U_Nek_20", "Hmax_Nek_18", "Hmax_Nek_19", "Hmax_Nek_20")

nekrose$Dif_18_19 <- nekrose$Nek_19 - nekrose$Nek_18 # Veraenderungen in der ETS
nekrose$Dif_19_20 <- nekrose$Nek_20 - nekrose$Nek_19 # Stufe werden berechnet

pdf(file = "tables_and_figures/Veraenderungen der Nekrosen.pdf",
    width=10, height=5, paper='special')
par(mfrow=c(1,2))
hist(nekrose$Dif_18_19, ylim = c(0,330), xlab = "Zusaetzliche Nekrosen 2019",
     main = "")
hist(nekrose$Dif_19_20, ylim = c(0,330), xlab = "Zusaetzliche Nekrosen 2020",
     main = "")
dev.off()

nekrose$keineNekrose_18 <- c(is.na(nekrose$Nek_18) | nekrose$Nek_18 %in% 0)
nekrose$keineNekrose_19 <- c(is.na(nekrose$Nek_19) | nekrose$Nek_19 %in% 0)
nekrose$keineNekrose_20 <- c(is.na(nekrose$Nek_20) | nekrose$Nek_20 %in% 0)
nekrose$NEKROSE_18_20 <- c(!nekrose$keineNekrose_18 | !nekrose$keineNekrose_19 |
                             !nekrose$keineNekrose_20)
#  In dieser Zeile wurden alle Bäume mit TRUE markiert, die irgendwann in 2018-
# 2020 eine sichtbare Nekrose hatten, einmal reicht.

nekrose <- nekrose[,-(16:18)] # Die drei Spalten "keine Nekrose 20XX" sind nun
# obsolet und werden gelöscht 

ets_stufe_1820       <- data.frame(data$ets_stufe_18, data$ets_stufe_19, 
                                   data$ets_stufe_20)
ets_stufe_1820[ets_stufe_1820 == 8] <- 5  #  8 stand für entnommen, 9 für nicht 
ets_stufe_1820[ets_stufe_1820 == 9] <- NA # einsehbar, beide mussten sinnvoll 
# ersetzt werden.
nekrose$ets_mean1820 <- rowMeans(ets_stufe_1820 , na.rm=T)
rm (ets_stufe_1820) # Der Mittelwert aller ETS Stufen zwischen 2018 und 2020
# wird gebildet

par(mfrow=c(1,1))
pdf(file = "tables_and_figures/Nekrose zu ETS-Stufe.pdf",
    width=10, height=5, paper='special')
boxplot(nekrose$ets_mean1820 ~ nekrose$NEKROSE_18_20, xlab = "Nekrose vorhanden",
        ylab = "Mittelwerte der ETS Stufen 2018-2020", )
dev.off()

hist(nekrose$ets_mean1820, xlab = "Mittelwerte der ETS Stufen 2018-2020", 
     main = "Histogramm der ETS-Stufen 2018-2020")

pdf(file = "tables_and_figures/Vergleich mit-ohne Stammfussnekrose_ETS-Stufe.pdf",
    width=10, height=10, paper='special')
par(mfrow=c(2,1))
hist(nekrose$ets_mean1820[nekrose$NEKROSE_18_20 == T], 
     xlab = "Mittelwerte der ETS Stufen 2018-2020", 
     main = "Eschen mit Stammfußnekrose", ylim = c(0,100) )
hist(nekrose$ets_mean1820[nekrose$NEKROSE_18_20 == F], 
     xlab = "Mittelwerte der ETS Stufen 2018-2020", 
     main = "Eschen ohne Stammfußnekrose", ylim = c(0,100) )
par(mfrow=c(1,1))
dev.off()

shapiro.test(nekrose$ets_mean1820) # p= 3.986e-16, nicht normalverteilt
qqnorm(nekrose$ets_mean1820) # dito
wilcox.test(nekrose$ets_mean1820 ~ nekrose$NEKROSE_18_20) # signifikanter 
# Unterschied
