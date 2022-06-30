### 10. Wie viele Individuen sind relativ tolerant? ----------------------------
# und welche

EtsStufenTemp <- EtsStufen
EtsStufenTemp[EtsStufenTemp == 8] <- NA
EtsStufenTemp[EtsStufenTemp == 9] <- NA
EtsStufenTemp$mean <- apply (EtsStufenTemp, 1, mean, na.rm = T)
TempMinimum <- which(EtsStufenTemp$mean == min(EtsStufenTemp$mean, na.rm = T))
EtsStufenTemp$ID <- ETSBaumIDgeneriert
EtsStufenTemp [TempMinimum, ]
plot(EtsStufenTemp$mean [order(EtsStufenTemp$mean)])
Plusbaeume <- EtsStufenTemp[EtsStufenTemp$mean <= 1.25, ]
Plusbaeume
write.csv(Plusbaeume, file = "tables_and_figures/Plusbaeume.csv")
rm(EtsStufenTemp, TempMinumum, Plusbaeume)