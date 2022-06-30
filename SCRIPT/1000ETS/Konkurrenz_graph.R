#============================ TITLE ===========================================#
# J.Osewold
# 11.05.22
# NEW
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/1000ETS/Konkurrenz_data.R", echo = T)

## NOTES -----------------------------------------------------------------------


## GRAFISCHE UEBERSICHT KONK  --------------------------------------------------

# sum (KonkAlleEschen == 0, na.rm = T) / (1005*16) # 0.2075871
# sum (is.na(KonkAlleEschen))/ (1005*16) # 0.09166667
# #Summe 30% Ausschuss
# ETSStufenTemp <- as.matrix(ETSStufen)
# ETSStufenTemp [ETSStufenTemp == 8] <- NA
# ETSStufenTemp [ETSStufenTemp == 9] <- NA
# EtsMean <- apply(ETSStufenTemp,1, mean, na.rm = T)
# 
# TempKonk <-  as.matrix(KonkAlleEschen[,2:10])
# TempKonk[TempKonk == 0] <- NA
# TempKonk[TempKonk == -99] <- NA
# KonkMean <- apply(TempKonk,1, mean, na.rm = T)
# 
# pdf(file = "tables_and_figures/Zusammenhang ETS-Stufe und Konkurrenz.pdf",
#     width=12, height=8, paper='special')
# plot(KonkMean, EtsMean, xlab = "Mittlerer Konkurrenzindex (c66xy) pro Baum",
#      ylab = "Mittlere ETS-Stufe pro Baum", 
#      main = "Zusammenhang ETS-Stufe und Konkurrenz")
# dev.off()
# 
# rm(EtsMean, ETSStufenTemp, TempKonk, KonkMean)



## TIDY UP  --------------------------------------------------------------------
rm()

## OUTPUT ----------------------------------------------------------------------
# 



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

