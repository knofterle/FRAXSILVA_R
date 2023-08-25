#============================ TITLE ===========================================#
# J.Osewold
# DATE
# STATUS
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/IBF/LOAD_DATA_TREE.R", encoding = "UTF-8")

## LIBRARYS --------------------------------------------------------------------
library(dplyr)

## NOTES -----------------------------------------------------------------------
# Ich brauchte für die Tabelle im Abstract für den Tagungsband die ungefähren
# Mitteldurchmesser der Eschen der Flächen. Ich habe mich dann für den Grund-
# flächenmittelstamm entschieden.

## mean statt Grundflächenmittelstamm  -----------------------------------------

# Das ist erstmal ein Versuch nur mit dem mean()
data_tree_ibf %>% 
	filter(id_baumart == 120) %>% 
	group_by(liegt_in_kernflaeche) %>% # liegt_in_kernfläche gibt die IBF IDs aus
	summarise(mean(BHD_CM))

# Hier das ganze nochmal manuell um zu sehen ob die Berechnung richtig ist:
tmp <- data_tree_ibf %>% 
	filter(id_baumart == 120)  %>% 
	filter(liegt_in_kernflaeche == "BB_1") %>% 
	select(BHD_CM)
mean(tmp$BHD_CM)
# Ja das ist korrekt. Wir können weiter machen.

## Grundflächenmittelstamm  -----------------------------------------

# Jetzt der Grundflächenmittelstamm, das Ergebnis ist der Durchmesser
data_tree_ibf %>% 
	filter(id_baumart == 120) %>% 
	group_by(liegt_in_kernflaeche) %>% 
	summarise(sqrt(mean((BHD_CM/2)^2 * pi) / pi) * 2)

# Die Daten habe ich dann abgetippt.

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

