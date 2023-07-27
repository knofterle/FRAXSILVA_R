library(dplyr)

# Das ist erstmal ein Versuch nur mit dem mean()
data_tree_ibf %>% 
	filter(id_baumart == 120) %>% 
	group_by(liegt_in_kernflaeche) %>% 
	summarise(mean(BHD_CM))

# Hier das ganze nochmal manuell um zu sehen ob die Berechnung richtig ist:
tmp <- data_tree_ibf %>% 
	filter(id_baumart == 120)  %>% 
	filter(liegt_in_kernflaeche == "BB_1") %>% 
	select(BHD_CM)
mean(tmp$BHD_CM)
# Ja das ist korrekt. Wir können weiter machen.

# Jetzt der Grundflächenmittelstamm, das Ergebnis ist der Durchmesser
data_tree_ibf %>% 
	filter(id_baumart == 120) %>% 
	group_by(liegt_in_kernflaeche) %>% 
	summarise(sqrt(mean((BHD_CM/2)^2 * pi) / pi) * 2)



	group_by(liegt_in_kernflaeche) 
	summarise(mean(BHD_CM))
	
	str(data_tree_ibf)
table(data_tree_ibf$liegt_in_kernflaeche)



sqrt(mean((BHD_CM/2)^2 * pi) / pi) * 2