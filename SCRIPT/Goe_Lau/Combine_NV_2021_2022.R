#===================== Combine NV 2021 and 2022 ===============================#
# J.Osewold
# 10.03.2023
# NEW
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/Goe_Lau/Aggregated tables NV_2021.R", encoding = "UTF-8")
source(file = "SCRIPT/Goe_Lau/Aggregated tables NV_2022.R", encoding = "UTF-8")

# nv_2021 and 2022
# nv_2021_plots and 2022
# nv_2021_with_empty and 2022
 
## LIBRARYS --------------------------------------------------------------------
require(dplyr)

## NOTES -----------------------------------------------------------------------


## CHANGE COLNAMES AND JOIN ----------------------------------------------------
names(nv_2021_plots) <- paste0(names(nv_2021_plots), "_2021")
names(nv_2022_plots) <- paste0(names(nv_2022_plots), "_2022")
names(nv_2021_plots)[1] <- "Plotnummer"
names(nv_2022_plots)[1] <- "Plotnummer"

nv_plots <-
	inner_join(x = nv_2021_plots,
						 y = nv_2022_plots,
						 by = "Plotnummer")
names(nv_plots)


## MARKED ASHES ----------------------------------------------------------------
# Das Ziel ist eine Tabelle mit den Spalten: ID, Plot,  Quadrant, Plotposition X,
#  und Y, Flaeche, Rueckegasse, 2021_Hoehe und so weiter, 2022_gefunden, 
#  2022_tot, 2022_Hoehe und so weiter mit eventuell NA. Wobei die Koordinaten 
#  erst später dazu kommen.
#  
#  Es gibt ein paar Eschen die 2022 mit Band gefunden wurden, die aber nicht im 
#  Datensatz 2021 auftauchen, diese werden dann beim mergen gedroppt.
#  Andersherum gibt es auch Eschen die 2022 nicht im Datensatz auftauchen, weder 
#  als nicht gefunden noch sonst irgendwie, die werden dann wohl als NA bei gefunden
#  und auch sonst überall mit NA eingetragen.
#  
#  Die nicht gefundenen Eschen wurden bei Load and Clean aussortiert, da sie
#  ja eigentlich 2022 keine Bäume mehr sind. Daher muss hier mit nv_with_empty 
#  gearbeitet werden.

nv_marked_2021 <- nv_2021_with_empty %>% 
	filter(!is.na(Esche.markiert))
nv_marked_2021$ID <- paste0(nv_marked_2021$Plotnummer, nv_marked_2021$Esche.markiert)
names(nv_marked_2021)
table(nv_marked_2021$Baumart_kurz)
filter(nv_marked_2021, Baumart_kurz == "RBu")
nv_marked_2021 <- nv_marked_2021 %>% 
	select(!c(Baumart_kurz, Jahr, Ausgeschlossen.Rand.Zaun)) %>% 
	rename_with(.fn = ~ paste0(., "_21"), .cols = all_of(
		c("Hoehe",
			"Anzahl.Triebe",
			"Einjaehriger.Saemling",
			"ETS.abgestorben.frisch",
			"ETS.abgestorben.alt",
			"ETS.lebend",
			"Verbiss.lebend",
			"Verbiss.tot",
			"Sonstige.Gruende.tot",
			"Bemerkungen",
			"ETS.abgestorben.frisch.terminal",
			"ETS.abgestorben.alt.terminal",
			"ETS.lebend.terminal",
			"Verbiss.lebend.terminal",
			"Verbiss.tot.terminal",
			"Sonstige.Gruende.tot.terminal",
			"Rueckegasse",
			"Blattflecken",
			"ETS"
		)
	))

# Kontrolle ob 2021 manche quadranten doppelt waren. Manuell korrigiert.
tmp_doubl <- nv_marked_2021 %>% 
	group_by(ID) %>% 
	filter(n()>1)
tmp_doubl

nv_marked_2022 <- nv_2022_with_empty %>% 
	filter(!is.na(Esche.markiert))
nv_marked_2022$ID <- paste0(nv_marked_2022$Plotnummer, nv_marked_2022$Esche.markiert)
table(nv_marked_2022$Baumart_kurz)
nv_marked_2022 <- nv_marked_2022 %>% 
	select(!c(Plotnummer, Esche.markiert, Baumart_kurz, Flaeche, Jahr, Rueckegasse)) %>% 
	rename_with(.fn = ~ paste0(., "_22"), .cols = all_of(
		c("Hoehe",
			"Anzahl.Triebe",
			"Einjaehriger.Saemling",
			"ETS.abgestorben.frisch",
			"ETS.abgestorben.alt",
			"ETS.lebend",
			"Verbiss.lebend",
			"Verbiss.tot",
			"Sonstige.Gruende.tot",
			"Gruene.Hoehe",
			"Aceria.fraxinivora",
			"Nicht.gerade",
			"Bemerkungen",
			"ETS.abgestorben.frisch.terminal",
			"ETS.abgestorben.alt.terminal",
			"ETS.lebend.terminal",
			"Verbiss.lebend.terminal",
			"Verbiss.tot.terminal",
			"Sonstige.Gruende.tot.terminal",
			"Aceria.fraxinivora.terminal",
			"ETS"
		)
	))
names(nv_marked_2022)
# Kontrolle ob 2022 manche quadranten doppelt waren. Manuell korrigiert.
tmp_doubl <- nv_marked_2022 %>% 
	group_by(ID) %>% 
	filter(n()>1)
tmp_doubl

nv_marked <- left_join(x = nv_marked_2021, y = nv_marked_2022, by = "ID")
count(nv_marked)
count(nv_marked_2021)
count(nv_marked_2022)
write.csv(nv_marked, file = "EXPORT/Goe_Lau/tables/nv_marked.csv", 
					fileEncoding = "UTF-8")

## GRAPHS ----------------------------------------------------


nv_plots %>% 
	arrange(n_ash_2021) %>% 
	mutate(index = row_number()) %>% 
	ggplot(data = ., aes(x = index)) +
	geom_point(aes(y = n_ash_2021), color = "blue") +
	geom_smooth(aes(y = n_ash_2021), color = "blue") +
	geom_point(aes(y = n_ash_2022), color = "red") +
	geom_smooth(aes(y = n_ash_2022), color = "red")
	
nv_plots %>% 
	arrange(height_mean_2021) %>% 
	filter(height_mean_2021 < 80) %>% 
	mutate(index = row_number()) %>% 
	ggplot(data = ., aes(x = index)) +
	geom_point(aes(y = height_mean_2021), color = "blue") +
	geom_smooth(aes(y = height_mean_2021), color = "blue") +
	geom_point(aes(y = height_mean_2022), color = "red") +
	geom_smooth(aes(y = height_mean_2022), color = "red")

## TIDY UP  --------------------------------------------------------------------
rm(tmp_doubl, tmp)

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

