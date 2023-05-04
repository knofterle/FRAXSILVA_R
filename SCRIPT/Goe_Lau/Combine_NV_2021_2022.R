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

source(file = "SCRIPT/Goe_Lau/GENERATE_PLOTDISTRIBUTION.R")
# plots_pos_goelau
 
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

## ADD PLOTPOSITIONS TO NV_PLOTS -----------------------------------------------

plots_pos_goelau <- rename(.data = plots_pos_goelau, Plotnummer = nr)

nv_plots <- left_join(x = nv_plots, y = plots_pos_goelau, by = "Plotnummer")

### EXPORT ---------------------------------------------------------------------
write.csv(nv_plots, file = "EXPORT/Goe_Lau/tables/nv_plots.csv", 
					fileEncoding = "UTF-8")

## MARKED ASHES ----------------------------------------------------------------
#  Das Ziel ist eine Tabelle mit den Spalten: ID, Plot,  Quadrant, Plotposition X,
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
			"ETS",
			"mehr.ressourcen",
			"Triebe.lebend"
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
table(nv_marked_2022$Baumart_kurz) # Die leeren Baumartenfelder sind gefunden==F
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
			"ETS",
			"mehr.ressourcen",
			"Triebe.lebend"
		)
	))
names(nv_marked_2022)
# Kontrolle ob 2022 manche quadranten doppelt waren. Manuell korrigiert.
tmp_doubl <- nv_marked_2022 %>% 
	group_by(ID) %>% 
	filter(n()>1)
tmp_doubl

### ADD COLUMN "DEATH" ---------------------------------------------------------
# Ursprünglich war die Spalte "ToT" hier eingebaut worden, aber ich habe 
# vergessen die Kommentare aus 2022 überhaupt zu sortieren. Daher habe ich 
# nun analog zu 2021 eine Spalte "ToT" bereits im Script "load and clean" 
# eingebaut.Dieser Teil wurde dadurch stark verkürzt.  

unique(nv_marked_2022$Bemerkungen_22)
str(nv_marked_2022)


## Die Entscheidende Frage bei den toten ist, ob es noch mehr Bäume gibt die 
## vielleicht ganz tot sein könnten. Eigentlich ist das Merkmal ja, dass alle
## Triebe die bei "Anzahl Triebe" stehen auch irgendwie tot sind
# Auch das hier wurde nach "load and clean" verschoben

# nv_marked_2022$Triebe.lebend <- 
# 	nv_marked_2022$Anzahl.Triebe_22 - 
# 	nv_marked_2022$ETS.abgestorben.frisch_22-
# 	nv_marked_2022$ETS.abgestorben.alt_22 -
# 	nv_marked_2022$Verbiss.tot_22-
# 	nv_marked_2022$Sonstige.Gruende.tot_22

tmp <- nv_marked_2022 %>% 
	filter(Triebe.lebend == 0)
write.csv(tmp, file = "TEMP/nv_marked_tot.csv", fileEncoding = "UTF-8")
	
	


### MERGE AND GEFUNDEN? ----------------------------------------------------------

nv_marked <- full_join(x = nv_marked_2021, y = nv_marked_2022, by = "ID")
count(nv_marked) # 1497
count(nv_marked_2021) # 1491
count(nv_marked_2022) # 1471
# 26 Bäume passen nicht so richtig, Dreck! 
nv_marked_test <- anti_join(x = nv_marked_2022, y = nv_marked_2021, by = "ID")
View(nv_marked_test)
nv_marked_test2 <- anti_join(x = nv_marked_2021, y = nv_marked_2022, by = "ID")
View(nv_marked_test2)


### EXPORT   ----------------------------------------------------------

write.csv(nv_marked, file = "EXPORT/Goe_Lau/tables/nv_marked.csv", 
					fileEncoding = "UTF-8")


## TIDY UP  --------------------------------------------------------------------
rm(tmp_doubl)

## OUTPUT ----------------------------------------------------------------------
# nv_marked
# nv_marked.csv
# 
# nv_marked_2022
# nv_marked_2021
# 
# nv_plots



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

