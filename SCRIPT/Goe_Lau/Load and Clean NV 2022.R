################ LOAD AND CLEAN NV 2022 ################################
# J.Osewold
# 07.03.2023
##### ALMOST DONE #####
################################################################################

###### LIBRARYS -------------------------------------------------------------------
library(dplyr)
###### REQUIRES -------------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here

###### NOTES -------------------------------------------------------------------
# Is similar to the script for 2021

###### CONNECT FILES  ----------------------------------------------------------
nv_polter_22 <- read.csv(file = "DATA/RAW/Goe_Lau/NV Aufnahme/2022_Goettingen_polter.csv",
												 header = T,
												 stringsAsFactors = F, 
												 fileEncoding = "UTF-8")
nv_polter_22 <- cbind(nv_polter_22, Flaeche = c("goe_pol"))
nv_ansitz_22 <- read.csv(file = "DATA/RAW/Goe_Lau/NV Aufnahme/2022_Goettingen_ansitz.csv",
												 header = T,
												 stringsAsFactors = F, 
												 fileEncoding = "UTF-8")
nv_ansitz_22 <- cbind(nv_ansitz_22, Flaeche = c("goe_ans"))
nv_lau_22 <- read.csv(file = "DATA/RAW/Goe_Lau/NV Aufnahme/2022_Steinhorst.csv",
											header = T,
											stringsAsFactors = F, 
											fileEncoding = "UTF-8")
nv_lau_22 <- cbind(nv_lau_22, Flaeche = c("lau"))
nv_2022 <- rbind(nv_polter_22, nv_ansitz_22, nv_lau_22)
nv_2022 <- cbind(nv_2022, Jahr = c(2022))
rm(nv_polter_22, nv_ansitz_22, nv_lau_22)


###### CHECK FOR ERRORS  ---------------------------------------------------------
# check for mistakes in the raw data, visually complete or typos?
colnames(nv_2022)
unique(nv_2022$Aceria.fraxinivora)

# plotnumbers complete?
which(!c(1:301) %in% unique(nv_2022$Plotnummer)) 
# 292 was missing, 292 wegen Rand ich habe den aber manuell hinzugefügt
which(!c(8901:9017) %in% unique(nv_2022$Plotnummer)) # complete
which(!c(8701:8817) %in% unique(nv_2022$Plotnummer)) 

#8816 and 8817 are missing but I will ignore them from now on


# sapply(nv_2021, table)

###### HARMONIZE ALL SPECIES NAMES  --------------------------------------------
unique(nv_2022$Baumart)
nv_2022$Baumart[nv_2022$Baumart %in% c("Rbu", "RBU", "rbu")] <- "RBu"
nv_2022$Baumart[nv_2022$Baumart %in% c("Esch", "esch", " esch")] <- "GEs"
nv_2022$Baumart[nv_2022$Baumart %in% c("BAH", "Bah")] <- "BAh"
nv_2022$Baumart[nv_2022$Baumart %in% c("Hbu", "hbu", "hainBu")] <- "HBu"
nv_2022$Baumart[nv_2022$Baumart %in% c("WLI", "Wli", "wli", "lindewi")] <- "WLi"
nv_2022$Baumart[nv_2022$Baumart %in% c("SAH", "Sah", "sah")] <- "SAh"
nv_2022$Baumart[nv_2022$Baumart %in% c("Salweide", "Weide", "SWei", "weide", 
																			 "WeideSa", "weidesa", "weide ")] <- "SaWei" 
nv_2022$Baumart[nv_2022$Baumart %in% c(" Eiche", "STi", " eiche", 
																			 "Que")] <- "Eiche" 
nv_2022$Baumart[nv_2022$Baumart %in% c("Keine Bäume", "keine Bäume",
																			 "keine bäume")] <- "keine Bäume" 
nv_2022$Baumart[nv_2022$Baumart %in% c(" ", "  ", "     ")] <- ""
nv_2022$Baumart[nv_2022$Baumart %in% c("bah")] <- "BAh"
nv_2022$Baumart[nv_2022$Baumart %in% c("fah")] <- "FAh"
nv_2022$Baumart[nv_2022$Baumart %in% c("ulme berg", "Ulme Berg", "ulme", 
																			 "ulmb", "ulmeb")] <- "BUl"
nv_2022$Baumart[nv_2022$Baumart %in% c("wki", "kirsche", "Kirsche")] <- "WKi"
nv_2022$Baumart[nv_2022$Baumart %in% c("traubeneiche", "que", "quercus", 
																			 "Quercus", "qzercus", "qzercus", 
																			 "tei")] <- "Eiche"
nv_2022$Baumart[nv_2022$Baumart %in% c("pappel")] <- "Pappel"
nv_2022$Baumart[nv_2022$Baumart %in% c(" birke")] <- "Birke"
nv_2022$Baumart[nv_2022$Baumart %in% c(" lärche")] <- "ELae"
nv_2022$Baumart[nv_2022$Baumart %in% c("weide ohr")] <- "OWei"
nv_2022$Baumart[nv_2022$Baumart %in% c("prunusserotina", "prunus serontina", 
																			 "Prunus serontina", 
																			 "prunus serontina")] <- "STrau"
nv_2022$Baumart[nv_2022$Baumart %in% c("felsenbirne")] <- "Felsenbirne"

nv_2022 <- rename(nv_2022, Baumart_kurz = Baumart)


###### REMOVE SOME MORE TYPOS  -------------------------------------------------
nv_2022$Anzahl.Triebe[nv_2022$Anzahl.Triebe %in% 
												"                                  "] <- NA
nv_2022$Anzahl.Triebe[nv_2022$Anzahl.Triebe %in% "1s"] <- "1"

nv_2022$Rueckegasse[nv_2022$Rueckegasse %in% c("t", "T")] <- TRUE
nv_2022$Rueckegasse[nv_2022$Rueckegasse %in% c(" ", "")] <- NA
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% c(" ", "")] <- NA
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% c(" SO", "SO ", "so", "So")] <- "SO"
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% 
											 	c("M", "Mitte", "mitte", "mitzte", "mittew")] <- "MITTE"
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% c("no", "No", "no ")] <- "NO"
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% c("sw")] <- "SW"
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% c("nw")] <- "NW"
nv_2022$Gefunden[nv_2022$Gefunden %in% c("t", "T", "t ", " t")] <- TRUE
nv_2022$Gefunden[nv_2022$Gefunden %in% c("f","f  ")] <- FALSE
nv_2022$Gefunden[nv_2022$Gefunden %in% c(""," ")] <- NA
nv_2022$Einjaehriger.Saemling[nv_2022$Einjaehriger.Saemling %in% c("", NA)] <- FALSE
nv_2022$Einjaehriger.Saemling[nv_2022$Einjaehriger.Saemling %in% c("t", "1")] <- TRUE
nv_2022$ETS.abgestorben.alt[nv_2022$ETS.abgestorben.alt %in% 
															"                                                         "] <- ""
nv_2022$Nicht.gerade[nv_2022$Nicht.gerade %in% c("x", "1", "t", "X", "x ")] <- TRUE
nv_2022$Nicht.gerade[nv_2022$Nicht.gerade %in% c("")] <- FALSE


###### FILL ALL LINES  ---------------------------------------------------------
temp_plotnummer <- 0
temp_rueckegasse <- 0

# In the raw data each plot was described only in the first line of a plot,
# all following lines lack these information.
# Its important to only check for the emptiness of "plotnumber", because all plots
# that do not lie on a trail have a NA in that column just as all following 
# lines after a plot description

for (i in 1:nrow(nv_2022)) {
  if (!is.na(nv_2022$Plotnummer[i])) {
    temp_plotnummer <- nv_2022$Plotnummer[i]
    temp_rueckegasse <- nv_2022$Rueckegasse[i]
  } else {
    nv_2022$Plotnummer[i] <- temp_plotnummer
    nv_2022$Rueckegasse[i] <- temp_rueckegasse
  }
}

nv_2022$Rueckegasse[is.na(nv_2022$Rueckegasse)] <- FALSE
table(nv_2022$Rueckegasse)
nv_2022$Rueckegasse <- as.logical(nv_2022$Rueckegasse)

###### TERMINAL TRUE/FALSE --------------------------------------------------------
# create columns that represent the "t" mark for terminal in the number of 
# shoots
# The code was copied from the IBF part

nv_2022$ETS.abgestorben.frisch.terminal <- FALSE
nv_2022$ETS.abgestorben.alt.terminal <- FALSE
nv_2022$ETS.lebend.terminal <- FALSE
nv_2022$Verbiss.lebend.terminal <- FALSE
nv_2022$Verbiss.tot.terminal <- FALSE
nv_2022$Sonstige.Gruende.tot.terminal <- FALSE
nv_2022$Aceria.fraxinivora.terminal <- FALSE

for (i in c("ETS.abgestorben.frisch",
						"ETS.abgestorben.alt",
						"ETS.lebend",
						"Verbiss.lebend",
						"Verbiss.tot",
						"Sonstige.Gruende.tot",
						"Aceria.fraxinivora")) {
	# Find all "t"s, the [[]] was neccessary to get a vector 
	select1 <- grep("t", nv_2022[[i]])
	select2 <- grep("T", nv_2022[[i]])
	select <- c(select1, select2) # These are positions not a TRUE/FALSE vector
	
	# write the information in the "terminal" column
	t <- paste0(i, ".terminal") 
	nv_2022[select, t] <- TRUE
	# delete the "t"s
	nv_2022[i] <- gsub("t", "", nv_2022[[i]])
	nv_2022[i] <- gsub("T", "", nv_2022[[i]])
	# change to numeric
	nv_2022[i] <- as.numeric(nv_2022[[i]])
}

###### SET DATATYPE FOR COLUMNS  -----------------------------------------------
nv_2022$Gefunden <- as.logical(nv_2022$Gefunden)
table(nv_2022$Gefunden)

nv_2022$Einjaehriger.Saemling <- as.logical(nv_2022$Einjaehriger.Saemling)
nv_2022$Einjaehriger.Saemling[is.na(nv_2022$Einjaehriger.Saemling)] <- F

nv_2022$Nicht.gerade <- as.logical(nv_2022$Nicht.gerade)

###### ADD COLUMN ETS GENERAL ------------------------------------------------------
nv_2022$ETS <-
	!is.na(nv_2022$ETS.abgestorben.frisch) |
	!is.na(nv_2022$ETS.abgestorben.alt) |
	!is.na(nv_2022$ETS.lebend)

###### ADD MISSING PLOTS  -----------------------------------------------------
# Diese Plots haben aus unterschiedlichen Gründen gefehlt, ich habe sie dann 
# ergänzt
# 292 Zaun
# 8816 RAND
# 8817 Rand
# aber manuell in der Tabelle (nur 292), der ist also schon drin
# standard_line <- nv_2022[3,]
# standard_line
# standard_line$Jahr <- 2022
# standard_line$Baumart_kurz <- ""
# standard_line$Hoehe <- NA
# 
# new_plots <- 
# 	standard_line %>% 
# 	slice(rep(1, each=3))
# 
# new_plots$Plotnummer <- c(292, 8816, 8817)
# new_plots$Bemerkungen <- c("Zaun", "RAND", "RAND")
# new_plots$Flaeche <- c("lau", "goe_ans", "goe_ans")
# new_plots$Baumart_kurz <- c("", "", "")
# new_plots
# 
# nv_2022 <- rbind(nv_2022,new_plots)


###### DELETE EMPTY PLOTS  -----------------------------------------------------
table(nv_2022$Baumart_kurz)
plotnumbers_with_empty_2022 <- unique(nv_2022$Plotnummer)

# The only information needed from the empty plots is the information about 
# logging trails. Therefore the data has to be saved for little more, but 
# separated to ease the counting of tree numbers
nv_2022_with_empty <- nv_2022
nv_2022 <- nv_2022[!nv_2022$Baumart_kurz %in% c("", "keine Bäume"), ]

# Die markierten Eschen die nicht wieder gefunden wurden bilden dennoch eine 
# Zeile in der Tabelle, die müssen zeitweise raus und später nochmal 
# verwendet werden.
nv_2022 <- nv_2022 %>% 
	filter(Gefunden == TRUE | is.na(Gefunden))

###### TIDY UP  ----------------------------------------------------------------
rm(temp_plotnummer, temp_rueckegasse, 
    i, select, select1, select2, t)

###### OUTPUT ------------------------------------------------------------------
# nv_2022
# plotnumbers_with_empty_2022
# nv_2022_with_empty




###### JUNK ------------------------------------------------------------------
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

