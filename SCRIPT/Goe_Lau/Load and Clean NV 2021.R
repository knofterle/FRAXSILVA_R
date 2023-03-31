################ LOAD AND CLEAN NV 2021 ################################
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
# I works like a charm with the data available at the 17.01.2022, and is updated 
# with the full dataset in June 22

###### CONNECT FILES  ----------------------------------------------------------
nv_polter <-	read.csv(
	file = "DATA/RAW/Goe_Lau/NV Aufnahme/2021_Polter.csv",
	header = T,
	stringsAsFactors = F,
	fileEncoding = "UTF-8"
)
nv_polter <- cbind(nv_polter, Flaeche = c("goe_pol"))
nv_ansitz <-	read.csv(
	file = "DATA/RAW/Goe_Lau/NV Aufnahme/2021_Ansitz.csv",
	header = T,
	stringsAsFactors = F,
	fileEncoding = "UTF-8"
)
nv_ansitz <- cbind(nv_ansitz, Flaeche = c("goe_ans"))
nv_lau <-	read.csv(
	file = "DATA/RAW/Goe_Lau/NV Aufnahme/2021_Steinhorst.csv",
	header = T,
	stringsAsFactors = F,
	fileEncoding = "UTF-8"
)
nv_lau <- cbind(nv_lau, Flaeche = c("lau"))

nv_2021 <- rbind(nv_polter, nv_ansitz, nv_lau)
nv_2021 <- cbind(nv_2021, Jahr = c(2021))
rm(nv_polter, nv_ansitz, nv_lau)

# # Replace column names/ I changed the names in the file according to the 
# naming in the IBF data
# # colnames(nv) <- c(
#             "plotnummer",
#             "rueckegasse",
#             "markierung_esche",
#             "baumart",
#             "hoehe",
#             "triebe_gesamt",
#             "saemling",
#             "triebe_ets_frisch",
#             "triebe_ets_alt",
#             "triebe_ets_lebend",
#             "triebe_verbissen_frisch",
#             "triebe_verbissen_alt",
#             "triebe_sonst_tot",
#             "blattflecken",
#             "bemerkungen")


###### CHECK FOR ERRORS  ---------------------------------------------------------
# check for mistakes in the raw data, visually complete or typos?
table(nv_2021$Rueckegasse)
sort(table(nv_2021$Plotnummer))

# plotnumbers complete?
which(!c(1:301) %in% unique(nv_2021$Plotnummer)) 
# No one is missing, I added them manually in the file
which(!c(8901:9017) %in% unique(nv_2021$Plotnummer)) # complete
which(!c(8701:8817) %in% unique(nv_2021$Plotnummer)) 
# 8816 and 8817 are missing, I remember to have skipped them, I will not mention
# them ever again


# sapply(nv_2021, table)

###### HARMONIZE ALL SPECIES NAMES  --------------------------------------------
unique(nv_2021$Baumart)
nv_2021$Baumart[nv_2021$Baumart %in% c("Rbu", "RBU", "rbu")] <- "RBu"
nv_2021$Baumart[nv_2021$Baumart %in% c("Esch", "esch")] <- "GEs"
nv_2021$Baumart[nv_2021$Baumart %in% c("BAH", "Bah")] <- "BAh"
nv_2021$Baumart[nv_2021$Baumart %in% c("Hbu", "hbu")] <- "HBu"
nv_2021$Baumart[nv_2021$Baumart %in% c("WLI", "Wli", "wli")] <- "WLi"
nv_2021$Baumart[nv_2021$Baumart %in% c("SAH", "Sah", "sah")] <- "SAh"
nv_2021$Baumart[nv_2021$Baumart %in% c("Salweide", "Weide", "SWei", "weide")] <- "SWe" 
nv_2021$Baumart[nv_2021$Baumart %in% c(" Eiche", "STi", " eiche")] <- "SEi" 
nv_2021$Baumart[nv_2021$Baumart %in% c("Keine Bäume", "keine Bäume")] <- "keine Bäume" 
nv_2021$Baumart[nv_2021$Baumart %in% c(" ")] <- "" 
nv_2021$Baumart[nv_2021$Baumart %in% c("bah")] <- "BAh"
nv_2021$Baumart[nv_2021$Baumart %in% c("fah")] <- "FAh"
nv_2021$Baumart[nv_2021$Baumart %in% c("ulme berg", "Ulme Berg", "ulme")] <- "BUl"
nv_2021$Baumart[nv_2021$Baumart %in% c("wki")] <- "WKi"

nv_2021 <- rename(nv_2021, Baumart_kurz = Baumart)
colnames(nv_2021)

###### REMOVE SOME MORE TYPOS  -------------------------------------------------
unique(nv_2021$Esche.markiert)
nv_2021$Esche.markiert[nv_2021$Esche.markiert %in% c(" ", "")] <- NA
nv_2021$Esche.markiert[nv_2021$Esche.markiert %in% c(" SO", "SO ", "so", "So")] <- "SO"
nv_2021$Esche.markiert[nv_2021$Esche.markiert %in% c("M", "Mitte", "mitte")] <- "MITTE"
nv_2021$Esche.markiert[nv_2021$Esche.markiert %in% c("no", "No")] <- "NO"
nv_2021$Esche.markiert[nv_2021$Esche.markiert %in% c("sw")] <- "SW"
nv_2021$Esche.markiert[nv_2021$Esche.markiert %in% c("nw")] <- "NW"

nv_2021$Einjaehriger.Saemling[nv_2021$Einjaehriger.Saemling %in% 2] <- 1
nv_2021$Rueckegasse[nv_2021$Rueckegasse %in% "z"] <- 1

nv_2021$ETS.abgestorben.alt[nv_2021$ETS.abgestorben.alt %in% "T!"] <- "T1"

###### REMOVE DOUBLED PLOTS  ---------------------------------------------------
nv_2021 <- nv_2021[!(nv_2021$Bemerkungen %in% c("Werden erst im Herbst gemacht",
                                 "wird im Herbst nachgeholt#")),]

# One of both plots with the number "283" was a typo the plot actually had the 
# number "238", I identified it manually and address it with the first tree 
# entry, or simply: its the first occurrence
nv_2021$Plotnummer[nv_2021$Plotnummer %in% 283][1] <- 238

###### FILL ALL LINES  ---------------------------------------------------------
nv_2021$Rueckegasse[nv_2021$Rueckegasse %in% ""] <- NA
temp_plotnummer <- 0
temp_rueckegasse <- 0

# In the raw data each plot was described only in the first line of a plot,
# all following lines lack these information.
# Its important to only check for the emptiness of "plotnumber", because all plots
# that do not lie on a trail have a NA in that column just as all following 
# lines after a plot description

for (i in 1:nrow(nv_2021)) {
  if (!is.na(nv_2021$Plotnummer[i])) {
    temp_plotnummer <- nv_2021$Plotnummer[i]
    temp_rueckegasse <- nv_2021$Rueckegasse[i]
  } else {
    nv_2021$Plotnummer[i] <- temp_plotnummer
    nv_2021$Rueckegasse[i] <- temp_rueckegasse
  }
}

# table(nv$Rueckegasse)
nv_2021$Rueckegasse[nv_2021$Rueckegasse %in% c(1, "TRUE")] <- TRUE
nv_2021$Rueckegasse[is.na(nv_2021$Rueckegasse)] <- FALSE
nv_2021$Rueckegasse <- as.logical(nv_2021$Rueckegasse)

###### TERMINAL TRUE/FALSE --------------------------------------------------------
# create columns that represent the "t" mark for terminal in the number of 
# shoots
# The code was copied from the IBF part

nv_2021$ETS.abgestorben.frisch.terminal <- FALSE
nv_2021$ETS.abgestorben.alt.terminal <- FALSE
nv_2021$ETS.lebend.terminal <- FALSE
nv_2021$Verbiss.lebend.terminal <- FALSE
nv_2021$Verbiss.tot.terminal <- FALSE
nv_2021$Sonstige.Gruende.tot.terminal <- FALSE

for (i in c("ETS.abgestorben.frisch",
						"ETS.abgestorben.alt",
						"ETS.lebend",
						"Verbiss.lebend",
						"Verbiss.tot",
						"Sonstige.Gruende.tot")) {
	# Find all "t"s, the [[]] was neccessary to get a vector 
	select1 <- grep("t", nv_2021[[i]])
	select2 <- grep("T", nv_2021[[i]])
	select <- c(select1, select2) # These are positions not a TRUE/FALSE vector
	
	# write the information in the "terminal" column
	t <- paste0(i, ".terminal") 
	nv_2021[select, t] <- TRUE
	# delete the "t"s
	nv_2021[i] <- gsub("t", "", nv_2021[[i]])
	nv_2021[i] <- gsub("T", "", nv_2021[[i]])
	# change to numeric
	nv_2021[i] <- as.numeric(nv_2021[[i]])
}

## KATEGORIEN AUS DEN KOMMENTAREN ----------------------------------------------
# Die Vorgehensweise ist ähnlich wie bei den IBF Daten, ich generiere aus den 
# Kommentaren Kategorien T/F und hänge sie an die Tabelle an.

nv_comments_raw <- unique(nv_2021$Bemerkungen)
write.csv(nv_comments_raw, file = "DATA/PROCESSED/GoeLau/comments_raw.csv",
					fileEncoding = "UTF-8")

comments_nv_proc <-
	read.csv(file = "DATA/PROCESSED/GoeLau/comments_proc.csv",
					 stringsAsFactors = F,
					 fileEncoding = "UTF-8")
comments_nv_proc <- subset(comments_nv_proc, select = -c(X)) # drop the "X" column
colnames(comments_nv_proc)
rowcheck_comm <- nrow(nv_2021)

# Beim Mergen wird die Reihenfolge der Bäume komplett durcheinander geworfen,
# die ist aber später noch relevant um die 1. Kommentare des Plots (die jeweils
# für den ganzen Plot gelten) heraus zu fischen. Daher wird hier die Reihenfolge
# erst gespeichert und dann wieder hergestellt.
nv_2021$Reihenfolge <- 1:nrow(nv_2021)
nv_2021 <- merge(
	nv_2021,
	comments_nv_proc,
	by.x = "Bemerkungen",
	by.y = "Bemerkungen",
	all.x = T,
	all.y = T
)
(rowcheck_comm <- rowcheck_comm == nrow(nv_2021))

nv_2021 <- nv_2021[order(nv_2021$Reihenfolge), ]


###### SET DATATYPE FOR COLUMNS  -----------------------------------------------
nv_2021$Einjaehriger.Saemling <- as.logical(nv_2021$Einjaehriger.Saemling)
nv_2021$Einjaehriger.Saemling[is.na(nv_2021$Einjaehriger.Saemling)] <- F

###### REPLACE SOME NAs  -----------------------------------------------

# Mit diesem Schnipsel werden die NAs aus den Spalten zum Thema Triebanzahl durch 
# nullen ersetzt, das erleichtert das Rechnen später ungemein.
selection <- !is.na(nv_2021$Anzahl.Triebe)
columns <- which(
	colnames(nv_2021) %in% c(
		"ETS.abgestorben.frisch",
		"ETS.abgestorben.alt",
		"ETS.lebend",
		"Verbiss.lebend",
		"Verbiss.tot",
		"Sonstige.Gruende.tot" # ,
#		"Aceria.fraxinivora"
	)
)
for (i in columns) {
	tmp <- nv_2021[selection, i]
	tmp[is.na(tmp)] <- 0
	nv_2021[selection, i] <- tmp
}

###### ADD COLUMN ETS GENERAL ------------------------------------------------------
nv_2021$ETS <-
	(nv_2021$ETS.abgestorben.frisch != 0) |
	(nv_2021$ETS.abgestorben.alt != 0) |
	(nv_2021$ETS.lebend != 0)

###### ADD LEBENDE TRIEBE  -----------------------------------------------------
nv_2021$Triebe.lebend <-
	nv_2021$Anzahl.Triebe - 
	nv_2021$ETS.abgestorben.frisch-
	nv_2021$ETS.abgestorben.alt -
	nv_2021$Verbiss.tot-
	nv_2021$Sonstige.Gruende.tot


###### DELETE EMPTY PLOTS  -----------------------------------------------------

plotnumbers_with_empty_2021 <- unique(nv_2021$Plotnummer)


# The only information needed from the empty plots is the information about 
# logging trails. Therefore the data has to be saved for little more, but 
# separated to ease the counting of tree numbers
nv_2021_with_empty <- nv_2021
nv_2021 <-nv_2021[!nv_2021$Baumart_kurz %in% c("", "keine Bäume"), ]
unique(nv_2021$Baumart_kurz)

###### TIDY UP  ----------------------------------------------------------------
rm(temp_plotnummer, temp_rueckegasse, 
   i, select, select1, select2, t, comments_nv_proc, nv_comments_raw, 
	 rowcheck_comm)

###### OUTPUT ------------------------------------------------------------------
# nv_2021
# plotnumbers_with_empty_2021
# nv_2021_with_empty




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

