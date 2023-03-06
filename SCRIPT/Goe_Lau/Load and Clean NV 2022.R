################ LOAD AND CLEAN NV 2022 ################################
# J.Osewold
# 01.07.22
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
unique(nv_2022$Plotnummer)
unique(nv_2022$Gefunden)


# plotnumbers complete?
which(!c(1:301) %in% unique(nv_2022$Plotnummer)) # 292 are missing, 292 wegen Rand
which(!c(8901:9017) %in% unique(nv_2022$Plotnummer)) # complete
which(!c(8701:8817) %in% unique(nv_2022$Plotnummer)) 
# 8816 and 8817 are missing


# sapply(nv_2021, table)

###### HARMONIZE ALL SPECIES NAMES  --------------------------------------------
unique(nv_2022$Baumart)
nv_2022$Baumart[nv_2022$Baumart %in% c("Rbu", "RBU", "rbu")] <- "RBu"
nv_2022$Baumart[nv_2022$Baumart %in% c("Esch", "esch")] <- "GEs"
nv_2022$Baumart[nv_2022$Baumart %in% c("BAH", "Bah")] <- "BAh"
nv_2022$Baumart[nv_2022$Baumart %in% c("Hbu", "hbu")] <- "HBu"
nv_2022$Baumart[nv_2022$Baumart %in% c("WLI", "Wli", "wli")] <- "WLi"
nv_2022$Baumart[nv_2022$Baumart %in% c("SAH", "Sah", "sah")] <- "SAh"
nv_2022$Baumart[nv_2022$Baumart %in% c("Salweide", "Weide", "SWei", "weide")] <- "SWe" 
nv_2022$Baumart[nv_2022$Baumart %in% c(" Eiche", "STi", " eiche")] <- "SEi" 
nv_2022$Baumart[nv_2022$Baumart %in% c("Keine Bäume", "keine Bäume", " ")] <- "" 
nv_2022$Baumart[nv_2022$Baumart %in% c("bah")] <- "BAh"
nv_2022$Baumart[nv_2022$Baumart %in% c("fah")] <- "FAh"
nv_2022$Baumart[nv_2022$Baumart %in% c("ulme berg", "Ulme Berg", "ulme")] <- "BUl"
nv_2022$Baumart[nv_2022$Baumart %in% c("wki")] <- "WKi"

nv_2022 <- rename(nv_2022, Baumart_kurz = Baumart)
colnames(nv_2022)

###### REMOVE SOME MORE TYPOS  -------------------------------------------------
unique(nv_2022$Esche.markiert)
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% c(" ", "")] <- NA
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% c(" SO", "SO ", "so", "So")] <- "SO"
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% 
											 	c("M", "Mitte", "mitte", "mitzte", "mittew")] <- "MITTE"
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% c("no", "No", "no ")] <- "NO"
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% c("sw")] <- "SW"
nv_2022$Esche.markiert[nv_2022$Esche.markiert %in% c("nw")] <- "NW"

nv_2022$Einjaehriger.Saemling[nv_2022$Einjaehriger.Saemling %in% 2] <- 1
nv_2022$Rueckegasse[nv_2022$Rueckegasse %in% c("t", "T")] <- 1
nv_2022$Rueckegasse[nv_2022$Rueckegasse %in% " "] <- ""


nv_2022$ETS.abgestorben.alt[nv_2022$ETS.abgestorben.alt %in% "T!"] <- "T1"

###### REMOVE DOUBLED PLOTS  ---------------------------------------------------
nv_2022 <- nv_2022[!(nv_2022$Bemerkungen %in% c("Werden erst im Herbst gemacht",
                                 "wird im Herbst nachgeholt#")),]

# One of both plots with the number "283" was a typo the plot actually had the 
# number "238", I identified it manually and address it with the first tree 
# entry, or simply: its the first occurrence
nv_2022$Plotnummer[nv_2022$Plotnummer %in% 283][1] <- 238

###### FILL ALL LINES  ---------------------------------------------------------
nv_2022$Rueckegasse[nv_2022$Rueckegasse %in% ""] <- NA
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

# table(nv$Rueckegasse)
nv_2022$Rueckegasse[nv_2022$Rueckegasse %in% c(1, "TRUE")] <- TRUE
nv_2022$Rueckegasse[is.na(nv_2022$Rueckegasse)] <- FALSE
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

for (i in c("ETS.abgestorben.frisch",
						"ETS.abgestorben.alt",
						"ETS.lebend",
						"Verbiss.lebend",
						"Verbiss.tot",
						"Sonstige.Gruende.tot")) {
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
nv_2022$Einjaehriger.Saemling <- as.logical(nv_2022$Einjaehriger.Saemling)
nv_2022$Einjaehriger.Saemling[is.na(nv_2022$Einjaehriger.Saemling)] <- F

###### ADD COLUMN ETS GENERAL ------------------------------------------------------
nv_2022$ETS <-
	!is.na(nv_2022$ETS.abgestorben.frisch) |
	!is.na(nv_2022$ETS.abgestorben.alt) |
	!is.na(nv_2022$ETS.lebend)

###### DELETE EMPTY PLOTS  -----------------------------------------------------
table(nv_2022$Baumart_kurz)
plotnumbers_with_empty <- unique(nv_2022$Plotnummer)

# The only information needed from the empty plots is the information about 
# logging trails. Therefore the data has to be saved for little more, but 
# separated to ease the counting of tree numbers
nv_with_empty <- nv_2022
nv_2022 <- nv_2022[nv_2022$Baumart_kurz != "", ]


###### TIDY UP  ----------------------------------------------------------------
rm(temp_plotnummer, temp_rueckegasse, 
    nv_polter, nv_lau,
   nv_ansitz, i)

###### OUTPUT ------------------------------------------------------------------
# nv
# plotnumbers_with_empty
# nv_with_empty




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

