################ LOAD AND CLEAN NV DATA ################################
# J.Osewold
# 01.07.22
##### ALMOST DONE #####
################################################################################

###### LIBRARYS -------------------------------------------------------------------

###### REQUIRES -------------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here

###### NOTES -------------------------------------------------------------------
# I works like a charm with the data available at the 17.01.2022, and is updated 
# with the full dataset in June 22

###### CONNECT FILES  ----------------------------------------------------------
nv_polter <- read.csv(file = "DATA/RAW/Goe_Lau/NV Aufnahme/2021_Polter.csv",
                      header = T,
                      stringsAsFactors = F)
nv_ansitz <- read.csv(file = "DATA/RAW/Goe_Lau/NV Aufnahme/2021_Ansitz.csv",
                      header = T,
                      stringsAsFactors = F)
nv_lau <- read.csv(file = "DATA/RAW/Goe_Lau/NV Aufnahme/2021_Steinhorst.csv",
                          header = T,
                          stringsAsFactors = F)
nv <- rbind(nv_polter, nv_ansitz, nv_lau)

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
table(nv$Rueckegasse)
sort(table(nv$Plotnummer))

# plotnumbers complete?
which(!c(1:301) %in% unique(nv$Plotnummer)) # 179 180 292 298 are missing
which(!c(8901:9017) %in% unique(nv$Plotnummer)) # complete
which(!c(8701:8817) %in% unique(nv$Plotnummer)) 
# 8816 and 8817 are missing, I remember to have skipped them


# sapply(nv, table)

###### HARMONIZE ALL SPECIES NAMES  --------------------------------------------
unique(nv$Baumart)
nv$Baumart[nv$Baumart %in% c("Rbu", "RBU", "rbu")] <- "RBu"
nv$Baumart[nv$Baumart %in% c("Esch", "esch")] <- "GEs"
nv$Baumart[nv$Baumart %in% c("BAH", "Bah")] <- "BAh"
nv$Baumart[nv$Baumart %in% c("Hbu", "hbu")] <- "HBu"
nv$Baumart[nv$Baumart %in% c("WLI", "Wli", "wli")] <- "WLi"
nv$Baumart[nv$Baumart %in% c("SAH", "Sah", "sah")] <- "SAh"
nv$Baumart[nv$Baumart %in% c("Salweide", "Weide", "SWei", "weide")] <- "SWe" 
nv$Baumart[nv$Baumart %in% c(" Eiche", "STi", " eiche")] <- "SEi" 
nv$Baumart[nv$Baumart %in% c("Keine Bäume", "keine Bäume", " ")] <- "" 
nv$Baumart[nv$Baumart %in% c("bah")] <- "BAh"
nv$Baumart[nv$Baumart %in% c("fah")] <- "FAh"
nv$Baumart[nv$Baumart %in% c("ulme berg", "Ulme Berg", "ulme")] <- "BUl"
nv$Baumart[nv$Baumart %in% c("wki")] <- "WKi"

nv <- rename(nv, Baumart_kurz = Baumart)
colnames(nv)

###### REMOVE SOME MORE TYPOS  -------------------------------------------------
unique(nv$Esche.markiert)
nv$Esche.markiert[nv$Esche.markiert %in% c(" ", "")] <- NA
nv$Esche.markiert[nv$Esche.markiert %in% c(" SO", "SO ", "so", "So")] <- "SO"
nv$Esche.markiert[nv$Esche.markiert %in% c("M", "Mitte", "mitte")] <- "MITTE"
nv$Esche.markiert[nv$Esche.markiert %in% c("no", "No")] <- "NO"
nv$Esche.markiert[nv$Esche.markiert %in% c("sw")] <- "SW"
nv$Esche.markiert[nv$Esche.markiert %in% c("nw")] <- "NW"

nv$Einjaehriger.Saemling[nv$Einjaehriger.Saemling %in% 2] <- 1
nv$Rueckegasse[nv$Rueckegasse %in% "z"] <- 1

nv$ETS.abgestorben.alt[nv$ETS.abgestorben.alt %in% "T!"] <- "T1"

###### REMOVE DOUBLED PLOTS  ---------------------------------------------------
nv <- nv[!(nv$Bemerkungen %in% c("Werden erst im Herbst gemacht",
                                 "wird im Herbst nachgeholt#")),]

# One of both plots with the number "283" was a typo the plot actually had the 
# number "238", I identified it manually and address it with the first tree 
# entry, or simply: its the first occurrence
nv$Plotnummer[nv$Plotnummer %in% 283][1] <- 238

###### FILL ALL LINES  ---------------------------------------------------------
nv$Rueckegasse[nv$Rueckegasse %in% ""] <- NA
temp_plotnummer <- 0
temp_rueckegasse <- 0

# In the raw data each plot was described only in the first line of a plot,
# all following lines lack these information.
# Its important to only check for the emptiness of "plotnumber", because all plots
# that do not lie on a trail have a NA in that column just as all following 
# lines after a plot description

for (i in 1:nrow(nv)) {
  if (!is.na(nv$Plotnummer[i])) {
    temp_plotnummer <- nv$Plotnummer[i]
    temp_rueckegasse <- nv$Rueckegasse[i]
  } else {
    nv$Plotnummer[i] <- temp_plotnummer
    nv$Rueckegasse[i] <- temp_rueckegasse
  }
}

# table(nv$Rueckegasse)
nv$Rueckegasse[nv$Rueckegasse %in% c(1, "TRUE")] <- TRUE
nv$Rueckegasse[is.na(nv$Rueckegasse)] <- FALSE
nv$Rueckegasse <- as.logical(nv$Rueckegasse)

###### TERMINAL TRUE/FALSE --------------------------------------------------------
# create columns that represent the "t" mark for terminal in the number of 
# shoots
# The code was copied from the IBF part

nv$ETS.abgestorben.frisch.terminal <- FALSE
nv$ETS.abgestorben.alt.terminal <- FALSE
nv$ETS.lebend.terminal <- FALSE
nv$Verbiss.lebend.terminal <- FALSE
nv$Verbiss.tot.terminal <- FALSE
nv$Sonstige.Gruende.tot.terminal <- FALSE

for (i in c("ETS.abgestorben.frisch",
						"ETS.abgestorben.alt",
						"ETS.lebend",
						"Verbiss.lebend",
						"Verbiss.tot",
						"Sonstige.Gruende.tot")) {
	# Find all "t"s, the [[]] was neccessary to get a vector 
	select1 <- grep("t", nv[[i]])
	select2 <- grep("T", nv[[i]])
	select <- c(select1, select2) # These are positions not a TRUE/FALSE vector
	
	# write the information in the "terminal" column
	t <- paste0(i, ".terminal") 
	nv[select, t] <- TRUE
	# delete the "t"s
	nv[i] <- gsub("t", "", nv[[i]])
	nv[i] <- gsub("T", "", nv[[i]])
	# change to numeric
	nv[i] <- as.numeric(nv[[i]])
}

###### SET DATATYPE FOR COLUMNS  -----------------------------------------------
nv$Einjaehriger.Saemling <- as.logical(nv$Einjaehriger.Saemling)
nv$Einjaehriger.Saemling[is.na(nv$Einjaehriger.Saemling)] <- F

###### ADD COLUMN ETS GENERAL ------------------------------------------------------
nv$ETS <-
	!is.na(nv$ETS.abgestorben.frisch) |
	!is.na(nv$ETS.abgestorben.alt) |
	!is.na(nv$ETS.lebend)

###### DELETE EMPTY PLOTS  -----------------------------------------------------
table(nv$Baumart_kurz)
plotnumbers_with_empty <- unique(nv$Plotnummer)

# The only information needed from the empty plots is the information about 
# logging trails. Therefore the data has to be saved for little more, but 
# separated to ease the counting of tree numbers
nv_with_empty <- nv
nv <- nv[nv$Baumart_kurz != "", ]


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

