#============================ IBF LOAD NV DATA ================================#
# J.Osewold
# 19.04.22
# STATUS :: Close to done
#==============================================================================#

## LIBRARYS ----------------------------------------------------------------
library(dplyr)

## REQUIRES ----------------------------------------------------------------
# csv files of all sample areas called "Alle_AUfnahmen_*"
# csv file with categories to comments, called "Bemerkungen_bearbeitet"

## NOTES -------------------------------------------------------------------
# The first steps will be merging all tables from the NV-Assessment. 
# Some data cleaning steps will be similar to the script of Lau/Goe I am still 
# thinking if it makes sense to share some of the code...
# 


## LOAD AND RBIND CSV  -----------------------------------------------------
data_nv_scho <-
  read.csv(file = "DATA/RAW/Schotten/NV Aufnahme Schotten/Alle_Aufnahmen_scho.csv",
           stringsAsFactors = F, fileEncoding = "UTF-8")
data_nv_scho$Flaeche <- "Schotten"

data_nv_mol <-
  read.csv(file = "DATA/RAW/Mollenfelde/NV Aufnahme Mollenfelde/Alle_Aufnahmen_mol.csv",
           stringsAsFactors = F, fileEncoding = "UTF-8")
data_nv_mol$Flaeche <- "Mollenfelde"

data_nv_huy <-
  read.csv(file = "DATA/RAW/Huy/NV Aufnahme Huy/Alle_Aufnahmen_huy.csv",
           stringsAsFactors = F, fileEncoding = "UTF-8")
data_nv_huy$Flaeche <- "Huy"

data_nv_weis  <-
  read.csv(file = "DATA/RAW/Weisweil/NV Aufnahme Weisweil/Alle_Aufnahmen_weis.csv",
           stringsAsFactors = F, fileEncoding = "UTF-8")
data_nv_weis$Flaeche <- "Weisweil"

data_nv_platt <-
  read.csv(file = "DATA/RAW/Plattenwald/NV Aufnahme Plattenwald/Alle_Aufnahmen_platt.csv",
           stringsAsFactors = F, fileEncoding = "UTF-8")
data_nv_platt$Flaeche <- "Plattenwald"

data_nv <- rbind(data_nv_scho, data_nv_mol, data_nv_huy, data_nv_weis, 
                 data_nv_platt)

## TERMINAL TRUE/FALSE --------------------------------------------------------
# create columns that represent the "t" mark for terminal in the number of 
# shoots

data_nv$ETS.abgestorben.frisch.terminal <- FALSE
data_nv$ETS.abgestorben.alt.terminal <- FALSE
data_nv$ETS.lebend.terminal <- FALSE
data_nv$Verbiss.lebend.terminal <- FALSE
data_nv$Verbiss.tot.terminal <- FALSE
data_nv$Sonstige.Gruende.tot.terminal <- FALSE

for (i in c("ETS.abgestorben.frisch",
            "ETS.abgestorben.alt",
            "ETS.lebend",
            "Verbiss.lebend",
            "Verbiss.tot",
            "Sonstige.Gruende.tot")) {
  # Find all "t"s, the [[]] was neccessary to get a vector 
  select1 <- grep("t", data_nv[[i]])
  select2 <- grep("T", data_nv[[i]])
  select <- c(select1, select2) # These are positions not a TRUE/FALSE vector
  # write the information in the "terminal" column
  t <- paste0(i, ".terminal") 
  data_nv[select, t] <- TRUE
  # delete the "t"s
  data_nv[i] <- gsub("t", "", data_nv[[i]])
  data_nv[i] <- gsub("T", "", data_nv[[i]])
  # change to numeric
  data_nv[i] <- as.numeric(data_nv[[i]])
}

## DATA CLEANING  --------------------------------------------------------------

### eigentlich.aelter and height typos -----------------------------------------
# Create column that represents the marks of "na" and "1" in the height measures
# called "eigentlich aelter"
data_nv$eigentlich.aelter <- FALSE
data_nv$eigentlich.aelter[data_nv$Hoehe.Vorjahr %in% c("1", "na")] <- TRUE
data_nv$eigentlich.aelter[data_nv$Hoehe.Vorvorjahr %in% c("1", "na", "(40)")] <-
  TRUE

# remove typos to change character to numeric
data_nv$Hoehe.Vorjahr[data_nv$Hoehe.Vorjahr %in% c("1", "na")] <- ""
data_nv$Hoehe.Vorjahr[data_nv$Hoehe.Vorjahr %in% c("^4200")] <- "4200"
data_nv$Hoehe.Vorvorjahr[data_nv$Hoehe.Vorvorjahr %in% c("1", "na", "(40)")] <-
  ""
data_nv$Hoehe.Vorvorjahr[data_nv$Hoehe.Vorvorjahr %in% c("65 (11,5)")] <- "65"
data_nv$Hoehe <- as.numeric(data_nv$Hoehe)
data_nv$Hoehe.Vorjahr <- as.numeric(data_nv$Hoehe.Vorjahr)
data_nv$Hoehe.Vorvorjahr <- as.numeric(data_nv$Hoehe.Vorvorjahr)


### clean Quadrant --------------------------------------------------------------
data_nv$Quadrant[data_nv$Quadrant %in% c("nord", 
                                         "Nord", 
                                         "norden", 
                                         "Norden", 
                                         "Norden              ")] <- "nord"
data_nv$Quadrant[data_nv$Quadrant %in% c("no",
                                         "NO")]                   <- "nordost"
data_nv$Quadrant[data_nv$Quadrant %in% c("ost",
                                         "Ost",
                                         "osten",
                                         "Osten")]                <- "ost"
data_nv$Quadrant[data_nv$Quadrant %in% c("so",
                                         "SO")]                   <- "suedost"
data_nv$Quadrant[data_nv$Quadrant %in% c("süd",
                                         "Süd",
                                         "süden",
                                         "Süden",
                                         "sued")]                 <- "sued"
data_nv$Quadrant[data_nv$Quadrant %in% c("sw",
                                         "SW")]                   <- "suedwest"
data_nv$Quadrant[data_nv$Quadrant %in% c("west",
                                         "West",
                                         "westen",
                                         "Westen")]               <- "west"
data_nv$Quadrant[data_nv$Quadrant %in% c("nw",
                                         "NW")]                   <- "nordwest"
data_nv$Quadrant[data_nv$Quadrant %in% c(" ", "", "zaun")]        <- NA

### clean Rueckegasse --------------------------------------------------------------
data_nv$Rueckegasse[data_nv$Rueckegasse %in% c(
  "1",
  "ja",
  "Ja",
  "TRUE",
  "In der Fahrspur",
  "Direkt auf Fahrspuhr")] <- TRUE
data_nv$Rueckegasse[data_nv$Rueckegasse %in% c(
  "drauf (aber alte Rückegasse)",
  "", NA)] <- FALSE
data_nv$Rueckegasse <- as.logical(data_nv$Rueckegasse)

### clean Esche.markiert --------------------------------------------------------------
data_nv$Esche.markiert[data_nv$Esche.markiert %in% c(
  "nord",
  "Nord",
  "norden")] <- "nord"
data_nv$Esche.markiert[data_nv$Esche.markiert %in% c(
  "no",
  "NO")] <- "nordost"
data_nv$Esche.markiert[data_nv$Esche.markiert %in% c(
  "ost",
  "Ost")] <- "ost"
data_nv$Esche.markiert[data_nv$Esche.markiert %in% c(
  "so",
  "SO")] <- "suedost"
data_nv$Esche.markiert[data_nv$Esche.markiert %in% c(
  "süd",
  "Süd",
  "sued")] <- "sued"
data_nv$Esche.markiert[data_nv$Esche.markiert %in% c(
  "sw",
  "SW")] <- "suedwest"
data_nv$Esche.markiert[data_nv$Esche.markiert %in% c(
  "west",
  "West")] <- "west"
data_nv$Esche.markiert[data_nv$Esche.markiert %in% c(
  "bw",
  "nw",
  "NW")] <- "nordwest"
data_nv$Esche.markiert[data_nv$Esche.markiert %in% c(
  " ",
  "")] <- NA
  
### clean species --------------------------------------------------------------
# This column had to be cleaned by a replacement table
# 
# I had to realise that merge changes the order of the columns and the rows...
# I need the order of rows to fill the plot and quadrant information later so
# I added a ID column.
# 
species_nv <- 
  read.csv("DATA/PROCESSED/IBF/Baumarten_bearbeitet.csv", 
           stringsAsFactors = F, fileEncoding = "UTF-8")
rowcheck_spec <- nrow(data_nv)
data_nv$ID_plant <- 1:nrow(data_nv)
data_nv <- merge(x = data_nv, y = species_nv, by.x = "Baumart", 
                 by.y = "Baumart_alt", all.x = T, all.y = T, sort = F)
rowcheck_spec <- (rowcheck_spec == nrow(data_nv))
data_nv <- dplyr::select(data_nv, !Baumart)
data_nv$Baumart_kurz[data_nv$Baumart_kurz == ""] <- NA
data_nv$Baumart_lang[data_nv$Baumart_lang == ""] <- NA
data_nv <- data_nv[order(data_nv$ID_plant),]

## CATEGORIES FROM COMMENTS -------------------------------------------------
# The comments were exported and categorized
# Following categories were added:
# KONTROLLIEREN
# Foto
# von.Nekrose.bedroht
# keine.Baeume
# tot
# gruene.Hoehe
# Pseudosomas.syringae
# Flaeche.bedeckt
# mehr.ressourcen
# Referenz
# nicht.gerade
# Johannistrieb
# Except of "Bemerkungen" no information were doubled

comments_nv <-
  read.csv(file = "DATA/PROCESSED/IBF/Bemerkungen_bearbeitet.csv",
           stringsAsFactors = F, fileEncoding = "UTF-8")
rowcheck_comm <- nrow(data_nv)
data_nv <- merge(data_nv, comments_nv, by.x = "Bemerkungen", 
              by.y = "Bemerkungen", all.x = T, all.y = T)
rowcheck_comm <- (rowcheck_comm == nrow(data_nv))
data_nv <- data_nv[order(data_nv$ID_plant),]


## CATEGORIES FROM SOMEWHERE ELSE  ---------------------------------------------
# Johannistrieb from Anzahl.triebe
data_nv$Johannistrieb [data_nv$Anzahl.Triebe == "J"] <- TRUE

## remove remaining characters fr
data_nv$Anzahl.Triebe [data_nv$Anzahl.Triebe == "J"] <- NA
data_nv$Anzahl.Triebe [data_nv$Anzahl.Triebe == "e"] <- NA
data_nv$Anzahl.Triebe <- as.numeric(data_nv$Anzahl.Triebe)



## FILL QUADRANT ---------------------------------------------------------------
# The approach is very similar to the one applied for the plotinformation, this
# time the information of the "Quadrant" is copied to all following empty lines
# 
# I had to change the order and first replace the quadrant stuff, since I 
# needed the partly empty plotcolumn, which is filled in the next section
# 
# BUT, I need to check whether all new plots start with a quadrant information...
# I have the feeling they dont...
# No they dont, if a plot starts without a quadrant information all following 
# rows until the first 

tmp_quadrant <- NA

for (i in 1:nrow(data_nv)) {
  if (is.na(data_nv$Quadrant[i]) & is.na(data_nv$Plotnummer[i])) {
    data_nv$Quadrant[i] <- tmp_quadrant
  } else if (is.na(data_nv$Quadrant[i]) & !is.na(data_nv$Plotnummer[i])) {
    data_nv$Quadrant[i] <- "NA"
    tmp_quadrant <- "NA"
  } else {
    tmp_quadrant <- data_nv$Quadrant[i]
  }
}
# A few information were missing, but I could reconstruct some:
data_nv$Quadrant[data_nv$Plotnummer == 83 &
                   data_nv$Flaeche == "Mollenfelde" &
                   data_nv$Quadrant == "NA"] <- "nordwest"


## FILL PLOTINFORMATION -------------------------------------------------
# In the raw data each plot was described only in the first line of a plot,
# all following lines lack these information. So they need to filled with the 
# data of the first line of the plot
# 

tmp_plotnummer <- NA
tmp_rueckegasse <- NA
tmp_rand <- NA
tmp_zaun <- NA
tmp_referenz <- NA
tmp_bedeckt <- NA

for (i in 1:nrow(data_nv)) {
  if (is.na(data_nv$Plotnummer[i])) {
    data_nv$Plotnummer[i] <- tmp_plotnummer
    data_nv$Rueckegasse[i] <- tmp_rueckegasse
    data_nv$Rand[i] <- tmp_rand
    data_nv$Zaun[i] <- tmp_zaun
    data_nv$Referenz[i] <- tmp_referenz
    data_nv$Flaeche.bedeckt[i] <- tmp_bedeckt
  } else {
    tmp_plotnummer <- data_nv$Plotnummer[i]
    tmp_rueckegasse <- data_nv$Rueckegasse[i]
    tmp_rand <- data_nv$Rand[i]
    tmp_zaun <- data_nv$Zaun[i]
    tmp_referenz <- data_nv$Referenz[i]
    tmp_bedeckt <- data_nv$Flaeche.bedeckt[i]
  }
}

## ADD COLUMN ETS GENERAL ------------------------------------------------------
data_nv$ETS <-
  !is.na(data_nv$ETS.abgestorben.frisch) |
  !is.na(data_nv$ETS.abgestorben.alt) |
  !is.na(data_nv$ETS.lebend)


## ORDER OF COLUMNS ------------------------------------------------------------
# And generate plotIDs
data_nv$ID_plot <- c(paste0(data_nv$Plotnummer, substr(data_nv$Flaeche, 1, 3)))
data_nv <- data_nv[ ,c("ID_plant",
                       "ID_plot",
                       "Flaeche",
                       "Plotnummer",
                       "Rand",
                       "Zaun",
                       "Rueckegasse",
                       "keine.baeume",
                       "Flaeche.bedeckt",
                       "Referenz",
                       "Quadrant",
                       "Esche.markiert",
                       "Baumart_kurz",
                       "Baumart_lang",
                       "Hoehe",
                       "Hoehe.Vorjahr",
                       "Hoehe.Vorvorjahr",
                       "Anzahl.Triebe",
                       "ETS.abgestorben.frisch",
                       "ETS.abgestorben.alt",
                       "ETS.lebend",
                       "ETS",
                       "Verbiss.lebend",
                       "Verbiss.tot",                    
                       "Sonstige.Gruende.tot",
                       "von.Nekrose.bedroht",
                       "ETS.abgestorben.frisch.terminal",
                       "ETS.abgestorben.alt.terminal",
                       "ETS.lebend.terminal",
                       "Verbiss.lebend.terminal",
                       "Verbiss.tot.terminal",
                       "Sonstige.Gruende.tot.terminal",
                       "eigentlich.aelter",
                       "Johannistrieb",
                       "nicht.gerade",
                       "mehr.ressourcen",
                       "Pseudomonas.syringae",
                       "gruene.Hoehe",
                       "tot",
                       "Foto",
                       "KONTROLLIEREN",
                       "Bemerkungen"
                       )]
                       
## EXPORT CSV ---------------------------------------------------------------
# Export the data_nv file

write.csv(x = data_nv, file = "EXPORT/IBF/tables/data_nv.csv", 
          fileEncoding = "UTF-8", row.names = F)


## TIDY UP  ----------------------------------------------------------------
rm(data_nv_huy, data_nv_mol, data_nv_platt, data_nv_scho, data_nv_weis,
   i, t, select, select1, select2, comments_nv, tmp_bedeckt, tmp_plotnummer,
   species_nv, tmp_quadrant, tmp_rand, tmp_referenz, tmp_rueckegasse, tmp_zaun,
   rowcheck_comm, rowcheck_spec)

## OUTPUT ------------------------------------------------------------------
# data_nv
# 
# rowcheck_spec and rowcheck_comm  
# (are there any errors due to the merging with the comments?)
# 



## JUNK --------------------------------------------------------------------
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

