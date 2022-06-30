################ LOAD SOLARISKOP DATA ################################
# J.Osewold
# 22.10.2021
##### DONE #####
################################################################################

###### LIBRARYS -------------------------------------------------------------------
library(stringr)

###### NOTES -------------------------------------------------------------------
# The data has to be loaded and afterwards only the correct thresholds selected
# I had multiple issues with the automatic naming of the solariskop, plus many 
# values are formatted as weird characters "7%" and "     name    "


###### STEP 1 Loading ----------------------------------------------------------

thr_all <- read.csv("DATA/RAW/Goe_Lau/Solariskop/All Images.csv", header = T, 
                    stringsAsFactors = F,  )

# The whole table was formatted with spaces :( ... 
thr_all[] <- lapply(thr_all, function(x) (gsub("[ ]", "", x)))

thr_selection <-
  read.csv2(file = "DATA/RAW/Goe_Lau/Solariskop/Auswahl des Thresholds All Images.csv",
            header = T,
            stringsAsFactors = F)

###### STEP 2 Harmonizing of plotnames/numbers ---------------------------------

# The Plotnumbers were formatted in a strange way by the solariskop, they had
# leading 0s. Plus issues like "001" "001B" "002"

# for all cases like "8746B" -> "08746B"
temp_slct <- (nchar(thr_selection$plotnr) == 5)
thr_selection$plotnr[temp_slct] <- str_pad(thr_selection$plotnr[temp_slct], 
                                           width = 6, pad = "0")

# for all cases like "8746" -> "08746"
temp_slct <- (nchar(thr_selection$plotnr) == 4)
thr_selection$plotnr[temp_slct] <- str_pad(thr_selection$plotnr[temp_slct], 
                                           width = 5, pad = "0")

# for all cases like "001" the leading zeros are inserted, str_pad ignores 
# cases longer than the width
thr_selection$plotnr <- str_pad(thr_selection$plotnr, width = 3, pad = "0")

# for all cases including a alphanumeric like "001B"
temp_slct <- grep("[[:alpha:]]", thr_selection$plotnr)
thr_selection$plotnr[temp_slct] <- str_pad(thr_selection$plotnr[temp_slct], 
                                           width = 4, pad = "0")
# complete the plotnames to "001B_Thr1"
thr_selection$verbatim <- paste(thr_selection$plotnr, 
                                thr_selection$threshold, sep = "_Thr")

###### STEP 2 Selection of the correct thresholds ------------------------------
thr_selected <- thr_all[thr_all$name %in% thr_selection$verbatim,]

###### STEP 3 Formatting  ------------------------------------------------------

# All values like "13%" -> 0.13
thr_selected[, c("DSF", "ISF", "TSF", "Opn", "GapF")] <- 
  lapply (thr_selected[, c("DSF", "ISF", "TSF", "Opn", "GapF")], 
          function(x) gsub ("%", "", x))
thr_selected[, c("DSF", "ISF", "TSF", "Opn", "GapF")] <- 
  lapply(thr_selected[, c("DSF", "ISF", "TSF", "Opn", "GapF")], function (x) {
         as.numeric(x)/100})

# All other characters values will get numeric
str(thr_selected)
thr_selected[, c("LAI", "ELAD", "atm", "Start", "End", "TF", "AOV", "lat",
                 "angle", "thr")] <- 
  lapply(thr_selected[, c("LAI", "ELAD", "atm", "Start", "End", "TF", "AOV", 
                          "lat", "angle", "thr")], as.numeric)

# The names will get rid of their _thr and become numeric and actual names
for (i in 1:6) {
  rep <- c("_Thr1", "_Thr2", "_Thr3", "_Thr4", "_Thr5", "B") [i]
  thr_selected$name <- gsub (rep, "", thr_selected$name) 
} 
thr_selected$name <- as.numeric(thr_selected$name)
rownames(thr_selected) <- thr_selected$name


### Tidy up 
rm(thr_all, thr_selection, temp_slct, i , rep)

###### OUTPUT ------------------------------------------------------------------
# thr_selected
             