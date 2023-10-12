#============================ TITLE ===========================================#
# J.Osewold
# 12.10.2023
# STATUS
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/1000ETS/Mortalitaet_Kaplan_Meier_1-3.R")
# mort_status
# 

## LIBRARYS --------------------------------------------------------------------
require(ggplot2)

## NOTES -----------------------------------------------------------------------
# Es wird additiv für jedes Jahr zusammen gefasst wie viele Eschen an welchen
# Gründen gestorben sind. Dabei werden absolut alle die eine Säge gesehen haben
# als Entnahmen eingetragen.

## CREATE NEW TABLE  ---------------------------------------------------------------------
# Die Tabelle sieht folgendermaßen aus:
#   Year tot_entnahme tot_natur lebend zensiert
#   2013            0         0   1002        0
#   2014           56        15    931        0
# ....
#   2022           255      325    406       16

mort_year <- data.frame(
	year = c(2013:2022),
	tot_entnahme = c(0),
	tot_natur = c(0),
	lebend = c(0),
	zensiert = c(0))
mort_year$lebend[1] <- nrow(mort_status)

for (i in 2:10) {
	tmp <- mort_status %>% filter(observ_time == i)
	mort_year$tot_entnahme[i] <- sum(tmp$reason %in% c("48", "18")) +
		mort_year$tot_entnahme[i-1]
	mort_year$tot_natur[i] <- sum(tmp$reason %in% c("52")) +
		mort_year$tot_natur[i-1] 
	mort_year$zensiert[i] <- sum(tmp$reason %in% c("NA", "9_")) +
		mort_year$zensiert[i-1]
	
	mort_year$lebend[i] <- 
		mort_year$lebend[1] - 
		mort_year$tot_entnahme[i] -
		mort_year$tot_natur[i] -
		mort_year$zensiert[i] 
}

## CREATE BarPlot  -----------------------------------------------------------

tmp <- data.frame(
	year = mort_year$year,
	y = c(mort_year$tot_entnahme, mort_year$tot_natur, mort_year$lebend, mort_year$zensiert),
	reason = c(rep(c("tot_entnahme", "tot_natur", "lebend", "zensiert"), each = 10)))
tmp$reason <- factor(tmp$reason, 
										 levels = c("tot_entnahme", "tot_natur", "zensiert", "lebend"))

plot <- ggplot(data = tmp) +
	geom_col(aes(x = year, y = y, fill = reason)) +
	scale_fill_manual(values=c("#333333", "#666633", "#3399CC", "#006600")) +
	scale_x_continuous(breaks = c(2013:2022),
										 labels = as.character(c(2013:2022)))
	
plot

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





sum(tmp$reason == "52")
