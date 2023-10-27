#======================== Zuwachs ~ ETS =======================================#
# J.Osewold
# 27.10.23
# REWORK
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/1000ETS/load_data_1000.R", encoding = "UTF-8")
# data_1000
# Durchmesser_1000
# ETS_Stufen_1000_clean

## LIBRARYS --------------------------------------------------------------------
require(dplyr)
require(ggplot2)
require(ggpubr)

## NOTES -----------------------------------------------------------------------
# 1 2 3 4 5.1 | 5.2 17 9 8  
# Zunächst einmal wird die ETS Stufe die aktuell zu sehen ist mit dem BHD Zuwachs
# der nächstes Jahr zu messen ist assoziiert.
# 5.2 9 17 8 alle diese Stufen werden durch NA ersetzt, bzw dadurch dass ich 
# ETSStufen_1000_clean nehme erübrigt sich der Schritt für 9 17 und 8.
# Die 5.1er werden mit den 4ern zusammen geworfen, denn nach neuerer Bonitur 
# sind das alles 4er

## DATEN WERDEN ANGEPASST  -----------------------------------------------------

ETS_tmp <- ETSStufen_1000_clean
D_tmp <- Durchmesser_1000

ETS_tmp$`2014`[data_1000$tot_14 == 2] <- NA
ETS_tmp$`2015`[data_1000$tot_15 == 2] <- NA
ETS_tmp$`2016`[data_1000$tot_16 == 2] <- NA
ETS_tmp$`2017`[data_1000$tot_17 == 2] <- NA
ETS_tmp$`2018`[data_1000$tot_18 == 2] <- NA
ETS_tmp$`2019`[data_1000$tot_19 == 2] <- NA
ETS_tmp$`2020`[data_1000$tot_20 == 2] <- NA
ETS_tmp$`2021`[data_1000$tot_21 == 2] <- NA
ETS_tmp$`2022`[data_1000$tot_22 == 2] <- NA

Zuwachs <- Zuwachs [data_1000$Durchmesser.problematisch.bzw..raus.lassen == FALSE, ]
ETS_tmp <- ETS_tmp [data_1000$Durchmesser.problematisch.bzw..raus.lassen == FALSE, ]
D_tmp <- D_tmp [data_1000$Durchmesser.problematisch.bzw..raus.lassen == FALSE, ]
# Ich habe erst angefangen, einzelne Werte zu suchen und anzupassen und dann
# herausgefunden, dass ich an diesem Punkt schon mal war und habe die Spalte 
# jetzt blind wieder verwendet. Das werde ich damals schon vernünftig gemacht 
# haben.

## NEUE TABELLE ANGELEGT   -----------------------------------------------------

x <- 1:nrow(D_tmp) # Das war der einfachste Weg die richtige nrow hinzukriegen
Zuwachs <- data.frame("2014" = c(x), "2015" = c(x), "2016" = c(x),
											"2017" = c(x), "2018" = c(x), "2019" = c(x),
											"2020" = c(x), "2021" = c(x))

# Zuwachsmessung:
for (i in 1:ncol(Zuwachs)) {
	Zuwachs[ ,i] <- D_tmp[, i+1] - D_tmp[, i]
}

# Ab hier werden alle Messwerte einfach aneinader gehängt. Und nicht mehr nach
# Jahren sortiert.
Zuwachs_Schadstufe <- data.frame(
	Zuwachs = c(1:(nrow(Zuwachs)*ncol(Zuwachs))),
	Schadstufe = c(1:(nrow(Zuwachs)*ncol(Zuwachs))),
	Durchmesser = c(1:(nrow(Zuwachs)*ncol(Zuwachs))))

Zuwachs_Schadstufe$Zuwachs <- unname(unlist(Zuwachs))
Zuwachs_Schadstufe$Schadstufe <- unname(unlist(ETS_tmp[ ,2:9]))
        # Hier ist wichtig zu beachten, dass nur die Schadstufen von 2014 bis 
        # 2021 (inklusive also n = 8) kopiert werden)
Zuwachs_Schadstufe$Durchmesser <- unname(unlist(D_tmp[ ,1:8]))
        # Hier gilt das gleiche nur dass die Durchmesser Tabelle ohnehin nur 
        # Daten ab 2014 beinhaltet. 

# Im Jahr 2015 wird nun also die ETS Stufe von 2015, der Durchmesser von 2015 
# und der Zuwachs von 2015:2016 aufgelistet.

Zuwachs_Schadstufe <- Zuwachs_Schadstufe %>% 
	mutate(Schadstufe = replace(Schadstufe, Schadstufe == 5, 4))

Zuwachs_Schadstufe$Schadstufe <- factor(Zuwachs_Schadstufe$Schadstufe,
																					 levels = c("1", "2", "3", "4"), 
																					 labels = c("1", "2", "3", "4"))

Zuwachs_Schadstufe <- Zuwachs_Schadstufe[
	!is.na(Zuwachs_Schadstufe$Schadstufe), ]

Zuwachs_Schadstufe$Durchmesser_grp[Zuwachs_Schadstufe$Durchmesser <= 200] <- 1
Zuwachs_Schadstufe$Durchmesser_grp[Zuwachs_Schadstufe$Durchmesser > 200 &
																	 	Zuwachs_Schadstufe$Durchmesser <= 500] <- 2
Zuwachs_Schadstufe$Durchmesser_grp[Zuwachs_Schadstufe$Durchmesser > 500] <- 3

Zuwachs_Schadstufe$Durchmesser_grp <- as.factor(Zuwachs_Schadstufe$Durchmesser_grp)

# Alternative:
c <- table(Zuwachs_Schadstufe$Durchmesser_grp)
levels(Zuwachs_Schadstufe$Durchmesser_grp) <-
	c(paste0("<= 200 \n Durchmesser [mm] \n n = ", c[1]),
		paste0("200 - 500 \n Durchmesser [mm] \n n = ", c[2]),
		paste0("> 500 \n Durchmesser [mm] \n n = ", c[3]))


levels(Zuwachs_Schadstufe$Durchmesser_grp) <-
	c("<= 200 \n Durchmesser [mm]",
		"200 - 500 \n Durchmesser [mm]",
		"> 500 \n Durchmesser [mm]")

Zuwachs_Schadstufe <-
	Zuwachs_Schadstufe[!is.na(Zuwachs_Schadstufe$Durchmesser_grp), ]


## GRAPH  --------------------------------------------------------------------
plot <- ggplot(data = Zuwachs_Schadstufe) +
	geom_vline(aes(xintercept = 0)) +
	geom_violin(aes(x = Zuwachs,
									y = Schadstufe,
									fill = Schadstufe),
							bw = "bcv",
							# bw = bandwith, bei dem default hat er nur bei Schadstufe 4 ein ZickZack immer
							# zu den 0.5er Werten gemacht.
							scale = "count") +
	scale_fill_discrete(type = c("#a2c617", "#ffdd00", "#f8a800", "#e5420f")) +
	coord_cartesian(xlim = c(-10, 15)) +
	facet_grid(cols = vars(Durchmesser_grp)) +
	theme_bw()
plot
plot + ggsave(filename = "EXPORT/1000ETS/FIGURES/Zuwachs_ETS.pdf", units = "mm",
							width = 200, height = 120)


## TIDY UP  --------------------------------------------------------------------
rm(D_tmp, ETS_tmp, plot, Zuwachs_Schadstufe)

## OUTPUT ----------------------------------------------------------------------
# Zuwachs
# und ein Plot in /EXPORT

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

