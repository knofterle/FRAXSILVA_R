#============================ TITLE ===========================================#
# J.Osewold
# 13.10.23
# STATUS
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/1000ETS/Mortalitaet_Kaplan_Meier_1-3.R")
 
# The data and variables from the previous scripts can be listed here

## LIBRARYS --------------------------------------------------------------------

## NOTES -----------------------------------------------------------------------
# Wenn eine Esche die observ_time 8 und status 1 und reason 52 hat, bedeutet das,
# dass die Esche im Sommer 2020 tot aufgefunden wurde.
# Wenn sie eine observ_time von 8 und einen status von 0 und einen reason von
# NA oder 9_ hat dann wurde sie 2019  noch gefunden und 2020 wurde das erste
# und vielleicht auch das letzte mal 9 eingetragen, d.h. nicht einsehbar oder 
# nicht gefunden oder so. Quasi ab dann NA
# 

## Schleife  ---------------------------------------------------------------------
# Für jedes Jahr ab 2013 werden alle Eschen gesucht die eine bestimmte Schadstufe
# hatten, anschließend werden wird gesucht welche Eschen in den nächsten 1 und 
# 2 Jahren aus natürlichen Gründen gestorben sind. 
# 1 = 2013, 8  = 2020
# 
# Es gab das Problem, dass die Spaltennamen nur Nummern hatten und das mag R 
# gar nicht, daher kommt hier zu jeder Spalte ein Buchstabe dazu. Aber das 
# funktioniert dann immer noch nicht, man kann einen characterstring fast 
# gar nicht zum... Okay es geht mit tmp[char_year] == 4 doch.
# 
# Was ich zunächst übersehen habe sind Eschen mit der Schadstufe 5 die aber 
# nicht tot sind. Die zählen nach heutiger Bonitierung eigentlich auch als 4er.
# Deshalb wurden die dann zusammen gefasst. Aber um den Vergleich zu haben und 
# für die Fehlersuche sind fünf und vier trotzdem auch separat zu haben.
# 
# 
# 

ETSStufen_1000_tmp <- cbind(ETSStufen_1000, baum_id = data_1000$baum_id)
# colnames(ETSStufen_1000_tmp) <- paste0("x", colnames(ETSStufen_1000)) 

eins <- data.frame(erstesJahr_lebt = c(1:8),erstesJahr_tot = c(0), 
									 zweitesJahr_lebt = c(0), zweitesJahr_tot = c(0), n_y0 = c(0))
zwei <- data.frame(erstesJahr_lebt = c(1:8),erstesJahr_tot = c(0), 
									 zweitesJahr_lebt = c(0), zweitesJahr_tot = c(0), n_y0 = c(0))
drei <- data.frame(erstesJahr_lebt = c(1:8),erstesJahr_tot = c(0), 
									 zweitesJahr_lebt = c(0), zweitesJahr_tot = c(0), n_y0 = c(0))
vier <- data.frame(erstesJahr_lebt = c(1:8),erstesJahr_tot = c(0), 
									 zweitesJahr_lebt = c(0), zweitesJahr_tot = c(0), n_y0 = c(0))
fünf <- data.frame(erstesJahr_lebt = c(1:8),erstesJahr_tot = c(0), 
									 zweitesJahr_lebt = c(0), zweitesJahr_tot = c(0), n_y0 = c(0))
vierfünf <- data.frame(erstesJahr_lebt = c(1:8),erstesJahr_tot = c(0), 
									 zweitesJahr_lebt = c(0), zweitesJahr_tot = c(0), n_y0 = c(0))

for (i in 1:8) {
	tmp <- mort_status %>% filter(observ_time >= i + 1) 
	tmp <- left_join(x = tmp, y = ETSStufen_1000_tmp, by = "baum_id")
	tmp_re <- tmp
	char_year <- as.character(2012 + i)
	
	tmp <- tmp[tmp[char_year] == 1, ] # Eschen werden nach Schadstufe gefiltert
	rownames(eins)[i] <- char_year
	eins$n_y0[i] <- nrow(tmp)
	eins$erstesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+2))
	eins$zweitesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+3)) 
	# wurde im Jahr danach noch gefunden und dann ist vielleicht irgendwas passiert
	eins$erstesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+1 & reason == "52"))
	eins$zweitesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+2 & reason == "52"))
	# Für das letzte Jahr (zweites Jahr lebt, 2020) muss ein anderer Weg gefunden
	# werden, weil es keine Bäume mit einer observ_time 2023 (11) gibt. Statt 
	# dessen sind das die Eschen die 2022 ein reason == alive bekommen haben:
	eins$zweitesJahr_lebt[8] <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
	# diese Zeile Code macht nur im letzten Durchgang sinn, aber dann wird der
	# Wert halt mehrmals überschrieben ist ja egal hauptsache am Ende steht
	# was sinnvolles.
	tmp <- tmp_re
	
	tmp <- tmp[tmp[char_year] == 2, ] # Eschen werden nach Schadstufe gefiltert
	rownames(zwei)[i] <- char_year
	zwei$n_y0[i] <- nrow(tmp)
	zwei$erstesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+2))
	zwei$zweitesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+3)) 
	zwei$erstesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+1 & reason == "52"))
	zwei$zweitesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+2 & reason == "52"))
	zwei$zweitesJahr_lebt[8] <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
	tmp <- tmp_re
	
	tmp <- tmp[tmp[char_year] == 3, ] # Eschen werden nach Schadstufe gefiltert
	rownames(drei)[i] <- char_year
	drei$n_y0[i] <- nrow(tmp)
	drei$erstesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+2))
	drei$zweitesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+3)) 
	drei$erstesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+1 & reason == "52"))
	drei$zweitesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+2 & reason == "52"))
	drei$zweitesJahr_lebt[8] <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
	tmp <- tmp_re
	
	tmp <- tmp[tmp[char_year] == 4 | tmp[char_year] == 5, ] # Eschen werden nach Schadstufe gefiltert
	rownames(vierfünf)[i] <- char_year
	vierfünf$n_y0[i] <- nrow(tmp)
	vierfünf$erstesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+2))
	vierfünf$zweitesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+3)) 
	vierfünf$erstesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+1 & reason == "52"))
	vierfünf$zweitesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+2 & reason == "52"))
	vierfünf$zweitesJahr_lebt[8] <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
	tmp <- tmp_re
	
	tmp <- tmp[tmp[char_year] == 5, ] # Eschen werden nach Schadstufe gefiltert
	rownames(fünf)[i] <- char_year
	fünf$n_y0[i] <- nrow(tmp)
	fünf$erstesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+2))
	fünf$zweitesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+3)) 
	fünf$erstesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+1 & reason == "52"))
	fünf$zweitesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+2 & reason == "52"))
	fünf$zweitesJahr_lebt[8] <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
	tmp <- tmp_re
	
	tmp <- tmp[tmp[char_year] == 4, ] # Eschen werden nach Schadstufe gefiltert
	rownames(vier)[i] <- char_year
	vier$n_y0[i] <- nrow(tmp)
	vier$erstesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+2))
	vier$zweitesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+3)) 
	vier$erstesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+1 & reason == "52"))
	vier$zweitesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+2 & reason == "52"))
	vier$zweitesJahr_lebt[8] <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
	tmp <- tmp_re
}


# Ich muss noch checken was die Filter mit NAs machen, denn bei nrow werden 
# auch NAs mit gezählt. Es scheint so, dass filter NAs nicht ausgibt
# (NA == 5 -> F), also alles wunderbar und so wie gewünscht.


## Überlebensprognose, Tabelle  -------------------------------------------
prognose <- data.frame(Schadstufe_y0 = c(1,2,3,4), 
											 n_y0 = c(0),
											 abgestorben_y1 = c(0),
											 abgestorben_y2 = c(0))

prognose[1,] <- c(1,
									sum(eins$n_y0),
									sum(eins$erstesJahr_tot)/ sum(eins$n_y0),
									sum(eins$zweitesJahr_tot)/ sum(eins$n_y0))

prognose[2,] <- c(2,
									sum(zwei$n_y0),
									sum(zwei$erstesJahr_tot)/ sum(zwei$n_y0),
									sum(zwei$zweitesJahr_tot)/ sum(zwei$n_y0))

prognose[3,] <- c(3,
									sum(drei$n_y0),
									sum(drei$erstesJahr_tot)/ sum(drei$n_y0),
									sum(drei$zweitesJahr_tot)/ sum(drei$n_y0))

prognose[4,] <- c(4,
									sum(vierfünf$n_y0),
									sum(vierfünf$erstesJahr_tot)/ sum(vierfünf$n_y0),
									sum(vierfünf$zweitesJahr_tot)/ sum(vierfünf$n_y0))



## Summen  --------------------------------------------------------------------
# Die Werte der Jahre werden nach n gewichtet und dann ein mean gebildet, wobei
# das aufs gleiche hinausläuft wie eine lange Summe. Insofern mache ich das,
# ist wesentlich einfacher ;)
# Ich habe die Summen nach hinten geschoben, damit die Tabellenerstellung in dem
# Abschnitt davor einfacher wird

eins [9,] <- apply(X = eins, FUN = sum, MARGIN = 2)
row.names(eins) [9] <- "sum"

zwei [9,] <- apply(X = zwei, FUN = sum, MARGIN = 2)
row.names(zwei) [9] <- "sum"

drei [9,] <- apply(X = drei, FUN = sum, MARGIN = 2)
row.names(drei) [9] <- "sum"

vierfünf [9,] <- apply(X = vierfünf, FUN = sum, MARGIN = 2)
row.names(vierfünf) [9] <- "sum"

fünf [9,] <- apply(X = fünf, FUN = sum, MARGIN = 2)
row.names(fünf) [9] <- "sum"

vier [9,] <- apply(X = vier, FUN = sum, MARGIN = 2)
row.names(vier) [9] <- "sum"


## TIDY UP  --------------------------------------------------------------------
rm(ETSStufen_1000_tmp, tmp)

## OUTPUT ----------------------------------------------------------------------
# 



## JUNK ------------------------------------------------------------------------


# i <- 4
# 
# tmp <- mort_status %>% filter(observ_time >= i + 1) 
# tmp <- left_join(x = tmp, y = ETSStufen_1000_tmp, by = "baum_id")
# tmp_re <- tmp
# char_year <- as.character(2012 + i)
# char_year
# tmp
# 
# tmp <- tmp[tmp[char_year] == 4, ] # Eschen werden nach Schadstufe gefiltert
# rownames(vier)[i] <- char_year
# vier$erstesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+2))
# vier$zweitesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+3)) 
# vier$erstesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+1 & reason == "52"))
# vier$zweitesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+2 & reason == "52"))
# vier$zweitesJahr_lebt[8] <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
# tmp <- tmp_re


## STEP 1  ---------------------------------------------------------------------
# tmp
# table(mort_status$observ_time)
# 
# head(mort_status)
# 
# tmp <- mort_status %>% filter(observ_time >= 9)
# 
# data_1000_tmp <- select(.data = data_1000, baum_id, ets_stufe_20) 
# head(data_1000_tmp)
# tmp <- left_join(x = tmp, y = data_1000_tmp, by = "baum_id")
# head(tmp)
# tmp$ets_stufe_20
# 
# tmp <- filter(tmp, ets_stufe_20 == 4)
# 
# survive1 <- nrow(tmp %>% filter(observ_time == 10))
# dead1 <- nrow(tmp %>% filter(observ_time == 9 & reason == "52"))
# 
# survive2 <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
# dead2 <- nrow(tmp %>% filter(observ_time == 10 & reason == "52"))
# 
# tmp <- mort_status %>% filter(observ_time >= 1 + 1)
# tmp <- left_join(x = tmp, y = ETSStufen_1000, by = "baum_id")

# Es gab das Problem, dass die Spaltennamen nur Nummern hatten und das mag R 
# gar nicht, daher kommt hier zu jeder Spalte ein Buchstabe dazu:

tmp[tmp[char_year]  == 4, ]

char_year

str(tmp[char_year])
T |F

tmp[char_year] == 4 | tmp[char_year] == 5


tibble( brot = c(NA, 5, 6)) %>%  filter(brot == 5)

sum(eins$erstesJahr_lebt, eins$erstesJahr_tot)
