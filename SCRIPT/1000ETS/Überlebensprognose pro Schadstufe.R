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

## Schleife  ---------------------------------------------------------------------
# Für jedes Jahr ab 2013 werden alle Eschen gesucht die eine bestimmte Schadstufe
# hatten, anschließend werden wird gesucht welche Eschen in den nächsten 1 und 
# 2 Jahren aus natürlichen Gründen gestorben sind. 
# 1 = 2013, 8  = 2020
# 
# Es gab das Problem, dass die Spaltennamen nur Nummern hatten und das mag R 
# gar nicht, daher kommt hier zu jeder Spalte ein Buchstabe dazu. Aber das 
# funktioniert dann immer noch nicht, man kann einen characterstring fast 
# gar nicht zum 
# 
# 

ETSStufen_1000_tmp <- cbind(ETSStufen_1000, baum_id = data_1000$baum_id)
# colnames(ETSStufen_1000_tmp) <- paste0("x", colnames(ETSStufen_1000)) 

eins <- data.frame(erstesJahr_lebt = c(1:8),erstesJahr_tot = c(0), 
									 zweitesJahr_lebt = c(0), zweitesJahr_tot = c(0))
zwei <- data.frame(erstesJahr_lebt = c(1:8),erstesJahr_tot = c(0), 
									 zweitesJahr_lebt = c(0), zweitesJahr_tot = c(0))
drei <- data.frame(erstesJahr_lebt = c(1:8),erstesJahr_tot = c(0), 
									 zweitesJahr_lebt = c(0), zweitesJahr_tot = c(0))
vier <- data.frame(erstesJahr_lebt = c(1:8),erstesJahr_tot = c(0), 
									 zweitesJahr_lebt = c(0), zweitesJahr_tot = c(0))

for (i in 1:8) {
	tmp <- mort_status %>% filter(observ_time >= i + 1) 
	tmp <- left_join(x = tmp, y = ETSStufen_1000_tmp, by = "baum_id")
	tmp_re <- tmp
	char_year <- as.character(2012 + i)
	
	tmp <- tmp[tmp[char_year] == 1, ] # Eschen werden nach Schadstufe gefiltert
	rownames(eins)[i] <- char_year
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
	zwei$erstesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+2))
	zwei$zweitesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+3)) 
	zwei$erstesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+1 & reason == "52"))
	zwei$zweitesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+2 & reason == "52"))
	zwei$zweitesJahr_lebt[8] <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
	tmp <- tmp_re
	
	tmp <- tmp[tmp[char_year] == 3, ] # Eschen werden nach Schadstufe gefiltert
	rownames(drei)[i] <- char_year
	drei$erstesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+2))
	drei$zweitesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+3)) 
	drei$erstesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+1 & reason == "52"))
	drei$zweitesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+2 & reason == "52"))
	drei$zweitesJahr_lebt[8] <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
	tmp <- tmp_re
	
	tmp <- tmp[tmp[char_year] == 4, ] # Eschen werden nach Schadstufe gefiltert
	rownames(vier)[i] <- char_year
	vier$erstesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+2))
	vier$zweitesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+3)) 
	vier$erstesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+1 & reason == "52"))
	vier$zweitesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+2 & reason == "52"))
	vier$zweitesJahr_lebt[8] <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
	tmp <- tmp_re
}


# Ich muss noch checken was die Filter mit NAs machen, denn bei nrow werden 
# auch NAs mit gezählt.

## Summen  --------------------------------------------------------------------
# Die Werte der Jahre werden nach n gewichtet und dann ein mean gebildet, wobei
# das aufs gleiche hinausläuft wie eine lange Summe. Insofern mache ich das,
# ist wesentlich einfacher ;)

eins [9,] <- apply(X = eins, FUN = sum, MARGIN = 2)
row.names(eins) [9] <- "sum"

zwei [9,] <- apply(X = zwei, FUN = sum, MARGIN = 2)
row.names(zwei) [9] <- "sum"

drei [9,] <- apply(X = drei, FUN = sum, MARGIN = 2)
row.names(drei) [9] <- "sum"

vier [9,] <- apply(X = vier, FUN = sum, MARGIN = 2)
row.names(vier) [9] <- "sum"


## TIDY UP  --------------------------------------------------------------------
rm(ETSStufen_1000_tmp, tmp)

## OUTPUT ----------------------------------------------------------------------
# 



## JUNK ------------------------------------------------------------------------
i <- 4

tmp <- mort_status %>% filter(observ_time >= i + 1) 
tmp <- left_join(x = tmp, y = ETSStufen_1000_tmp, by = "baum_id")
tmp_re <- tmp
char_year <- as.character(2012 + i)
char_year
tmp

tmp <- tmp[tmp[char_year] == 4, ] # Eschen werden nach Schadstufe gefiltert
rownames(vier)[i] <- char_year
vier$erstesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+2))
vier$zweitesJahr_lebt[i] <- nrow(tmp %>% filter(observ_time >= i+3)) 
vier$erstesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+1 & reason == "52"))
vier$zweitesJahr_tot[i] <- nrow(tmp %>% filter(observ_time == i+2 & reason == "52"))
vier$zweitesJahr_lebt[8] <- nrow(tmp %>% filter(observ_time == 10 & reason == "alive"))
tmp <- tmp_re

