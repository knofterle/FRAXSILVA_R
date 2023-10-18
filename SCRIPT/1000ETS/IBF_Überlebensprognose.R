#============================ TITLE ===========================================#
# J.Osewold
# 18.10.2023
# NEW
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
# IBF Boniturdaten

## LIBRARYS --------------------------------------------------------------------
require(dplyr)
require(ggplot2)

## NOTES -----------------------------------------------------------------------
# Eschen die einmal abgestorben sind verschwinden nicht aus dem Datensatz sondern
# bleiben für die folgenden Jahre bei "5", sie können aber auf "6" wechseln. 
# Das ist anders als in meinem Datensatz. Daher musste ich noch Substraktionen 
# in die Kalkulationen einbauen.

## STEP 1  ---------------------------------------------------------------------
ibf <- read.csv2(file = "DATA/RAW/IBF/20230919_ibf_data.csv", encoding = "UTF-8")
str(ibf)

## STEP 1  ---------------------------------------------------------------------

prognose_ibf <- data.frame(
	Schadstufe_y0 = c(1, 2, 3, 4),
	n_y0 = c(0),
	abgestorben_y1 = c(0),
	abgestorben_y2 = c(0),
	umgefallen_y1 = c(0),
	umgefallen_y2 = c(0)
)


for (i in 1:4) {
	tmp <-
		ibf %>% filter(Season_Jahr == "Sommer_2021" &
									 	Kronenzustand == as.character(i)) %>%
		select(baum_id)
	tmp <- tmp$baum_id
	
	tmp2 <- ibf %>%
		filter(Season_Jahr == "Sommer_2022" &
					 	Kronenzustand != "5" &
					 	baum_id %in% tmp) %>%
		select(baum_id)
	# Eschen die im ersten Jahr nicht gestorben sind
	tmp2 <- tmp2$baum_id
	abgestorben_y2 <- ibf %>%
		filter(Kronenzustand == "5" &
					 	Season_Jahr == "Sommer_2023" &
					 	baum_id %in% tmp2) %>%
		nrow() /
		length(tmp)
	# Es werden nur Eschen gezählt die im ersten Jahr (y1) noch lebten
	
	tmp3 <- ibf %>%
		filter(Season_Jahr == "Sommer_2022" &
					 	Kronenzustand != "6" &
					 	baum_id %in% tmp) %>%
		select(baum_id)
	# Eschen die im ersten Jahr nicht umgefallen sind
	tmp3 <- tmp3$baum_id
	umgefallen_y2 <- ibf %>%
		filter(Kronenzustand == "6" &
					 	Season_Jahr == "Sommer_2023" &
					 	baum_id %in% tmp3) %>%
		nrow() /
		length(tmp)
	# Es werden nur Eschen gezählt die im Jahr davor (y1) noch standen (auch solche
	# mit "5"
	
	prognose_ibf[i,] <- c(
		i,
		
		length(tmp),
		
		ibf %>% filter(
			baum_id %in% tmp &
				Kronenzustand == "5" & Season_Jahr == "Sommer_2022"
		) %>% nrow() / length(tmp),
		
		abgestorben_y2,
		
		ibf %>% filter(
			baum_id %in% tmp &
				Kronenzustand == "6" & Season_Jahr == "Sommer_2022"
		) %>% nrow() / length(tmp),
		
		umgefallen_y2
	)
}

str(prognose_ibf)
prognose_ibf

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
# 
# tmp <- ibf %>% filter(Season_Jahr == "Sommer_2022" & Kronenzustand == "5") %>% 
# 	select(baum_id)
# ibf %>% filter(baum_id %in% tmp & Kronenzustand == "6" & Season_Jahr == "Sommer_2023")
# tmp <- tmp$baum_id
# ibf %>% filter(baum_id %in% tmp, Season_Jahr == "Sommer_2022") %>% select(Kronenzustand)
# 
# ibf %>% filter(
# 	baum_id %in% tmp &
# 		Kronenzustand == "6" & Season_Jahr == "Sommer_2023"
# ) %>%
# 	filter(Kronenzustand != "6" & Season_Jahr == "Sommer_2022") %>% 
# 	nrow() /
# 	length(tmp)
# 
# tmp2 <- ibf %>%
# 	filter(Season_Jahr == "Sommer_2022" &
# 				 	Kronenzustand != "5" &
# 				 	baum_id %in% tmp) %>%
# 	select(baum_id) 
# # Eschen die im ersten Jahr nicht gestorben sind
# tmp2 <- tmp2$baum_id
# abgestorben_y2 <- ibf %>%
# 	filter(Kronenzustand == "5" &
# 				 	Season_Jahr == "Sommer_2023" &
# 				 	baum_id %in% tmp2) %>% 
# 	nrow() /
# 	tmp
# ibf %>%
# 	filter(Kronenzustand == "5" &
# 				 	Season_Jahr == "Sommer_2023" &
# 				 	baum_id %in% tmp2) %>% nrow()
# 
# tmp2
# ibf %>%
# 	filter(Kronenzustand == "5" &
# 				 	Season_Jahr == "Sommer_2023" &
# 				 	baum_id %in% tmp2)
