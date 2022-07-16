#============================ structure tree data =============================#
# J.Osewold
# 14.07.22
# MESS
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/IBF/LOAD_DATA_TREE.R")

## LIBRARYS --------------------------------------------------------------------
library(dplyr)
library(sf)
library(sfheaders)
## NOTES -----------------------------------------------------------------------
# Dieses Skript soll ein paar Basisinformationen aus den Baumdaten der IBF holen
# 
# Vermutlich wird jede Fläche einen eigenes Kapitel bekommen


## BB Steglitz  ---------------------------------------------------------------------
str(data_tree_steg)
tmp <- data_tree_steg %>% 
	filter (Baumart == "Fraxinus excelsior")
nrow(tmp) # 112
nr <- c(13,14,15,18,19,104,106,107,113,115,117,119,120,121,122,123,124,125,126,128,129,130,131,132,133,134,135,136,137,
138,139,160,161,162,163,164,165,166,167,168,169,170,200,201,202,208,209,210,211,
212,213,214,215,216,217,224,225,226,227,228,235,236,
237,238,239,1,2,3,4,5,6,7,8,9,10,11,
12,142,143,144,145,146,150,151,152,153,154,155,156,157,159,
171,172,173,174,175,176,177,178,179,180,181,182,183,184,185,186,187,188,189,
191,192,193,194,195,196,197,198,199,221,
222,223,
229,234)
nr[!nr %in% intersect(tmp$nr, nr)] # Es scheint als wären die Bäume 1:12 irgendwie
# dazu gemischt worden. Dazu gibt es in den Bonituren auch keine BHDs oder ähnliches

range(tmp$BHD_cm) # 20 101
dbh10 <- tmp %>% 
	select(BHD_cm) %>% 
	arrange(desc(BHD_cm)) %>% 
	slice(1:11)
dbh <- dbh10 ^2 /4 * pi
sqrt(mean(dbh$BHD_cm) / pi * 4) # 66.8

tmptd <- tmp$BHD_cm ^2 /4 * pi
sqrt(mean(tmptd) / pi * 4) # 42.6

table(data_tree_steg$Baumart)
tmp <- data_tree_steg
tmp$g <- tmp$BHD_cm ^2 /4 * pi
g <- tmp %>% 
	group_by(Baumart) %>% 
	summarise(g = sum(g, na.rm = T))
g$percent <- g$g/sum(g$g)
g

## BB Steglitz JUNK  ---------------------------------------------------------------------

# plot(data_tree_steg$x, data_tree_steg$y, )
# table(data_tree_steg$Bemerkung)
# 
# data_tree_steg %>% 
# #	filter(Bemerkung == "Probebäume") %>% 
# #	select(x,y) %>% 
# 	ggplot(aes(x = x, y = y, color = Bemerkung, label = nr)) +
# 	geom_point() + 
# 	geom_label()
# 
# # 13 14 15 18 19  exclude
# # 46 149 219 266 124 corners a
# # 77 113 178 261 123 corners b
# 
# borders <- st_sfc(sf::st_polygon(
# 	list(
# 		cbind(
# 		data_tree_steg$x[c(46, 149, 219, 266, 124, 46)],
# 		data_tree_steg$y[c(46, 149, 219, 266, 124, 46)])
# 	)
# )
# )
# 
# points <-
# 	sfheaders::sf_multipoint(sort(data_tree_steg),
# 													 x = "x",
# 													 y = "y",
# 													 multipoint_id = "nr",
# 													 keep = T)
# points <- st_as_sf(data_tree_steg, coords = c("x", "y"), dim = "XY")
# class(points)
# points
# points <- st_combine(points)	
# rest <- st_intersection(borders, points)
# rest <- st_contains(borders, points)
# rest <-  points[borders,] # DAS HIER FUNKTIONIERT; ES IST SOOO DUMMM!!!!!



## SCHOTTEN  --------------------------------------------------------------------
str(data_tree_scho)
tmp <- data_tree_scho %>% 
	filter(art == 311) %>% 
	filter(auf == max(auf)) %>% 
	filter( d != -999)

range(tmp$d)
dbh10 <- tmp %>% 
	select(d) %>% 
	arrange(desc(d)) %>% 
	slice(1:5)
dbh <- dbh10 ^2 /4 * pi
sqrt(mean(dbh$d) / pi * 4) # 843

tmptd <- tmp$d ^2 /4 * pi
sqrt(mean(tmptd) / pi * 4) # 535

## MOLLENFELDE  --------------------------------------------------------------------
tmp <- data_tree_mol %>% 
	filter(art == 311) %>% 
	filter(auf == max(auf)) %>% 
	filter( d != -999)
nrow(tmp) # 111
range(tmp$d) # 71 680

dbh10 <- tmp %>% 
	select(d) %>% 
	arrange(desc(d)) %>% 
	slice(1:11)
dbh <- dbh10 ^2 /4 * pi
sqrt(mean(dbh$d) / pi * 4) # 579

tmptd <- tmp$d ^2 /4 * pi
sqrt(mean(tmptd) / pi * 4) # 384

## HUY  --------------------------------------------------------------------
tmp <- data_tree_huy %>% 
	filter(art == 311) %>% 
	filter(auf == max(auf)) %>% 
	filter( d != -999)
nrow(tmp) # 104
range(tmp$d) # 68 420

dbh10 <- tmp %>% 
	select(d) %>% 
	arrange(desc(d)) %>% 
	slice(1:10)
dbh <- dbh10 ^2 /4 * pi
sqrt(mean(dbh$d) / pi * 4) # 386

tmptd <- tmp$d ^2 /4 * pi
sqrt(mean(tmptd) / pi * 4) # 261

## BW_2 WEISWEIL  --------------------------------------------------------------------
str(data_tree_weis)
tmp <- data_tree_weis %>% 
	filter(Baumart == "Es") 
nrow(tmp) # 94
tmp$d <- (tmp$BHD_1 + tmp$BHD_2)/2
tmp$d[tmp$BaumNr == 179] <- 14 # Tippfehler bei BHD_2
tmp <- tmp %>% 
	filter(d >= 7)
range(tmp$d, na.rm = T) # 7.3 77.0

dbh10 <- tmp %>% 
	select(d) %>% 
	arrange(desc(d)) %>% 
	slice(1:9)
dbh <- dbh10 ^2 /4 * pi
sqrt(mean(dbh$d) / pi * 4) # 64.0

tmptd <- tmp$d ^2 /4 * pi
sqrt(mean(tmptd) / pi * 4) # 41.5

# Baumartenzusammmensetzung
table(data_tree_weis$Baumart)
tmp <- data_tree_weis
tmp$d <- (tmp$BHD_1 + tmp$BHD_2)/2
tmp$d[tmp$BaumNr == 179] <- 14 # Tippfehler bei BHD_2
tmp$d[tmp$BaumNr == 323] <- 8.5 # Tippfehler 
tmp$d[tmp$BaumNr == 57] <- 24 # Tippfehler 
tmp$d[tmp$d < 7] <- NA
tmp$g <- tmp$d ^2 /4 * pi
g <- tmp %>% 
	group_by(Baumart) %>% 
	summarise(g = sum(g, na.rm = T))
g$percent <- g$g/sum(g$g)
g

## BW_1 Plattenwald  --------------------------------------------------------------------

str(data_tree_platt)
tmp <- data_tree_platt %>% 
	filter(Baumart == "Es") 
nrow(tmp) # 235
tmp$d <- (tmp$BHD_1 + tmp$BHD_2)/2
tmp$d[tmp$BaumNr == 462] <- 35.5 # Tippfehler bei BHD_2
tmp$d[tmp$BaumNr == 206] <- NA # keine Messung
tmp <- tmp %>% 
	filter(d >= 7)
range(tmp$d, na.rm = T) # 7 52

dbh10 <- tmp %>% 
	select(d) %>% 
	arrange(desc(d)) %>% 
	slice(1:23)
dbh <- dbh10 ^2 /4 * pi
sqrt(mean(dbh$d) / pi * 4) # 42.1

tmptd <- tmp$d ^2 /4 * pi
sqrt(mean(tmptd) / pi * 4) # 27.5

# Baumartenzusammmensetzung
table(data_tree_platt$Baumart)
tmp <- data_tree_platt
tmp$d <- (tmp$BHD_1 + tmp$BHD_2)/2
tmp$d[tmp$BaumNr == 462] <- 35.5 # Tippfehler bei BHD_2
tmp$d[tmp$BaumNr == 206] <- NA # keine Messung
tmp$d[tmp$d < 7] <- NA
tmp$g <- tmp$d ^2 /4 * pi
g <- tmp %>% 
	group_by(Baumart) %>% 
	summarise(g = sum(g, na.rm = T))
g$percent <- g$g/sum(g$g)
g

## BY_1 Monheim  --------------------------------------------------------------------
str(data_tree_mon)
tmp <- data_tree_mon %>% 
	filter(Baumart == "Esche")
nrow(tmp) # 224
tmp$BHD[tmp$BHD < 7] <- NA
range(tmp$BHD, na.rm = T) # 7.0 78.0

dbh10 <- tmp %>% 
	select(BHD) %>% 
	arrange(desc(BHD)) %>% 
	slice(1:22)
dbh <- dbh10 ^2 /4 * pi
dbh
sqrt(mean(dbh$BHD) / pi * 4) # 59.9

tmptd <- tmp$BHD ^2 /4 * pi
sqrt(mean(tmptd, na.rm = T) / pi * 4) # 38.6

# Baumartenzusammmensetzung
table(data_tree_mon$Baumart)
tmp <- data_tree_mon
tmp$d <- tmp$BHD
# tmp$d[tmp$BaumNr == 462] <- 35.5 # Tippfehler bei BHD_2
# tmp$d[tmp$BaumNr == 206] <- NA # keine Messung
tmp$d[tmp$d < 7] <- NA
tmp$g <- tmp$d ^2 /4 * pi
g <- tmp %>% 
	group_by(Baumart) %>% 
	summarise(g = sum(g, na.rm = T))
g$percent <- g$g/sum(g$g)
g

## BY_2 Bruckberg  --------------------------------------------------------------------
# Hier gibt es fast keine Daten, daher nur in der Excel gearbeitet
## BY_3 Isen  --------------------------------------------------------------------
# Hier gibt es fast keine Daten, daher nur in der Excel gearbeitet

## MV_1 Greifswald  --------------------------------------------------------------------
str(data_tree_grfw)
colnames(data_tree_grfw) [colnames(data_tree_grfw) == "zdztfzzt_T.C.254"] <-
	"Baumart"
colnames(data_tree_grfw) [colnames(data_tree_grfw) == "BHD.N.19.11"] <-
	"BHD"
table(data_tree_grfw$BHD)
tmp <- data_tree_grfw %>% 
	filter(Baumart == "Gem. Esche")

nrow(tmp) # 123
tmp$BHD[tmp$BHD < 7] <- NA
range(tmp$BHD, na.rm = T) # 15.5 91.3

dbh10 <- tmp %>% 
	select(BHD) %>% 
	arrange(desc(BHD)) %>% 
	slice(1:12)
dbh <- dbh10 ^2 /4 * pi
dbh
sqrt(mean(dbh$BHD) / pi * 4) # 83.7

tmptd <- tmp$BHD ^2 /4 * pi
sqrt(mean(tmptd, na.rm = T) / pi * 4) # 63.7

# Baumartenzusammmensetzung
table(data_tree_grfw$Baumart)
tmp <- data_tree_grfw
tmp$d <- tmp$BHD
# tmp$d[tmp$BaumNr == 462] <- 35.5 # Tippfehler bei BHD_2
# tmp$d[tmp$BaumNr == 206] <- NA # keine Messung
tmp$d[tmp$d < 7] <- NA
tmp$g <- tmp$d ^2 /4 * pi
g <- tmp %>% 
	group_by(Baumart) %>% 
	summarise(g = sum(g, na.rm = T))
g$percent <- g$g/sum(g$g)
g

## SN_1  Leutzsch --------------------------------------------------------------------
str(data_tree_leu)
tmp <- data_tree_leu %>% 
	filter(Bezeichnung == "GES")
nrow(tmp) # 38
tmp$BHD[tmp$BHD < 7] <- NA
range(tmp$BHD, na.rm = T) # 19 113

dbh10 <- tmp %>% 
	select(BHD) %>% 
	arrange(desc(BHD)) %>% 
	slice(1:4)
dbh <- dbh10 ^2 /4 * pi
dbh
sqrt(mean(dbh$BHD) / pi * 4) # 101.7

tmptd <- tmp$BHD ^2 /4 * pi
sqrt(mean(tmptd, na.rm = T) / pi * 4) # 68.4

# Baumartenzusammmensetzung
table(data_tree_leu$Bezeichnung)
tmp <- data_tree_leu
tmp$d <- tmp$BHD
# tmp$d[tmp$BaumNr == 462] <- 35.5 # Tippfehler bei BHD_2
# tmp$d[tmp$BaumNr == 206] <- NA # keine Messung
tmp$d[tmp$d < 7] <- NA
tmp$g <- tmp$d ^2 /4 * pi
g <- tmp %>% 
	group_by(Bezeichnung) %>% 
	summarise(g = sum(g, na.rm = T))
g$percent <- g$g/sum(g$g)
g

## SN_2 Bienhof   --------------------------------------------------------------------
str(data_tree_bie)
tmp <- data_tree_bie %>% 
	filter(Baumart == "GES")
nrow(tmp) # 175
tmp$BHD[tmp$BHD < 7] <- NA
range(tmp$BHD, na.rm = T) # 7.5 70.0

dbh10 <- tmp %>% 
	select(BHD) %>% 
	arrange(desc(BHD)) %>% 
	slice(1:18)
dbh <- dbh10 ^2 /4 * pi
dbh
sqrt(mean(dbh$BHD) / pi * 4) # 41.0

tmptd <- tmp$BHD ^2 /4 * pi
sqrt(mean(tmptd, na.rm = T) / pi * 4) # 24.1

# Baumartenzusammmensetzung
table(data_tree_bie$Baumart)
tmp <- data_tree_bie
tmp$d <- tmp$BHD
# tmp$d[tmp$BaumNr == 462] <- 35.5 # Tippfehler bei BHD_2
# tmp$d[tmp$BaumNr == 206] <- NA # keine Messung
tmp$d[tmp$d < 7] <- NA
tmp$g <- tmp$d ^2 /4 * pi
g <- tmp %>% 
	group_by(Baumart) %>% 
	summarise(g = sum(g, na.rm = T))
g$percent <- g$g/sum(g$g)
g

## TH_1 Ettersberg   --------------------------------------------------------------------
data_tree_ett <- rbind(data_tree_ett_a, data_tree_ett_b)
str(data_tree_ett)
colnames(data_tree_ett) [7:8] <- c("Baumart", "BHD")
tmp <- data_tree_ett %>% 
	filter(Baumart == "ES")
nrow(tmp) # 152 # aber die Daten wurden zu spät aufgenommen, die Flächen hatten
# ursprüpnglich mal etwa 180 Eschen
tmp$BHD[tmp$BHD < 7] <- NA
range(tmp$BHD, na.rm = T) # 11 48

dbh10 <- tmp %>% 
	select(BHD) %>% 
	arrange(desc(BHD)) %>% 
	slice(1:15)
dbh <- dbh10 ^2 /4 * pi
dbh
sqrt(mean(dbh$BHD) / pi * 4) # 39.2

tmptd <- tmp$BHD ^2 /4 * pi
sqrt(mean(tmptd, na.rm = T) / pi * 4) # 27.5

# Baumartenzusammmensetzung
table(data_tree_ett$Baumart)
tmp <- data_tree_ett
tmp$d <- tmp$BHD
# tmp$d[tmp$BaumNr == 462] <- 35.5 # Tippfehler bei BHD_2
# tmp$d[tmp$BaumNr == 206] <- NA # keine Messung
tmp$d[tmp$d < 7] <- NA
tmp$g <- tmp$d ^2 /4 * pi
g <- tmp %>% 
	group_by(Baumart) %>% 
	summarise(g = sum(g, na.rm = T))
g$percent <- g$g/sum(g$g)
g

## TH_2 Schwansee   --------------------------------------------------------------------
# Keine Daten verfügbar

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

