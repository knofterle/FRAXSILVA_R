#======================== LOAD TREES LAU ======================================#
# J.Osewold
# 20.04.2023
# GOOD
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here
source(file = "SCRIPT/WaldwachstumskundeDB/load_mdb_data.R")

## LIBRARYS --------------------------------------------------------------------
require(dplyr)
require(ggplot2)
## NOTES -----------------------------------------------------------------------


## LOAD DATA  ------------------------------------------------------------------
tree_lau <- load.mdb.data(path = "DATA/RAW/Goe_Lau/316313.mdb", all.column = T)
str(tree_lau)
table(tree_lau$edvid)
as.character(tree_lau[306,])

## FIND SUBSTRACT COORDINATES  -------------------------------------------------
# ECK1 und ECK2 sind auf beiden Parzellen vertreten und es handelt sich in echt
# um den selben Punkt, wenn ich also die Koordinaten dieser Punkte 
# vereinheitliche kann ich mit der Abweichung die gesamte zweite Parzelle 
# verschieben und dadurch quasi an Parzelle eins anhängen.

eck1 <- tree_lau %>%
	filter(nr.stammv == "ECK1") %>%
#	filter(edvid.stammv == "31631301") %>%
	select(x, y, edvid.stammv)
eck1 <- rbind(eck1[2:1,], eck1[2,] - eck1[1,])
eck1

eck2 <- tree_lau %>%
	filter(nr.stammv == "ECK2") %>%
	#	filter(edvid.stammv == "31631301") %>%
	select(x, y, edvid.stammv)
eck2 <- rbind(eck2[2:1,], eck2[2,] - eck2[1,])
eck2

# Differenzen bei ECK1 und ECK2 sind nah beeinander aber nicht exakt gleich, es
# es gibt eine Differenz von x = 37cm y = 5cm. Ich werde den Mittelwert nehmen
# und die gesamte 2. Parzelle verschieben.

x_shift <- mean(c(eck1$x[3], eck2$x[3]))
y_shift <- mean(c(eck1$y[3], eck2$y[3]))

## MOVE PARZ 2 -----------------------------------------------------------------
tree_lau_parz2 <- 
	tree_lau %>% 
	filter(edvid.stammv == "31631302") %>% 
	mutate(x.new = x + x_shift) %>% 
	mutate(y.new = y + y_shift)
# Drei Bäume wurden zwar aufgenommen aber später bei erstellung des Stammv nicht
# mehr gefunden, daher gibt es zu denen keine Koordinaten. Ich werde sie in 
# Zukunft auch einfach raus lassen.

tree_lau_parz2$x <- tree_lau_parz2$x.new
tree_lau_parz2$y <- tree_lau_parz2$y.new
tree_lau_parz2 <- tree_lau_parz2 %>% 
	select(! c("x.new", "y.new"))

# Nachdem tree_lau_parz2 jetzt genauso aussieht wie die alte Tabelle kann ich 
# beide mergen:
tree_lau <- rbind(tree_lau %>% filter(edvid.stammv == "31631301"), 
											tree_lau_parz2)

# Ein Test ob es so aussieht wie erwartet:
ggplot(data = tree_lau, aes(x = x, y =y)) +
	geom_point() +
	geom_point(data = tree_lau %>% filter(nr.stammv == "73"), 
						 aes(y = y, x = x, color ="red"))

## WRITE FILE ------------------------------------------------------------------
write.csv(x = tree_lau, file = "DATA/PROCESSED/GoeLau/tree_lau.csv", 
					fileEncoding = "UTF-8")

## TIDY UP  --------------------------------------------------------------------
rm(eck1, eck2, tree_lau_parz2, x_shift, y_shift)

## OUTPUT ----------------------------------------------------------------------
# tree_lau aber mit angepassten Koordinaten der 2. Parzelle



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

