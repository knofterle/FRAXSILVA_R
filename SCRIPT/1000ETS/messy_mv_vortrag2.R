#============================ TITLE ===========================================#
# J.Osewold
# 10.10.23
# RENEW
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here
# 
# 
# 

## LIBRARYS --------------------------------------------------------------------

## NOTES -----------------------------------------------------------------------
# Ich versuche das Skript "messy_mv_vortrag wieder zum Laufen zu bringen und 
# vernünftig zu dokumentieren.

## STEP 1  ---------------------------------------------------------------------

count_non_na <- function(x) {sum(!is.na(x))}
count_non_na(ETSStufen_1000_clean$`2015`) # Test

# Diese Tabelle listet für jedes Jahr die Anzahl der Boniturmesswerte, quasi 
# die Anzahl der Bäume
live_count <- apply(ETSStufen_1000_clean, 2, count_non_na)

# Diese Tabelle listet alle Bäume die pro Jahr final gestorben sind. Die Spalte
# tot_XX diente vor allem dazu "echte" 5er die nicht wieder auferstanden sind von
# den "falschen" 5ern zu unterscheiden. Die Spalte lässt leider keinen Rückschluss
# auf den Grund des Ausscheidens zu.
# Mir ist nicht ganz klar warum ich die Zahl der Toten pro Jahr durch 2 geteilt 
# habe.
death_count <- apply(Tote_1000, 2 ,sum)
death_count <- death_count/2
death_count <- c(0, death_count)
death_count

# Für jedes Jahr wird die Zahl der Lebenden und der neu Gestorbenen aufgelistet
# Außerdem das Verhältnis der beiden zueinander.
live_count <- as.data.frame(live_count)
live_count$death <- death_count
colnames(live_count) <- c("total", "death")
live_count$ratio <- live_count$death / live_count$total
live_count$year <- rownames(live_count)

# Hier wird die Anzahl der lebenden für jedes Jahr als Barplot dargestellt.
library(ggplot2)
ggplot(data = live_count) +
	geom_col(aes(x = year, y = total))



# Diese Skript diente hauptsächlich dazu einen Barplot mit den verbleibenden,
# gestorbenen und entnommenen Eschen für jedes Jahr zu basteln. Aber das klappt 
# hinten und vorne nicht. Die entnommenen sind nicht wirklich die entnommenen...
# 
# Ich habe die folgenden Annahme getroffen: Für den Kaplan Meier werden alle 
# Bäume die entnommen wurden und sehr schlecht aussahen als natürlich gestorben
# angesehen und bekommen den mortstatus 1. Alle anderen entnommenen werden 
# zensiert, mort_status enthält also alle Bäume von denen wir glauben, dass sie
# aus ETS oder Natur gestorben sind. Aber tote_1000 enthält leider nicht alle 
# toten sondern wieder nur diejenigen 5er die dann auch tot geblieben sind.
# 8er bekommen bei tote_1000 ein NA. Das war vielleicht nicht ganz so klug...
# 
# 
# Aber im Grunde ist diese Darstellung eine sehr gute Idee


# 
# mort_status$observ_time
# 
table(mort_status$status)
table(mort_status$observ_time)



table <- as.data.frame(c(2013:2023))
colnames(table) <- "year"
table$death <- 0
table$total <- 0
for (i in 11:2) {
	table$death[i-1] <- sum(mort_status$status[mort_status$observ_time == i])
	table$total[i-1] <- length(mort_status$status[mort_status$observ_time == i]) +
		table$total[i]
}
table
live_count

table$ratio <- table$death / table$total
table
death_count
death_count <- apply(Tote_1000, 2 ,sum)
table$entnommen <- c(death_count, 0, 0)

ggplot(data = table[1:9,]) +
	geom_col(aes(x = year, y = total), fill = "gray") +
	geom_col(aes(x = year, y = entnommen), fill = "black") +
	geom_col(aes(x = year, y = death), fill = "red") +
	scale_x_continuous(breaks = c(2013:2021))

## STEP 2  ---------------------------------------------------------------------

# Fälle: 
# 




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

