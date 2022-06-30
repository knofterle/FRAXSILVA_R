################ MOTATILAET KAPLAN MEIER 1-4 ###################################
# J.Osewold
# 17.12.21
##### DRAFT #####
################################################################################

###### LIBRARYS -------------------------------------------------------------------
library(dplyr)

###### REQUIRES -------------------------------------------------------------------
# This runs all previously necessary scripts
source(file = "SCRIPT/1000ETS/load_data_1000.R")

# data
# ETSStufen
# Tote

###### NOTES -------------------------------------------------------------------


###### SORTIEREN IN TOT ODER NICHT UND ZEITPUNKT -------------------------------

# Alle noch lebenden werden als im letzten Jahr zensiert behandelt
# Die Idee ist, dass jede Variante von abgstorben oder zensiert, mit if Befehlen
# abgefragt wird. Der lock-Wert soll eine doppelung von zwei if-Treffern 
# aufdecken (dies funktioniert nach einem Test mit 5.2 und 9 auch)
# 
# mort_status$status = 1 bedeutet das Ereignis (tot) ist eingetreten
# 
# Als an ETS gestorben werden alle Eschen definiert, die vor einer 8 bei 5 
# gestanden haben 
# 
# Die 9 in den ets stufen kann nur vorkommen wenn danach keine anderen Stufen 
# mehr festgestellt wurden. Das entspricht daher auch einer zensierung.
# Wenn zwischendurch eine Messung ausgefallen ist wurde statt dessen eine 17 
# eingefügt. z.B 1 3 4 17 17 2 1 oder 1 3 4 9 9 aber NIE 1 3 4 9 9 2 1 

mort_status <- data.frame(matrix(nrow = nrow(data_1000), ncol = 6))
colnames(mort_status) <- c("edvid", "bnr", "age2013", "age", 
                           "observ_time", "status")
error <- c()

for (row in 1:nrow(data_1000)) {
  ets <- ETSStufen_1000[row,]
  dead <- Tote_1000 [row,]
  pos <- NA
  lock <- 0
  if (8 %in% ets) { 
    pos <- first(which(ets == 8))
    if (ets[pos-1] %in% c(5)) { # 3 2 1 5 8 NA NA
      mort_status$status[row] <- 1
      lock <- lock +1
    }
    if (ets[pos-1] %in% c(1:4,17)) { # 3 1 2 1 8 NA NA
      mort_status$status[row] <- 0
      lock <- lock +1
    }
  } 
  if (9 %in% ets) { # 3 1 3 9 9 9 NA NA 
    pos <- first(which(ets == 9))
    mort_status$status[row] <- 0
    lock <- lock +1
  } 
  if (2 %in% dead) { # 3 1 3 5 5.2 NA 
    pos <- first(which(dead == 2)) + 1
    mort_status$status[row] <- 1
    lock <- lock +1
  } 
  # Hier werden alle gefiltert, die irgendwann NAs enthalten, wenn davor keine 
  # 5 oder 8 enthalten ist, wurde der Baum in dieser Zeit nicht mehr beobachtet
  # und wird entsprechend des Alters zu der Zeit zensiert. 
  # Alle anderen die keine NAs enthalten und vorher noch keinen status bekommen 
  # haben entsprechend dort auch nur NA enthalten muessen folglich im letzten 
  # Jahr immer noch leben. 
  if (any(is.na(ets))) {
    if (ets[first(which(is.na(ets)))-1] %in% c(5,8)) { # 3 1 3 2 5.2 NA
      
    } else { # 3 1 2 NA NA NA 
      pos <- first(which(is.na(ets)))
      mort_status$status[row] <- 0
      lock <- lock +1
    }
  } else { # 3 1 2 4 1 3 2 
    if (is.na(mort_status$status[row])) {
      pos <- 10 # fuer das jahr 2021
      mort_status$status[row] <- 0
      lock <- lock +1
    }
  }
  
  mort_status$edvid[row] <- data_1000$edv_id[row]
  mort_status$bnr[row]   <- data_1000$bnr[row]
  mort_status$age[row]   <- data_1000$alt_ets13[row] + pos -1
  mort_status$age2013 [row] <- data_1000$alt_ets13 [row]
  mort_status$observ_time[row] <- pos
  
  # Dies dient lediglich der einfacheren Fehlersuche, die Zeilen werden 
  # angezeigt und es wird nach doppelten Fällen gefiltert
  print(pos)
  print(row)
  if (lock > 1) {
    error <- c(error,row)
  }
  if (lock == 0) {
    error <- c(error,row)
  }
}

rm(error, pos, row, lock, dead)

###### MANUELLE KALKULATION STERBERATE -----------------------------------------

age <- 1.6: 200.6 # Die Alter in den Ursprungsdaten sind bereits auf die Saison 
# berechnet
kp <- as.data.frame(age)
kp$n <- 0
kp$dead <- 0

#  Fuer jeden Baum in ETS Datenbank werden alle Jahre, die dieser aufgenommen
# wurde bestimmt und fuer jedes das entsprechende Jahr der Zaehler n bzw. dead 
# um eins erhöht

for (i in 1:nrow(mort_status)) {
  kp$n [mort_status$age2013[i]:mort_status$age[i]] <- 
    kp$n [mort_status$age2013[i]:mort_status$age[i]] +1
  
  if (mort_status$status[i] == 1) {
    kp$dead[mort_status$age[i]] <- kp$dead[mort_status$age[i]] +1
  }
}

kp$percent <- kp$dead/kp$n

#  Eine fiktive Population wird von 100 beginnend jedes Jahr um die 
# Sterblichkeitsrate verringert

kp$reduction <- 100
kp$percent[is.na(kp$percent)] <- 0
for (i in 1:(nrow(kp)-1)) {
  kp$reduction[i+1] <- kp$reduction[i]*(1-kp$percent[i+1])
}

###### DARSTELLUNG STERBERATE --------------------------------------------------

pdf(file = "EXPORT/1000ETS/FIGURES/Kaplan_Meier_1-4.pdf",
    width=14, height=7, paper='special')

par(mar = c(5, 5, 3, 5))
plot(kp$age, kp$dead, type ="p", ylab = "Anzahl",
     main = "Kaplan Meier Eschentriebsterben", xlab = "Alter",
     col = "blue", ylim = c(0, 230), pch = 20)
par(new = TRUE)
plot(kp$age, kp$n, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "red", lty = 2, ylim = c(0, 230))

par(new = TRUE)
plot(kp$age, kp$reduction, type = "l", xaxt = "n", yaxt = "n",
     ylab = "", xlab = "", col = "black", lty = 1, lwd = 2, ylim = c(0,100))
axis(side = 4)
mtext("Populationsverlauf", side = 4, line = 3)
legend("topright", c("Populationsverlauf", "Anzahl Messwerte", "Anzahl abgestorbener"),
       col = c("black","red", "blue"), lty = c(1, 2, 0), lwd = c(2, 1, NA), 
       pch = c(NA, NA, 20))
dev.off()

#  Dieser Graph dient nur dazu in einer Powerpoint die Teile des Plots einzeln
# darzustellen
pdf(file = "EXPORT/1000ETS/FIGURES/Kaplan_Meier_Datenabdeckung.pdf",
    width=14, height=7, paper='special')
par(mar = c(5, 5, 3, 5))
plot(kp$age, kp$n, type = "l",
     ylab = "Anzahl", xlab = "Alter", col = "red", lty = 2, ylim = c(0, 230))
dev.off()


### TIDY UP  -------------------------------------------------------------------
rm(age, i, ets)

###### OUTPUT ------------------------------------------------------------------
# kp
# mort_status
# diverse Plots...
# 
###### JUNK ------------------------------------------------------------------

# 
# mort_status <- data.frame(matrix(nrow = nrow(data), ncol = 6))
# colnames(mort_status) <- c("edvid", "bnr", "age2013", "age", "observ_time", 
#                         "status")
# error <- c()

# for (row in 1:nrow(data)) {
#   ets <- ETSStufen[row,]
#   dead <- Tote [row,]
#   pos <- NA
#  lock <- 0
#  if (8 %in% ets) {
#    pos <- first(which(ets == 8))
#     if (ets[pos-1] %in% c(4,5)) {
#      mort_status$status[row] <- 1
#       lock <- lock +1
#    }
#    if (ets[pos-1] %in% c(1:3,17)){
#      mort_status$status[row] <- 0
#     lock <- lock +1
#    }
#  }
#  if (9 %in% ets){
#    pos <- first(which(ets == 9))
#     mort_status$status[row] <- 0
#    lock <- lock +1
#   }
#  if (2 %in% dead){
#    pos <- first(which(dead == 2))+1
#    mort_status$status[row] <- 1
#    lock <- lock +1
#   }
#  mort_status$edvid[row] <- data$edv_id[row]
#   mort_status$bnr[row]   <- data$bnr[row]
#   mort_status$age[row]   <- data$alt_ets13[row] + pos -1
#  mort_status$age2013 [row] <- data$alt_ets13[row]
#  #  mort_status$observ_time[row] <- pos
#  print(pos)
#   print(row)
#   if (lock > 1) {
#     error <- c(error,row)
#   }
# }
# rm(error, pos, row, lock, dead)
# 

# ######## Eine ältere Heransgehensweise um einen Überblick zu gewinnen ##########
# 
# 
# library(pals)
# library(ggpubr)
# 
# sp <- autoplot(km_fit, xlim = c(-10,160))                                        
# xplot <- ggdensity(mort_status$age, fill = "grey", xlim = c(-10,160)) +
# theme(axis.line = element_blank(), 
#       axis.ticks = element_blank(), axis.text.x = element_blank(), 
#      axis.text.y = element_blank(), axis.title.x = element_blank())
# k <- ggarrange(xplot, sp, 
#           nrow = 2,  align = "hv", 
#           heights = c(1, 2),
#           common.legend = F,
#           vjust = c(-5))
# pdf(file = "Kaplan-Meier_density.pdf",
#     width=10, height=5, paper='special')
# k
# dev.off()
# 
# timespan <- matrix(c(data$alt_ets13, data$alt_ets13+7), ncol = 2)
# timespan <- as.data.frame(timespan)
# timespan[,3] <- as.factor(data$region)
# colnames(timespan) <- c("begin","end","region")
# timespan_unique <- as.data.frame(unique(timespan[,1:2]))
# timespan_unique$region <- timespan[rownames(timespan_unique),3]
# timespan_unique <- timespan_unique[order(timespan_unique[,1],
#                                          decreasing = F),]
# timespan_unique$groups <- c(1:nrow(timespan_unique))
# timespan_unique <- timespan_unique[,c(4,3,1,2)]
# levels(timespan_unique$region) <- c("Goettingen", "Elm", "KF Lauenburg", 
#                                     "FoA Schotten")
# 
# j <- ggplot(timespan_unique, aes())
# j <- j + geom_errorbar(aes(y = groups, xmin = begin, xmax = end))
# j <- j + geom_rect(mapping = aes(xmin = begin, ymin = 0,
#                                  xmax = end, ymax = 35, fill = region), 
#                    alpha = 0.5)
# j <- j + labs(x = "age", y = "")
# 
# # j <- j + theme(axis.ticks.y = element_blank(),  
# #         axis.text.y = element_blank(), plot.background = element_blank())
# pdf(file = "Kaplan-Meier_zeitliche-Abdeckung.pdf",
#     width=10, height=5, paper='special')
# j + scale_fill_brewer(palette="Accent")
# dev.off()
# 
# rm(timespan, timespan_unique, df, ETSStufen999, i, j, k, p , sp, kp,
#    km_fit, xplot, km, Overview, age, ets)
# 
# setwd(DefaultWorkingDirectory)
# 
##### Anwendung Kaplan Meier ---------------------------------------------------
# Overview <- Surv(mort_status$age, mort_status$status)
# # ERSTE VERSION 
# 
# km <- with(mort_status, Surv(observ_time, status))
# km_fit <- survfit(Surv(observ_time, status) ~ 1, data=mort_status)
# pdf(file = "Kaplan-Meier_ObservTime.pdf",
#     width=10, height=5, paper='special')
# autoplot(km_fit)
# dev.off()
# 
# km_fit <- survfit(Surv(age, status) ~ 1, data=mort_status)
# pdf(file = "Kaplan-Meier_Age.pdf",
#     width=10, height=5, paper='special')
# autoplot(km_fit)
# dev.off()
# 
# # ZWEITE VERSION
# 
# km_fit <- survfit(Surv(observ_time, status) ~ 1, data=mort_status)
# pdf(file = "Kaplan-Meier_ObservTime_censored.pdf",
#     width=10, height=5, paper='special')
# autoplot(km_fit)
# dev.off()
# 
# km_fit <- survfit(Surv(age, status) ~ 1, data=mort_status)
# pdf(file = "Kaplan-Meier_Age_censored.pdf",
#     width=10, height=5, paper='special')
# autoplot(km_fit)
# dev.off()


###### Vergleich der manuellen Berechnung mit einer Berechnung des Surv-Paketes#####

# pdf(file = "EXPORT/FIGURES/Kaplan-Meier_linkstrunkiert.pdf",
#     width=14, height=7, paper='special')
# autoplot(survfit(Surv(time = mort_status$age2013, time2 = mort_status$age, 
#                       event = mort_status$status) ~ 1))
# dev.off()