
### Analyse des 1000ETS Datensatzes --------------------------------------------

# Johannes Osewold

################################################################################
# Einlesen der relevanten Daten
# Einlesen der mdb Archive
# Berechnung der Mortalität mit Kaplan Meier
# Erste Uebersichten werden erstellt (ETS-Stufen pro Jahr, 
                                       # Stufenveraenderung pro Jahr,
                                       # Zuwachs pro Jahr,
                                       # Nekrose - ETS Stufe)
# Einteilung der Baeume in abgestorben oder nicht
# Schaedigung nach Regionen
# Schaedigung nach Alter
# Darstellung der Konkurrenz
# Zusammenhang zwischen Nekrose und ETS-Stufe
# Zuwachs und ETS-Stufe
# Mischungsanteil und ETS-Stufe
################################################################################

rm(list = ls())
opar <- par() # Eine Kopie der standard Grafikeinstellungen
DefaultWorkingDirectory <- getwd()
setwd(DefaultWorkingDirectory)

source(file = "1.Read_Database.R", echo = T) 
source(file = "Einlesen_der_mdb_archive.R", echo = T) # Linux or Windows?
source(file = "Mortalitaet_Kaplan_Meier_1-3.R", echo = T)
source(file = "Mortalitaet_Kaplan_Meier_1-4.R", echo = T)
source(file = "Erste_Uebersichten.R", echo = T)
source(file = "Mortalitaet.R", echo = T)
source(file = "Schaedigung_nach_Region.R", echo = T)
source(file = "Schaedigung_nach_Alter.R", echo = T)
source(file = "Konkurrenz.R", echo = T)
source(file = "Nekrosen.R", echo = T)
source(file = "Zuwachs_Ets_Stufe.R", echo = T)
source(file = "Mischungsanteil.R", echo = T)
source(file = "Tolerante_Individuen.R", echo = T)










