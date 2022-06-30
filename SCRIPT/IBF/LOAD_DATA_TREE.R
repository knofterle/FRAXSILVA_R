############################# LOAD_DATA_TREE ###################################
# J.Osewold
# 30.03.2022
##### BEGINNING #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------

###### REQUIRES ----------------------------------------------------------------
source(file = "SCRIPT/WaldwachstumskundeDB/load_mdb_data.R")


###### NOTES -------------------------------------------------------------------
# 
# 
# 
###### LOAD TREE DISTRIBUTION AND TREE HEIGHT FROM MDB -------------------------
data_tree_scho <- load.mdb.data(
  path = "DATA/RAW/Schotten/K19302.mdb", all.column = T)
data_tree_mol <- load.mdb.data(
  path = "DATA/RAW/Mollenfelde/314313.mdb", all.column = T)
data_tree_huy <- load.mdb.data(
  path = "DATA/RAW/Huy/T18313.mdb", all.column = T)

###### LOAD TREE DISTRIBUTION AND TREE HEIGHT FROM CSV -------------------------
# The data assessed by FVA in BadenWüttenberg had to be adapted to the data 
# format used by the nw-fva.
# Adaptions are:
# - column names: $x $y $nr
# 
data_tree_weis <- read.csv2(file = "DATA/RAW/Weisweil/Stammverteilung.csv")
colnames(data_tree_weis) <-
  c("nr", "Baumart", "BHD_1", "BHD_2", "Zwiesel", "Bemerkung", "x", "y", 
    "BA_Gruppe", "Kraft_Klas")


data_tree_platt <- read.csv2(file = "DATA/RAW/Plattenwald/Stammverteilung.csv")
colnames(data_tree_platt) <-
  c("nr", "Baumart", "BHD_1", "BHD_2", "Zwiesel", "Bemerkung", "x", "y", 
    "BA_Gruppe", "Kraft_Klas")

###### TIDY UP  ----------------------------------------------------------------


###### OUTPUT ------------------------------------------------------------------
# data_tree_scho
# data_tree_mol
# data_tree_huy
# data_tree_platt
# data_tree_weis



###### JUNK --------------------------------------------------------------------
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

