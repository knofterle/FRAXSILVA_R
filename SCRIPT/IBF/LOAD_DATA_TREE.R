############################# LOAD_DATA_TREE ###################################
# J.Osewold
# 24.10.2022
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
# The data assessed by FVA in Baden Würtenberg had to be adapted to the data 
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

data_tree_steg <- read.csv(file = "DATA/RAW/Steglitz/IBF_BB1_Baeume_20211214.csv")
colnames(data_tree_steg) <-
	c("ID", "nr_fm", "nr", "Baumart", "Hoehe", "kraftsche_Klasse", 
		"Kronenzustand", "WR_Kron", "Bemerkung", "x", "y", "DBH_mm", "BHD_cm", 
		"Height_m", "Status", "Schicht", "BA_Gruppe")

data_tree_mon <- read.csv(file = "DATA/RAW/Monheim/IBF_BY1_Baeume_20211214.csv")
colnames(data_tree_mon) <- 
	c("ID", "nr", "x", "y", "z", "wgs84_x", "wgs84_y", "wgs84_z", "ID2", 
		"ID_nummer", "BA_Gruppe", "Baum_ID", "Baumart", "Umfang", "BHD",
		"Kraft_Klas", "Bemerkung")

data_tree_grfw <-
	read.csv(file = "DATA/RAW/IBF/Greifswald/IBF_MV1_Baeume_20220228.csv")

data_tree_leu <- 
	read.csv(file = "DATA/RAW/IBF/Leutzsch/Baeume_SN1_Leutzsch_UTM_Koordinaten.csv")

data_tree_bie <- 
	read.csv(file = "DATA/RAW/IBF/Bienhof/Baeume_SN2_Bienhof_UTM_Koordinaten.csv")

data_tree_ett_a <- 
	read.csv(file = "DATA/RAW/IBF/Ettersberg/IBF_TH1a_Baeume_20211216.csv")

data_tree_ett_b <- 
	read.csv(file = "DATA/RAW/IBF/Ettersberg/IBF_TH1b_Baeume_20211216.csv")


###### LOAD TREE DISTRIBUTION AND TREE HEIGHT FROM shape -------------------------


###### TIDY UP  ----------------------------------------------------------------


###### OUTPUT ------------------------------------------------------------------
# data_tree_scho
# data_tree_mol
# data_tree_huy
# data_tree_platt
# data_tree_weis
# data_tree_steg
# data_tree_mon
# data_tree_grfw
# data_tree_leu
# data_tree_noe
# data_tree_ett_a
# data_tree_ett_b



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

