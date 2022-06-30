############################# TITLE ############################################
# J.Osewold
# 30.03.22
##### UNUSED #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------

###### REQUIRES ----------------------------------------------------------------
# The package Hmisc or RODBC doesnt exist in linux or windows, therefore I had 
# to use this decision step to separate between both systems

if (.Platform$OS.type == "windows") {
  source(file = "SCRIPT/WaldwachstumskundeDB/load_mdb_windows_sep.R")
} else {
  source(file = "SCRIPT/WaldwachstumskundeDB/load_mdb_linux_sep.R")
}

# load.mdb.windows.sep ()
# load.mdb.linux.sep ()

###### NOTES -------------------------------------------------------------------
# This script defines a function that is able to merge many tables derived from 
# a mdb file. 
# 
# So far it merges "Auf", "Stammv", "Baum", 
# 
# The function is dependent on the PATH to the file, "name of the columns",
# ALL = T/F
# 


load.mdb.data <- function (path, columns, all.column) {

	###### DECIDE FOR LINUX OR WINDOWS  ------------------------------------------
	if (.Platform$OS.type == "windows") {
		auf <- load.mdb.windows.sep (path = path, name = "Auf")
		stammv <- load.mdb.windows.sep (path = path, name = "Stammv")
		baum <- load.mdb.windows.sep (path = path, name = "Baum")
	} else {
		auf <- load.mdb.linux.sep (path = path, name = "Auf")
		stammv <- load.mdb.linux.sep (path = path, name = "Stammv")
		baum <- load.mdb.linux.sep (path = path, name = "Baum")
	}
	###### STAMMV AND BAUM  ------------------------------------------------
	
	#### GENERATE ID  ------------------------------------------------
	#  I could not use the column "baumid" because not all trees have one and I dont 
	# know the reason for that. Therefore I generated a new ID column.
	
	stammv$id <- paste0(stammv$edvid, stammv$auf, stammv$nr, stammv$art)
	stammv$id <- gsub (" ", "", stammv$id) 
	# The tree numbers are formatted in different ways, therefore I had to delete 
	# all spaces
	
	baum$id <- paste0(baum$edvid, baum$auf, baum$nr, baum$art)
	baum$id <- gsub (" ", "", baum$id)
	# The tree numbers are formatted in different ways, therefore I had to delete 
	# all spaces
	
	#### CLEAN COLUMNS  --------------------------------------------------------
	# I dont want to double these columns, otherwise I would need to delete them
	# later by a different name. I hope the data is the same in baum and stammv
	baum <- subset(baum, select = -c(auf, nr, edvid, datum, Stempel, art, 
																	 baumid)) 
	
	# This column is named similar but does not include the same data..
	colnames(baum) [colnames(baum) == "bemerk"] <- "bemerk.baum"
	colnames(stammv) [colnames(stammv) == "bemerk"] <- "bemerk.stammv"

	#### MERGE BAUM AND STAMMV  ------------------------------------------------
	mdb.data <- merge(baum, stammv, by = "id", all.x = T, all.y = T)
	
	###### AUF  ----------------------------------------------
	
	auf <- subset(auf, select = -c(datum, Stempel, id, edvid))
	colnames(auf) [colnames(auf) == "bemerk"] <- "bemerk.auf"
	
	#### MERGE AUF AND THE REST  ------------------------------------------------
	mdb.data <- merge(mdb.data, auf, by = "auf", all.x = T, all.y = T)

	#### SELECT THE COLUMNS   ------------------------------------------------
	if (all.column == F) {
	  mdb.data <- subset(mdb.data, select = columns)
	}
	return(mdb.data)
}

###### TIDY UP  ----------------------------------------------------------------
rm()

###### OUTPUT ------------------------------------------------------------------
# load.mdb.data() :/PATH/to/the/file "name of thecolumns"



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

