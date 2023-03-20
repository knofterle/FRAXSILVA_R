############################# LOAD MDB DATA ####################################
# J.Osewold
# 02.03.2022
##### NEW #####
################################################################################

###### LIBRARYS ----------------------------------------------------------------

###### REQUIRES ----------------------------------------------------------------
source(file = "SCRIPT/WaldwachstumskundeDB/load_mdb_data.R")

###### NOTES -------------------------------------------------------------------
# This works for windows and linux in .mdb format

###### LOAD THE DATA FROM 309313------------------------------------------------
tree_data <- load.mdb.data(path = "DATA/RAW/Goe_Lau/309313.mdb", all.column = T) 
tree_data_polter <- tree_data [tree_data$edvid %in% c(30931301, 30931302), ]
tree_data_ansitz <- tree_data [tree_data$edvid %in% c(30931303, 30931304), ]

###### OBSOLETE AND SHOULD BE REMOVED ONCE -------------------------------------

# linux
# baum <- load.mdb.linux.sep(path = "DATA/RAW/Goe_Lau/309313.mdb", name = "Baum") 
# baum_polter <- baum [baum$edvid %in% c(30931301, 30931302), ]
# baum_ansitz <- baum [baum$edvid %in% c(30931303, 30931304), ]
# 
# stammv <- load.mdb.linux.sep(path = "DATA/RAW/Goe_Lau/309313.mdb", name = "Stammv") 
# stammv_polter <- stammv [stammv$edvid %in% c(30931301, 30931302), ]
# stammv_ansitz <- stammv [stammv$edvid %in% c(30931303, 30931304), ]

# # windows
# baum <- load.mdb.windows.sep(path = "DATA/RAW/Goe_Lau/309313.mdb", name = "Baum") 
# baum_polter <- baum [baum$edvid %in% c(30931301, 30931302), ]
# baum_ansitz <- baum [baum$edvid %in% c(30931303, 30931304), ]
# 
# stammv <- load.mdb.windows.sep(path = "DATA/RAW/Goe_Lau/309313.mdb", name = "Stammv") 
# stammv_polter <- stammv [stammv$edvid %in% c(30931301, 30931302), ]
# stammv_ansitz <- stammv [stammv$edvid %in% c(30931303, 30931304), ]
# 

###### TIDY UP  ----------------------------------------------------------------
rm(tree_data)

###### OUTPUT ------------------------------------------------------------------
# stammv_polter, baum_polter, stammv_ansitz, baum_ansitz, tree_data_polter, 
# tree_data_ansitz


###### JUNK --------------------------------------------------------------------

# stammv_polter <- stammv [stammv$edvid %in% c("30931301", "30931302"), ]
# baum_polter <- baum [baum$edvid %in% c("30931301", "30931302"), ]
# stammv_ansitz <- stammv [stammv$edvid %in% c("30931303", "30931304"), ] 
# baum_ansitz <- baum [baum$edvid %in% c("30931303", "30931304"), ]
# #### MERGE STAMMV AND BAUM -----------------------------------------------------
# 
# #  I could not use the column "baumid" because not all trees have one and I dont 
# # know the reason for that. Therefore I generated a new ID column.
# stammv$id <- paste0(stammv$edvid, stammv$auf, stammv$nr, stammv$art)
# stammv$id <- gsub (" ", "", stammv$id)
# baum$id <- paste0(baum$edvid, baum$auf, baum$nr, baum$art)
# baum$id <- gsub (" ", "", baum$id)
# 
# # I dont want to double these columns:
# baum <- subset(baum, select = -c(auf, nr, edvid, bemerk, datum, Stempel, art, 
#                                  baumid)) 
# 
# tree_data <- merge(baum, stammv, by = "id", all.x = T, all.y = T)
# # I dont need this column anymore
# tree_data <- subset(tree_data, select = -c(id))
# 
# #  I had to split them to keep the data processing as simple as possible
# tree_data_polter <- tree_data[tree_data$edvid %in% c(30931301, 30931302), ]
# tree_data_ansitz <- tree_data[tree_data$edvid %in% c(30931303, 30931304), ]


