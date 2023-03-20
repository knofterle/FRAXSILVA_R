################ LOAD TREE DISTRIBUTION DATA ####################################
# J.Osewold
# 01.12.2021
##### DRAFT #####
################################################################################

###### NOTES -------------------------------------------------------------------
#  I copied the code from my side project "generate a plotdistribution of every
# trial area" at the 1st of December. 
# 

###### LIBRARY -----------------------------------------------------------------
library(RODBC)

###### DEFINE BASIC FUNCTIONS  -------------------------------------------------------
load.edvid <- function(id){
  waldwachstum <- odbcConnect("Waldwachstum")
  select <- paste0("SELECT Parz.edvid
                    FROM Parz")
  all_edvid <- sqlQuery(waldwachstum, select)
  all_edvid <- all_edvid$edvid
  edvid <- all_edvid[grep(id, all_edvid)]
  return(edvid[1])
  odbcCloseAll()
}

load.parcels <- function(edvid){
  waldwachstum <- odbcConnect("Waldwachstum")
  select <- paste0("SELECT Parz.edvid, Parz.id, Parz.parz
                    FROM Parz 
                    WHERE (((Parz.edvid)= ", "\"",
                   edvid, "\"",
                   "))")
  parc_temp <- sqlQuery(waldwachstum, select)
  return(parc_temp)
  odbcCloseAll()
}

load.stammv <- function(edvid) {
  waldwachstum <- odbcConnect("Waldwachstum")
  select <- paste0("SELECT Stammv.baumid, Stammv.nr, Stammv.art, Stammv.edvid,
                    Stammv.auf, Stammv.x, Stammv.y, Stammv.z
                    FROM Stammv 
                    WHERE (((Stammv.edvid)= ", "\"",
                   edvid, "\"",
                   "))")
  stammv_temp <- sqlQuery(waldwachstum, select)
  return(stammv_temp)
  odbcCloseAll()
}

load.baum <- function(edvid) {
  waldwachstum <- odbcConnect("Waldwachstum")
  select <- paste0("SELECT Baum.edvid, Baum.baumid, 
                    Baum.alt, Baum.ou, Baum.d, Baum.h, Baum.bemerk, Baum.auf, 
                    Baum.nr
                    FROM Baum 
                    WHERE (((Baum.edvid)= ", "\"",
                   edvid, "\"",
                   "))")
  baum_temp <- sqlQuery(waldwachstum, select)
  return(baum_temp)
  odbcCloseAll()
}

###### DEFINE NESTED FUNCTIONS  ------------------------------------------------
load.data <- function(edvid){
  edvid <- load.edvid(id)
  parcels <- load.parcels (edvid)
  stammv <- data.frame()
  for (i in parcels$id) {
    stammv <- rbind(load.stammv(i), stammv)
  }
  baum <- data.frame()
  for (i in parcels$id) {
    baum <- rbind(load.baum(i), baum)
  }
  
  #  I could not use the column "baumid" because not all trees have one and I dont 
  # know the reason for that. Therefore I generated a new ID column.
  stammv$id <- paste0(stammv$edvid, stammv$auf, stammv$nr, stammv$art)
  stammv$id <- gsub (" ", "", stammv$id)
  baum$id <- paste0(baum$edvid, baum$auf, baum$nr, baum$art)
  baum$id <- gsub (" ", "", baum$id)
  
  # I dont want to double these columns:
  baum <- subset(baum, select = -c(auf, nr, edvid, bemerk, datum, Stempel, art, 
                                   baumid)) 
  
  data <- merge(baum, stammv, by = "id", all.x = T, all.y = T)
  data <- subset(data, select = -c(id))
  
  return (data)
}
###### LOAD ACTUAL DATA OF 309313  ---------------------------------------------

# Each parcel had to be loaded separately 
stammv_polter <- rbind (load.stammv(30931301), load.stammv(30931302))
stammv_ansitz <- rbind (load.stammv(30931303), load.stammv(30931304))

baum_polter <- rbind (load.baum(30931301), load.baum(30931302))
baum_ansitz <- rbind (load.baum(30931303), load.baum(30931304)) 

tree_data <- load.data("309")
tree_data_polter <- tree_data[tree_data$edvid %in% c(30931301, 30931302), ]
tree_data_ansitz <- tree_data[tree_data$edvid %in% c(30931303, 30931304), ]

### Tidy up

###### OUTPUT ------------------------------------------------------------------
# stammv_polter, baum_polter, stammv_ansitz, baum_ansitz, tree_data_polter, 
# tree_data_ansitz