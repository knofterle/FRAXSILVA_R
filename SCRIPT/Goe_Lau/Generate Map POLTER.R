################ GENERATE MAP POLTER ###########################################
# J.Osewold
# 02.03.2022
##### DRAFT #####
################################################################################

###### NOTES -------------------------------------------------------------------
# 


###### REQUIRES ----------------------------------------------------------------
if (.Platform$OS.type == "unix") {
  source (file = "SCRIPT/Goe_Lau/Load MDB Data.R")
} else { 
  source (file = "SCRIPT/Goe_Lau/Laden der Stammverteilung Windows.R") 
}

source(file = "SCRIPT/Goe_Lau/Load Solariskop Data.R")
source(file = "SCRIPT/Goe_Lau/Generate Plotdistribution POLTER.R") 

# stammv_polter, baum_polter, tree_data_polter
# thr_selected
# nvplots_polter, ref_polter

###### LIBRARY -----------------------------------------------------------------
library(ggplot2)

###### MAP THE PLOT WITH TREES AND NV PLOTS ------------------------------------

# All important points of the map are merged
map_polter <- data.frame(
  nr = c(stammv_polter$nr, nvplots_polter$nr, ref_polter$nr, "zero"),
  x = c(stammv_polter$x, nvplots_polter$x, ref_polter$x, zero_x),
  y = c(stammv_polter$y, nvplots_polter$y, ref_polter$y, zero_y),
  type = c(rep("tree", nrow(stammv_polter)), 
           rep("plot", nrow(nvplots_polter)),
           rep("ref", nrow(ref_polter)), "zero"))


color <- map_polter$type
color[color == "tree"] <- "green"
color[color == "plot"] <- "black"
color[color == "ref"] <- "red"
color[color == "zero"] <- "blue"

# And plotted
plot(map_polter$x, map_polter$y, col = color, xlim = c(0,70), ylim = c(0,80))

# All trees are plotted
pdf(file = "EXPORT/Goe_Lau/Polter_1" ,width=6,height=6,paper='special')
  ggplot(data = tree_data_polter, aes (x,y)) +
    geom_point(colour = "green4", size = tree_data_polter$d/20, alpha = 0.2) +
    geom_point(colour = "green4", size = tree_data_polter$d/130) +
    lims( x = c(0,65), y = c(0,85)) +
  #  geom_point(data = solaris_polter, aes(x, y, colour = TSF), size = 7) +
    theme_bw()
dev.off()

# Only the remaining trees are plotted
pdf(file = "EXPORT/Goe_Lau/Polter_2" ,width=6,height=6,paper='special')
 sub_tree_data_polter <- tree_data_polter[tree_data_polter$a == "", ]
 ggplot(data = sub_tree_data_polter, aes (x,y)) +
   geom_point(colour = "green4", size = sub_tree_data_polter$d/20, alpha = 0.2) +
   geom_point(colour = "green4", size = sub_tree_data_polter$d/130) +
   lims( x = c(0,65), y = c(0,85)) +
 #  geom_point(data = nvplots_polter, aes(x,y, colour = TSF), pch = 15, size = 6,
 #             alpha = 0.8)
 #  geom_point(data = solaris_polter, aes(x, y, colour = TSF), size = 7) +
  theme_bw()
dev.off()

###### VISUALIZE THE SOLARISKOP DATA -------------------------------------------

solaris_polter <- merge(x = thr_selected, y = nvplots_polter, by.x = "name",  
                        by.y = "nr", all.x = F, all.y = T) 

# Only remaining trees are plotted
pdf(file = "EXPORT/Goe_Lau/Polter_3" ,width=6,height=6,paper='special')
ggplot(data = sub_tree_data_polter, aes (x,y)) +
  geom_point(colour = "green4", size = sub_tree_data_polter$d/20, alpha = 0.2) +
  geom_point(colour = "green4", size = sub_tree_data_polter$d/130) +
  geom_point(data = solaris_polter, aes(x,y, colour = TSF), pch = 15, size = 6,
             alpha = 0.7) +
  lims( x = c(0,65), y = c(0,85)) +
  #  geom_point(data = solaris_polter, aes(x, y, colour = TSF), size = 7) +
  theme_bw() + 
  theme(legend.position="none") 
dev.off()

### Tidy up 
rm(color, sub_tree_data_polter, map_polter)

###### OUTPUT ------------------------------------------------------------------
# solaris_polter, 


