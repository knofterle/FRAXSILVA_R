################ GENERATE MAP ANSITZ ###########################################
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
source(file = "SCRIPT/Goe_Lau/Generate Plotdistribution ANSITZ.R") 

# stammv_ansitz, baum_ansitz, tree_data_ansitz
# thr_selected
# nvplots_ansitz, ref_ansitz

###### LIBRARY -----------------------------------------------------------------
library(ggplot2)

###### MAP THE PLOT WITH TREES AND NV PLOTS ------------------------------------

# All important points of the map are merged
map_ansitz <- data.frame(
  nr = c(stammv_ansitz$nr, nvplots_ansitz$nr, ref_ansitz$nr, "zero"),
  x = c(stammv_ansitz$x, nvplots_ansitz$x, ref_ansitz$x, zero_x),
  y = c(stammv_ansitz$y, nvplots_ansitz$y, ref_ansitz$y, zero_y),
  type = c(rep("tree", nrow(stammv_ansitz)), 
           rep("plot", nrow(nvplots_ansitz)),
           rep("ref", nrow(ref_ansitz)), "zero"))


color <- map_ansitz$type
color[color == "tree"] <- "green"
color[color == "plot"] <- "black"
color[color == "ref"] <- "red"
color[color == "zero"] <- "blue"

# And plotted
plot(map_ansitz$x, map_ansitz$y, col = color, xlim = c(0,70), ylim = c(0,80))

# All trees are plotted
pdf(file = "EXPORT/Goe_Lau/Ansitz_1" ,width=6,height=6,paper='special')
ggplot(data = tree_data_ansitz, aes (x,y)) +
  geom_point(colour = "green4", size = tree_data_ansitz$d/20, alpha = 0.2) +
  geom_point(colour = "green4", size = tree_data_ansitz$d/130) +
  lims( x = c(0,65), y = c(0,85)) +
  #  geom_point(data = solaris_ansitz, aes(x, y, colour = TSF), size = 7) +
  theme_bw()
dev.off()

# Only the remaining trees are plotted
pdf(file = "EXPORT/Goe_Lau/Ansitz_2" ,width=6,height=6,paper='special')

sub_tree_data_ansitz <- tree_data_ansitz[tree_data_ansitz$a == "", ]

ggplot(data = sub_tree_data_ansitz, aes (x,y)) +
  geom_point(colour = "green4", size = sub_tree_data_ansitz$d/20, alpha = 0.2) +
  geom_point(colour = "green4", size = sub_tree_data_ansitz$d/130) +
  lims( x = c(0,65), y = c(0,85)) +
  #  geom_point(data = nvplots_ansitz, aes(x,y, colour = TSF), pch = 15, size = 6,
  #             alpha = 0.8)
  #  geom_point(data = solaris_ansitz, aes(x, y, colour = TSF), size = 7) +
  theme_bw()
dev.off()

###### VISUALIZE THE SOLARISKOP DATA -------------------------------------------

solaris_ansitz <- merge(x = thr_selected, y = nvplots_ansitz, by.x = "name",  
                        by.y = "nr", all.x = F, all.y = T) 

# Only remaining trees are plotted
pdf(file = "EXPORT/Goe_Lau/Ansitz_3" ,width=6,height=6,paper='special')
ggplot(data = sub_tree_data_ansitz, aes (x,y)) +
  geom_point(colour = "green4", size = sub_tree_data_ansitz$d/20, alpha = 0.2) +
  geom_point(colour = "green4", size = sub_tree_data_ansitz$d/130) +
  geom_point(data = solaris_ansitz, aes(x,y, colour = TSF), pch = 15, size = 6,
             alpha = 0.7) +
  lims( x = c(0,65), y = c(0,85)) +
  #  geom_point(data = solaris_ansitz, aes(x, y, colour = TSF), size = 7) +
  theme_bw() + 
  theme(legend.position="none") 
dev.off()

### Tidy up 
rm(color, sub_tree_data_ansitz, map_ansitz)

###### OUTPUT ------------------------------------------------------------------
# solaris_ansitz, 



# ## TEST SITE AT THE END IT WORKS MORE OR LESS
# ## 
# vector <- c( solaris_ansitz$x[1] - solaris_ansitz$x[2] ,
#             solaris_ansitz$y[1] - solaris_ansitz$y[2])
# 
# 
# sub_tree_data_ansitz <- tree_data_ansitz[tree_data_ansitz$a == "", ]
# ggplot(data = sub_tree_data_ansitz, aes (x,y)) +
#   geom_point(colour = "green4", size = sub_tree_data_ansitz$d/20, alpha = 0.2) +
#   geom_point(colour = "green4", size = sub_tree_data_ansitz$d/130) +
#   #geom_rect(data = solaris_ansitz, aes(xmin = x, ymin = y, xmax = x+xy$x , 
#   #                                      ymax = y+xy$y, fill = TSF),
#   #            alpha = 0.7) +
#   lims( x = c(0,65), y = c(0,85)) +
#   geom_polygon(data = nvplots_mapping, aes(x = x, y = y, group = nr), size = 1) +
#   theme_bw() + 
#   theme(legend.position="none") 
# 
# 
# 
# distances <- solaris_ansitz[ , c("x", "y")] - c(solaris_ansitz [1, c("x", "y")] )
# distances <- apply(distances*distances, 1, sum)
# distances <-  sqrt(distances)
# which( distances <= 6 & distances >= 1)
# distances <- solaris_ansitz[ , c("x", "y")] - c(solaris_ansitz [1, c("x", "y")] )
# xy <- distances[2, ]/5 + distances[6, ]/5
# 
# solaris_ansitz[1, c("x", "y")]
# nvplots_mapping <- solaris_ansitz[ , c("x", "y")]
# nvplots_mapping[, 3:4] <- nvplots_mapping[,1:2] - c(distances[2,]/5)
# nvplots_mapping[, 5:6] <- nvplots_mapping[,1:2] - c(xy)
# nvplots_mapping[, 7:8] <- nvplots_mapping[,1:2] - c(distances[6,]/5)
# 
# nvplots_mapping <- matrix(data = nvplots_mapping, ncol = 2, byrow = F)
# nvplots_mapping <- data.frame(nvplots_mapping)
# colnames(nvplots_mapping) <- c("x", "y")
# nvplots_mapping$nr <- rep(seq(8701,8817), 4)
# nvplots_mapping <- nvplots_mapping[order(nvplots_mapping$nr), ]
# nvplots_mapping[1:10,]
# nvplots_mapping
# nvplots_mapping[118,]
# 
# 
# 
# empty <- c(nvplots_mapping$x, nvplots_mapping$x.1, nvplots_mapping$x.2, 
#              nvplots_mapping$x.3)
# empty <- data.frame (empty)
# empty$y <- c(nvplots_mapping$y, nvplots_mapping$y.1, nvplots_mapping$y.2, 
#              nvplots_mapping$y.3)
# colnames (empty) <- c("x","y")
# empty$nr <- rep(seq(8701,8817), 4)
# nvplots_mapping <- empty
# nvplots_mapping <- nvplots_mapping[order(nvplots_mapping$nr), ]
# empty[1,]
# empty[118,]
