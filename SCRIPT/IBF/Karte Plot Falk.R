# random test date, to be deleted

x <- floor(runif(100, min=0, max=101))
y <- floor(runif(100, min=0, max=101))
plot(x,y)

z <- seq(1,100, 4)
points(z,z, pch=22)

############################# GENERATE PLOTMAP #################################
# J.Osewold
# 12.04.22
##### DRAFT #####
################################################################################

###### Notes ----------------------------------------------------------------
"Ich will von den Altbäumen berechnen ob sie in einem Host / Gruppe / Trupp stehen.

Ich brauche: DF mit Koordinaten der Altbäume, deren Höhe / BHD.
                                       ✓

Schleife: Überprüfen ob im Umkreis x dieses Baumes ein weiterer Baum selber Art steht. 
Falls ja: ?

"
###### LIBRARYS ----------------------------------------------------------------

## 1. Have the computer install all required packages:                    ------

packages=c("RODBC"  # Package RODBC implements ODBC database connectivity.
)


installed = rownames(installed.packages())

for (pkg in packages) {
	if (! pkg %in% installed) {
		install.packages(pkg)
	}
}
dev.off(dev.list()["RStudioGD"])  # Delete graphics memory 
rm(list = ls())                   # Delete workspace 
cat("\014")		     	              # Clear console 
gc(reset = TRUE)                  # Garbage collection


###### REQUIRES ----------------------------------------------------------------
source(file = "SCRIPT/IBF/LOAD_DATA_TREE.R")
	'-> Lädt die DFs der einzelnen IB+ Flächen'

source(file = "SCRIPT/IBF/GENERATE_PLOTDISTRIBUTION.R")
	'-> kreiert die x & y-Koordinaten der Subplots'

source(file = "SCRIPT/IBF/generate_plotmap.R")
'	-> kreirt einen Plot-Befehl für die Karten, angepasst an Format der Ursprungsdaten'


###### NOTES -------------------------------------------------------------------


###### SCHOTTEN  ---------------------------------------------------------------
plot.all1 <- function(tree_data, plots_pos, plots_ref) {
	
	### MERGE ALL DATA -----------------------------------------------------------
	all_points <- data.frame(
		nr = c(data_tree_scho$nr, plots_pos_scho$nr, plots_ref_scho$nr),
		x = c(data_tree_scho$x, plots_pos_scho$x, plots_ref_scho$x),
		y = c(data_tree_scho$y, plots_pos_scho$y, plots_ref_scho$y),
		type = c(rep("tree", nrow(data_tree_scho)), 
						 rep("plot", nrow(plots_pos_scho)),
						 rep("ref_target", nrow(plots_ref_scho)/2),
						 rep("ref_current", nrow(plots_ref_scho)/2)))
	
	### SET COLOURS --------------------------------------------------------------
	color <- all_points$type
	color[color == "tree"] <- "gray"
	color[color == "plot"] <- "black"
	color[color == "ref_target"] <- "red"
	color[color == "ref_current"] <- "green"
	
	### PLOT ---------------------------------------------------------------------
	plot <- plot(all_points$x, all_points$y, col = color
							 #, xlim = c(0,70), ylim = c(0,80)
	)
	return(plot)
}

df <- plot.all1("all_points")


# Test ob das "auch so" geht
plot(data_tree_scho$x, data_tree_scho$y, col=data_tree_scho$art, pch=25, bg=data_tree_scho$art)
length(unique(data_tree_scho$art))
data_tree_scho$art=="221"


plot.all1(tree_data = data_tree_scho, plots_pos = plots_pos_scho,
         plots_ref = plots_ref_scho)
plots_ref_scho

c(data_tree_scho$nr, plots_pos_scho$nr, plots_ref_scho$nr)






all_points <- data.frame(
	nr = c(data_tree_scho$nr),
	x = c(data_tree_scho$x),
	y = c(data_tree_scho$y),
	art = c(data_tree_scho$art),
	dia = c(data_tree_scho$dmess))






all_points <- all_points[!is.na(all_points$art),]

# remove weird one without Art
all_points <- all_points[all_points$art!=0,]


plot(all_points$x, all_points$y, col=1, pch=21, bg=adjustcolor(as.numeric(as.factor(all_points$art)), .5), cex = all_points$dia/100)
plot(all_points$x, all_points$y, col=1, pch=21, bg=all_points$art+1, cex = all_points$dia/100)


data_tree_scho$nr


as.numeric(as.factor(all_points$art))

plot(1,1, cex=10, pch=25,bg=adjustcolor("red", alpha.f=.5))

table(all_points$art)


## Hegyi ####

all_points$xy <- all_points$x + 1i*all_points$y
k<-2; plotarea<-10000


CZR<-sqrt(10000/(nrow(all_points))/plotarea)*k  
tree0<-all_points$nr
nrow(all_points)

x0<-NULL
y0<-NULL
spec0<-NULL
d0<-NULL
pos11<-NULL
pos12<-NULL
posxy<-NULL

for(n in 1:length(tree0)){
	x0[n]<-all_points$x[all_points$nr==tree0[n]]
	y0[n]<-all_points$y[all_points$nr==tree0[n]]
	spec0[n]<-all_points$Sp[all_points$nr==tree0[n]]
	d0[n]<-all_points$dia[all_points$nr==tree0[n]]
	pos11[n]<-all_points$xy[all_points$nr==tree0[n]]
	pos12[n]<-all_points$xy[all_points$nr==tree0[n]]
	posxy[n]<-all_points$xy[all_points$nr==tree0[n]]
}

x0
y0
spec0
d0
pos11
pos12
posxy

ref_tree<-cbind(tree0, spec0, d0, x0, y0); ref_tree # all the ref trees 
ref_tree<-as.data.frame(ref_tree)
is(ref_tree)
cbind(ref_tree, posxy)
ref_tree <- cbind(ref_tree, posxy) # neu

plot(all_points$xy, main="Treecoordinates Plot 1", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(all_points$dia)/10)  

for(n in 1:length(tree0)){
	pos11<-ref_tree$posxy[n]
	pos12<-all_points$xy[n]
}
is(ref_tree$posxy)
is(tree0)
dist<-(pos11-pos12)
abs(dist)




new1<-matrix(NA,0,2)

for(n in 1:nrow(all_points)){
	pos11[n]<-all_points$xy[n]
	pos12[n]<-all_points$xy[n]
	new1<-cbind(pos11, pos12)
}


x<-1*sin(2);x


### all-all #####

loc<-c(1:3,8:10,12,13)
for(o in 1:length(loc)) {
	
	# PlotX <- all_points[all_points$LOC==(loc[o]), ]
	PlotX <- all_points
	
	### ----------------------------------------------------------------------------
	### Set Competition Zone Radii
	
	#k<-2 
	#plotarea<-10000/(30*30) # fläche = 10000, plot = 30
	#CZR<-sqrt(10000/(nrow(PlotX))/plotarea)*k
	#print(CZR)
	
	#Alternativ Frank Hegyis Version mit 10 Fuß:
	CZR<-3.05
	
	### ----------------------------------------------------------------------------
	### Create empty Vectors to fill data.frame
	
	tree0<-PlotX$nr 
	x0    <-PlotX$x 
	y0    <-PlotX$y 
	spec0 <-PlotX$art 
	d0    <-PlotX$dia
	ref_tree<-cbind.data.frame(tree0,x0, y0, spec0, d0)
	
	### ----------------------------------------------------------------------------
	### Run loop for Hegyi Index
	
	hegyi <- rep(0, length(tree0))
	test<-NULL
	
	for(n in 1:length(tree0)) {
		bhd_i <- d0[n]
		
		for(i in 1:nrow(PlotX)) {
			if(tree0[n] != PlotX$nr[i]) {
				
				xdiff <- x0[n] - PlotX$x[i]
				ydiff <- y0[n] - PlotX$y[i]
				
				distance <- sqrt( xdiff^2 + ydiff^2 )
				
				if(distance <= CZR) {
					bhd_target <- PlotX$dia[i]
					
					hegyi[n] = hegyi[n] + ( bhd_target / bhd_i ) / distance 
				}
			}
		}
	}
	
	
	### ----------------------------------------------------------------------------
	### Bind Hegyi with ref_tree and all_points
	
	ref_tree<-cbind(ref_tree, hegyi); ref_tree
	
	#all_points$Hegyi[all_points$LOC==loc[o]] <- ifelse(all_points$nr[all_points$LOC==loc[o]] %in% (ref_tree$tree0), ref_tree$hegyi, NA)#Produces wrong order
	i1 <- with(all_points, match(nr[LOC==loc[o]], with(ref_tree, tree0)))
	all_points$Hegyi[all_points$LOC==loc[o]] <- ref_tree$hegyi[i1]
}



#max(all_points[, 26], na.rm = TRUE)
#write.csv2(all_points, file = "MyData.csv")





df <- all_points
sum(is.na(df$xy))

nrow(df)-sum(is.na(df$xy))

df <- df[complete.cases(df[,c("xy")]),] # Remove all Rows without XY Coordinates



if(df[3,] != PlotX$nr[i]) {
	
	xdiff <- x0[n] - PlotX$x[i]
	ydiff <- y0[n] - PlotX$y[i]
	
	distance <- sqrt( xdiff^2 + ydiff^2 )
	
	if(distance <= CZR) {
		bhd_target <- PlotX$dia[i]
		
		hegyi[n] = hegyi[n] + ( bhd_target / bhd_i ) / distance 
	}
}


zone <- 6 # Abstand die der Oberhöhe entspricht...
focus <- 3 # Baum den ich zum ausprobieren betrachte

for (i in 1:nrow(df)) {
	print(
		abs(df[focus,]$xy-df[i,]$xy)
	)
	arrows(Re(df[focus,]$xy),Im(df[focus,]$xy), Re(df[i,]$xy), Im(df[i,]$xy), length = 0, col = "purple")
	ifelse(
		abs(df[focus,]$xy-df[i,]$xy) <= zone,
		arrows(Re(df[focus,]$xy),Im(df[focus,]$xy), Re(df[i,]$xy), Im(df[i,]$xy), length = 0, col = "red"),
		NA
	)
}

df[abs(df[x,]$xy-df$xy) <= 6,]

df[3,]$xy


arrows(Re(df[3,]$xy),Im(df[3,]$xy), Re(df[10,]$xy), Im(df[10,]$xy), length = 0, col = "purple")

arrows(Re(df[focus,]$xy),Im(df[focus,]$xy), Re(df[i,]$xy), Im(df[i,]$xy), length = 0, col = "red")





plot(all_points$xy, main="Treecoordinates Plot 1", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(all_points$dia)/10)  
# text(all_points$xy, labels = all_points$nr)


# allgemeiner Test
if (abs(df[3,]$xy-df[16,]$xy) <= 6) {
	arrows(Re(df[3,]$xy),Im(df[3,]$xy), Re(df[16,]$xy), Im(df[16,]$xy), length = 0, col = "purple", lwd = 3)
}

#  Test ob ich von einem Baum auf die umliegenden zugreifen kann
for (xx in 1:nrow(df)) {
	print(df[xx,]$xy)
		if (abs(df[3,]$xy-df[xx,]$xy) <= 6) {
		arrows(Re(df[3,]$xy),Im(df[3,]$xy), Re(df[xx,]$xy), Im(df[xx,]$xy), length = 0, col = "purple")
	}	
}

# Test ob ich von allen Bäumen einzelnd auf die Umliegenden zugreifen kann
pb = txtProgressBar(min = 0, max = nrow(df), initial = 0, char = ":", style = 3) 
for (cc in 1:nrow(df)) {
	for (xx in 1:nrow(df)) {
		if (abs(df[cc,]$xy-df[xx,]$xy) <= 6) {
			arrows(Re(df[cc,]$xy),Im(df[cc,]$xy), Re(df[xx,]$xy), Im(df[xx,]$xy), length = 0, col = "purple")
		}	
	}
	setTxtProgressBar(pb,cc)
}; close(pb)

# Anstelle von Linien Zeichnen eine Vermerkung im DataFrame des Trupps einfügen
df$gruppierung <- 0
pb = txtProgressBar(min = 0, max = nrow(df), initial = 0, char = ":", style = 3) 
for (cc in 1:nrow(df)) {
	for (xx in 1:nrow(df)) {
		if (abs(df[cc,]$xy-df[xx,]$xy) <= 6) {
			df[cc,]$gruppierung <- df[cc,]$gruppierung+1
		}	
	}
	setTxtProgressBar(pb,cc)
}; close(pb)
table(df$gruppierung)


# Okay. Soweit, so gut. Aber es geht ja um Truppen / Gruppen einer Art. Also mal Subset nach Art.

table(df$art)
d221 <- subset(df, art == "221")
d211 <- subset(df, art == "211")
d322 <- subset(df, art == "322")
d321 <- subset(df, art == "321")
d311 <- subset(df, art == "311")
d110 <- subset(df, art == "110")
d354 <- subset(df, art == "354")
d323 <- subset(df, art == "323")
d999 <- subset(df, art == "999")
d811 <- subset(df, art == "811")
d666 <- subset(df, art == "666")
d555 <- subset(df, art == "555")
d333 <- subset(df, art == "333")
d342 <- subset(df, art == "342")
d111 <- subset(df, art == "111")

plot(all_points$xy, main="Treecoordinates Plot 1", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(all_points$dia)/10, pch = 1)

# Versuch mit der Art 211

# Test ob ich von allen Bäumen der Art 211 einzelnd auf die Umliegenden der Art 211 zugreifen kann
pb = txtProgressBar(min = 0, max = nrow(d211), initial = 0, char = ":", style = 3) 
for (cc in 1:nrow(d211)) {
	for (xx in 1:nrow(d211)) {
		if (abs(d211[cc,]$xy-d211[xx,]$xy) <= 6) {  # Maximaler Abstand zwischen Bäumen der betrachteten Art in Metern
			if (abs(d211[cc,]$xy-d211[xx,]$xy) > 0) { # Minimaler Abstand zwischen Bäumen der betrachteten Art in Metern
			arrows(Re(d211[cc,]$xy),Im(d211[cc,]$xy), Re(d211[xx,]$xy), Im(d211[xx,]$xy), length = 0, col = "purple", lwd = 2)
			}
		}
	}
	setTxtProgressBar(pb,cc)
	}; close(pb); rm(pb)





	 									 									 									 








# Alle Baumarten suchen nach Baumarten im Umkreis x

plot(all_points$xy, main="Treecoordinates Plot 1", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(all_points$dia)/10, pch = 1, col=as.numeric(as.factor(all_points$art)), lwd=2)
points(all_points[all_points$art=="311",]$xy, cex=sqrt(all_points[all_points$art=="311",]$dia)/10, pch=21, bg=rgb(0,0,0,.5))

ff <- list(d110, d111, d211, d221, d311, d321, d322, d323, d333, d342, d354, d555, d666, d811, d999)
# ff <- list(d221, d321)


for (vv in 1:length(ff)) {
	print("Durchlauf Nr.")
	print(vv)	
	print("Anzahl Zeilen im Datensatz") 
	print(		nrow(ff[[vv]])	)
	
	pb = txtProgressBar(min = 0, max = nrow(ff[[vv]]), initial = 0, char = ":", style = 3) 
	for (cc in 1:nrow(ff[[vv]])) {
		for (xx in 1:nrow(ff[[vv]])) {
			if (abs(ff[[vv]][cc,]$xy-ff[[vv]][xx,]$xy) <= 6) {  # Maximaler Abstand zwischen Bäumen der betrachteten Art in Metern
				if (abs(ff[[vv]][cc,]$xy-ff[[vv]][xx,]$xy) > 0) { # Minimaler Abstand zwischen Bäumen der betrachteten Art in Metern
					arrows(Re(ff[[vv]][cc,]$xy),Im(ff[[vv]][cc,]$xy), Re(ff[[vv]][xx,]$xy), Im(ff[[vv]][xx,]$xy), length = 0, col = vv, lwd = 2)
				}
			}
		}
		setTxtProgressBar(pb,cc)
	}
}; close(pb); rm(pb)






d211$gruppierung <- 0
for (cc in 1:nrow(d211)) {
	for (xx in 1:nrow(d211)) {
		if (abs(d211[cc,]$xy-d211[xx,]$xy) <= 6) {
			if (abs(d211[cc,]$xy-d211[xx,]$xy) > 0) {
			d211[cc,]$gruppierung <- d211[cc,]$gruppierung+1
			}	
		}
	}
}
table(d211$gruppierung)'
0   1  2  3 
26  6  7  2 
26 haben keine Verbindung innerhalb der maximalen Distanz, 6 haben eine, 7 zwei und 2 haben drei Bäume um sich'

points(d211[d211$gruppierung > 0,]$xy, pch = 16, cex = 6, col = adjustcolor(d211[d211$gruppierung > 0,]$gruppierung, .5))



### Subplots #####
 # Bekomme ich die Subplots hinein?


plots_pos_scho$xy <- plots_pos_scho$x + 1i*plots_pos_scho$y

points(plots_pos_scho$xy, pch = 22,bg=2, col=3, srt = 25)
text(plots_pos_scho$xy, labels = "\u25FB", srt = 25)

points(plots_pos_scho$xy, pch = 22, srt = 5)


# Fokus Subplots: Wie viele Eschen sind im Umkreis?
plot(all_points$xy, main="Treecoordinates Plot 1", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(all_points$dia)/10, pch = 1, col=as.numeric(as.factor(all_points$art)), lwd=2)
text(plots_pos_scho$xy, labels = "\u25FB", srt = 25)

plots_pos_scho$umkreis <- 0
for (cc in 1:nrow(plots_pos_scho)) {
	for (xx in 1:nrow(d211)) {
		if(abs(plots_pos_scho[cc,]$xy-d211[xx,]$xy)<= 6) {
			plots_pos_scho[cc,]$umkreis <- plots_pos_scho[cc,]$umkreis + 1
		}
	}
}
table(plots_pos_scho$umkreis)

"> table(plots_pos_scho$umkreis)
0  1  2  3  4 
77 21  7  1  1
77 Sublots haben keine BA 211 im Umkreis von 6m, 21 eine, 7 zwei, eine 3 und eine 4."

# Nun hab ich das aufgeschrieben und muss es noch visualisieren
pb = txtProgressBar(min = 0, max = nrow(d211), initial = 0, char = ":", style = 3) 
for (cc in 1:nrow(plots_pos_scho)) {
	for (xx in 1:nrow(d211)) {
		if (abs(plots_pos_scho[cc,]$xy-d211[xx,]$xy) <= 6) {  # Maximaler Abstand zwischen Bäumen der betrachteten Art in Metern
			arrows(Re(plots_pos_scho[cc,]$xy),Im(plots_pos_scho[cc,]$xy), Re(d211[xx,]$xy), Im(d211[xx,]$xy), length = 0, col = "purple", lwd = 2)
		}
	}
	setTxtProgressBar(pb,cc)
}; close(pb); rm(pb)


# Und nun für alle Baumarten nacheinander

ff <- list(d110, d111, d211, d221, d311, d321, d322, d323, d333, d342, d354, d555, d666, d811, d999)

for (vv in 1:length(ff)) {
	print("Durchlauf Nr.")
	print(vv)	
	print("Anzahl Zeilen im Datensatz") 
	print(		nrow(ff[[vv]])	)
	
	pb = txtProgressBar(min = 0, max = nrow(ff[[vv]]), initial = 0, char = ":", style = 3) 
	for (cc in 1:nrow(ff[[vv]])) {
		for (xx in 1:nrow(ff[[vv]])) {
			if (abs(ff[[vv]][cc,]$xy-ff[[vv]][xx,]$xy) <= 6) {  # Maximaler Abstand zwischen Bäumen der betrachteten Art in Metern
					arrows(Re(ff[[vv]][cc,]$xy),Im(ff[[vv]][cc,]$xy), Re(ff[[vv]][xx,]$xy), Im(ff[[vv]][xx,]$xy), length = 0, col = vv, lwd = 2)
				}
			}
		setTxtProgressBar(pb,cc)
	}
}; close(pb); rm(pb)

# ------------------------------------------------------


plot(all_points$xy, main="Treecoordinates Plot 1", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(all_points$dia)/10, pch = 1, col=as.numeric(as.factor(all_points$art)), lwd=2)
points(plots_pos_scho$xy, pch=22, bg=rgb(0,1,1,.5))



for (vv in 1:length(ff)) {            # Wechselt zwischen Baumarten
	print("Durchlauf Nr.")
	print(vv)	
	print("Anzahl Zeilen im Datensatz") 
	print(		nrow(ff[[vv]])	)
	pb = txtProgressBar(min = 0, max = nrow(ff[[vv]]), initial = 0, char = ":", style = 3) 
	for (xx in 1:nrow(plots_pos_scho)) {  # wechselt zwischen Subplots
		for (cc in 1:nrow(ff[[vv]])) {        # Wechselt zwischen Bäumen
			if(abs(plots_pos_scho[xx,]$xy - ff[[vv]][cc,]$xy) <= 6){
				arrows(Re(plots_pos_scho[xx,]$xy),Im(plots_pos_scho[xx,]$xy), Re(ff[[vv]][cc,]$xy), Im(ff[[vv]][cc,]$xy), length = 0, col = vv, lwd = 2)
				# arrows(Re(ff[[vv]][cc,]$xy), Im(ff[[vv]][cc,]$xy), Re(plots_pos_scho[xx,]$xy),Im(plots_pos_scho[xx,]$xy) ,length = 0.1, col = vv, lwd = 2)
				# } else{
				# arrows(Re(plots_pos_scho[xx,]$xy),Im(plots_pos_scho[xx,]$xy), Re(ff[[vv]][cc,]$xy), Im(ff[[vv]][cc,]$xy), length = 0, col = "red", lwd = 2)
			}
		}
	setTxtProgressBar(pb,xx) # läuft nicht gut
	}
}; close(pb); rm(pb)

### Hier



###### MOLLENFELDE  ------------------------------------------------------------
plot.all1(tree_data = data_tree_mol, plots_pos = plots_pos_mol,
         plots_ref = plots_ref_mol)
plots_ref_mol

###### HUY  ------------------------------------------------------------
plot.all1(tree_data = data_tree_huy, plots_pos = plots_pos_huy,
         plots_ref = plots_ref_huy)
plots_ref_huy

###### WEISWEIL  ------------------------------------------------------------
plot.all1(tree_data = data_tree_weis, plots_pos = plots_pos_weis,
         plots_ref = plots_ref_weis)
plots_ref_weis

###### PLATTENWALD  ------------------------------------------------------------
map_platt <- plot.all2(tree_data = data_tree_platt, plots_pos = plots_pos_platt,
         plots_ref = plots_ref_platt)
ggsave(filename = "EXPORT/IBF/figures/map_plattenwald.pdf", plot = map_platt, 
       device = "pdf", units = "cm", width = 18, height = 18)
plots_ref_platt

###### TIDY UP  ----------------------------------------------------------------
rm(map_platt)

###### OUTPUT ------------------------------------------------------------------
# plot
# 



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

