# random test date, to be deleted

x <- floor(runif(100, min=0, max=101))
y <- floor(runif(100, min=0, max=101))
plot(x,y)

z <- seq(1,100, 4)
points(z,z, pch=22)

############################# GENERATE PLOTMAP #################################
# F. R. Schrewe
# 04.08.2022
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
packages=c("RODBC",   # Package RODBC implements ODBC database connectivity.
					 "ggplot2", # A system for declaratively creating graphics.
					 "dplyr"    # Helps you solve the most common data manipulation challenges.
)


installed = rownames(installed.packages())

for (pkg in packages) {
	if (! pkg %in% installed) {
		install.packages(pkg)
	}
}


dev.off(dev.list()["RStudioGD"])  # Delete graphics memory 
rm(list = ls())                   # Delete workspace 
gc(verbose = TRUE, reset = TRUE)  # Garbage collection
cat("\014")		     	              # Clear console 


###### REQUIRES ----------------------------------------------------------------
source(file = "SCRIPT/IBF/LOAD_DATA_TREE.R")
#  -> Lädt die DFs der einzelnen IB+ Flächen
rm(list=setdiff(ls(), "data_tree_scho")) # Zur Übersichtlichkeit: Vorab alles löschen dass ich noch nicht brauche

source(file = "SCRIPT/IBF/GENERATE_PLOTDISTRIBUTION.R")
#  -> kreiert die x & y-Koordinaten der Subplots
rm(list=setdiff(ls(), c("plots_ref_scho", "plots_pos_scho", "data_tree_scho"))) # Zur Übersichtlichkeit: Vorab alles löschen dass ich noch nicht brauche
	
source(file = "SCRIPT/IBF/generate_plotmap.R")
#  -> kreirt einen Plot-Befehl für die Karten, angepasst an Format der Ursprungsdaten

source(file = "SCRIPT/IBF/LOAD_NV.R") 
#  Loads and cleans all data from the Winter IBF surveys


###### NOTES -------------------------------------------------------------------


### SCHOTTEN  ---------------------------------------------------------------


# Test ob das "auch so" geht
plot(data_tree_scho$x, data_tree_scho$y, col=data_tree_scho$art, pch=25, bg=data_tree_scho$art)
length(unique(data_tree_scho$art))
data_tree_scho$art=="221"


plot.all1(tree_data = data_tree_scho, plots_pos = plots_pos_scho,
         plots_ref = plots_ref_scho)
plots_ref_scho





###### Create Dataframe  ---------------------------------------------------------------

# Create Dataframe with Treedata from Schotten
all_points <- data.frame(
	nr = c(data_tree_scho$nr),
	x = c(data_tree_scho$x),
	y = c(data_tree_scho$y),
	art = c(data_tree_scho$art.stammv),
	dia = c(data_tree_scho$dmess))


# Clean Data
# remove weird ones without Art
all_points <- all_points[!is.na(all_points$art),]
all_points <- all_points[all_points$art!=0,]


plot(all_points$x, all_points$y, col=1, pch=21, bg=adjustcolor(as.numeric(as.factor(all_points$art)), .5), cex = sqrt(all_points$dia/100))

as.numeric(as.factor(all_points$art))
table(all_points$art)






all_points$xy <- all_points$x + 1i*all_points$y

plot(all_points$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(all_points$dia)/10)  
# text(all_points$xy, labels = all_points$nr)





df <- all_points; rm(all_points) # kein Bock mehr auf langen Namen
sum(is.na(df$xy))
sum(is.na(df$y))
sum(is.na(df$x))

nrow(df)-sum(is.na(df$xy))

df <- df[complete.cases(df[,c("xy")]),] # Remove all Rows without XY Coordinates


# allgemeiner Test mit zufälligem Baum
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
}; close(pb); rm(pb, cc, xx)

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
}; close(pb); rm(pb, cc, xx)
table(df$gruppierung)


# Okay. Soweit, so gut. Aber es geht ja um Truppen / Gruppen einer Art. Also mal Subset nach Art.

table(df$art)                    # Species                  # n
d999 <- subset(df, art == "999") # Unbekannte Baumart        15
d110 <- subset(df, art == "110") # Quercus spec.              2
d111 <- subset(df, art == "111") # Quercus robur				   		2
d211 <- subset(df, art == "211") # Fagus sylvatica           41
d221 <- subset(df, art == "221") # Carpinus betulus         142
d311 <- subset(df, art == "311") # Fraxinus excelsior        91
d321 <- subset(df, art == "321") # Acer pseudoplatanus      132
d322 <- subset(df, art == "322") # Acer platanoides          11
d323 <- subset(df, art == "323") # Acer campestre             7
d333 <- subset(df, art == "333") # Ulmus minor                1 
d342 <- subset(df, art == "342") # Tilia  cordata             1
d354 <- subset(df, art == "354") # Prunus avium               2
d811 <- subset(df, art == "811") # Larix decidua              1
d555 <- subset(df, art == "555") #                            5
d666 <- subset(df, art == "666") # Hochsitz                   1

# Was ist 555? vermutlich der Weg?
plot(df$x, df$y, col=1, pch=21, bg=adjustcolor(as.numeric(as.factor(df$art)), .5), cex = sqrt(df$dia)/10)
text(df[df$art=="555",]$x, df[df$art=="555",]$y); lines(df[df$art=="555",]$x, df[df$art=="555",]$y)

aggregate(list(Anz = df$art), by = list(Art = df$art), FUN = length)

# Löschen von Daten die ich nicht brauche
rm("d555", "d666")

plot(df$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(df$dia)/10, pch = 1)

# Versuch mit der Art 211

# Test ob ich von allen Bäumen der Art 311 einzelnd auf die Umliegenden der Art 211 zugreifen kann
pb = txtProgressBar(min = 0, max = nrow(d311), initial = 0, char = ":", style = 3) 
for (cc in 1:nrow(d311)) {
	for (xx in 1:nrow(d211)) {
		if (abs(d311[cc,]$xy-d211[xx,]$xy) <= 6) {  # Maximaler Abstand zwischen Bäumen der betrachteten Art in Metern
			if (abs(d311[cc,]$xy-d211[xx,]$xy) > 0) { # Minimaler Abstand zwischen Bäumen der betrachteten Art in Metern
			arrows(Re(d311[cc,]$xy),Im(d311[cc,]$xy), Re(d211[xx,]$xy), Im(d211[xx,]$xy), length = 0, col = "purple", lwd = 2)
			}
		}
	}
	setTxtProgressBar(pb,cc)
	}; close(pb); rm(pb)





	 									 									 									 








# Alle Baumarten suchen nach Artgenosssen im Umkreis x

plot(df$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(df$dia)/10, pch = 1, col=as.numeric(as.factor(df$art)), lwd=2, sub = "Eschen markiert")
points(df[df$art=="311",]$xy, cex=sqrt(df[df$art=="311",]$dia)/10, pch=21, bg=rgb(0,0,0,.5))
ff <- list(d110, d111, d211, d221, d311, d321, d322, d323, d333, d342, d354, d811, d999)
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

plot(df$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(df$dia)/10, pch = 1)
points(d211[d211$gruppierung > 0,]$xy, pch = 16, cex = sqrt(d211[d211$gruppierung > 0,]$dia)/10, col = adjustcolor(d211[d211$gruppierung > 0,]$gruppierung, .5))
legend("bottomleft", inset = .02, legend=c(sort(unique(d211[d211$gruppierung > 0,]$gruppierung), decreasing = F)),
			 bg = "white", 
			 pch=21, pt.bg = c(adjustcolor(sort(unique(d211[d211$gruppierung > 0,]$gruppierung), decreasing = F), .5)), 
			 pt.cex=1.5, y.intersp = .75, box.col = "aquamarine3", title = "Anzahl Nachbarn", text.font=11)


### Subplots #####
 # Bekomme ich die Subplots hinein?


plots_pos_scho$xy <- plots_pos_scho$x + 1i*plots_pos_scho$y

points(plots_pos_scho$xy, pch = 22,bg=2, col=3, srt = 25)
text(plots_pos_scho$xy, labels = "\u25FB", srt = 25)

points(plots_pos_scho$xy, pch = 22, srt = 5)


# Fokus Subplots: Wie viele Eschen sind im Umkreis?
plot(df$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(df$dia)/10, pch = 1, col=as.numeric(as.factor(df$art)), lwd=2)
text(plots_pos_scho$xy, labels = "\u25FB", srt = 25)

plots_pos_scho$umkreis <- 0
for (cc in 1:nrow(plots_pos_scho)) {
	for (xx in 1:nrow(d311)) {
		if(abs(plots_pos_scho[cc,]$xy-d311[xx,]$xy)<= 6) {
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

ff <- list(d110, d111, d211, d221, d311, d321, d322, d323, d333, d342, d354, d811, d999)

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


plot(df$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(df$dia)/10, pch = 1, col=as.numeric(as.factor(df$art)), lwd=2)
points(plots_pos_scho$xy, pch=22, bg=rgb(0,1,1,.5))
text(plots_pos_scho$xy, labels = plots_pos_scho$nr)


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







dfn <- data_nv[data_nv$Flaeche=="Schotten",]
dd <- dfn[dfn$ID_plot=="79Sch" & dfn$Baumart_kurz=="GEs",]

str(dd)

table(dd$ETS)
table(dfn$ID_plot)
table(dfn$Plotnummer)

sum(dd$ETS)
length(dd$ETS)


aggregate(list(Anz = dfn$Plotnummer), by = list(Plot = dfn$Plotnummer), FUN = length)


# Anzahl Eschen gesamt
plots_pos_scho$n.fe <- 0

# Anzahl Eschen mit ETS
plots_pos_scho$n.ets <- 0

# Anzahl aller Bäume
plots_pos_scho$n.ba <- 0

head(plots_pos_scho)

# Spalten mit Werten füllen
plots_pos_scho[79,]$n.fe <- nrow(dfn[dfn$Plotnummer=="79" & dfn$Baumart_kurz=="GEs",])
plots_pos_scho[79,]$n.ets <- nrow(dfn[dfn$Plotnummer=="79" & dfn$ETS==TRUE,])
plots_pos_scho[79,]$n.ba <- nrow(dfn[dfn$Plotnummer=="79",])



# Als schleife:
for (cc in 1:nrow(plots_pos_scho)){
	plots_pos_scho[cc,]$n.fe <- nrow(dfn[dfn$Plotnummer==cc & dfn$Baumart_kurz=="GEs",])
	plots_pos_scho[cc,]$n.ets <- nrow(dfn[dfn$Plotnummer==cc & dfn$ETS==TRUE,])
	plots_pos_scho[cc,]$n.ba <- nrow(dfn[dfn$Plotnummer==cc,])
}


# Da sind aber noch die ohne Bäume drin.


for (cc in 1:nrow(plots_pos_scho)){
	if((dfn[dfn$Plotnummer==cc,]$keine.baeume==TRUE)[1]){ # [1] Weil wenn ein Subplot keine Bäume hat, also FALSE ist und mehrere Bäume hat nimmt er nur den ersten.. sind ja eh alle FALSE
		print(cc)
		plots_pos_scho[cc, names(plots_pos_scho) %in% c("n.fe", "n.ba")] <- plots_pos_scho[cc, names(plots_pos_scho) %in% c("n.fe", "n.ba")] -1
	} else{}
}


plots_pos_scho$prozent <- (plots_pos_scho$n.ets / plots_pos_scho$n.fe)*100
plots_pos_scho
## Den bonitierschlüssel anwenden... sonst wird das nichts


unique(dfn$Baumart_lang)

sum(is.na(dfn$Baumart_lang))

dfn$Anzahl.Triebe
dfn$ETS.lebend[dfn$Baumart_kurz=="GEs"]
dfn$ETS.abgestorben.frisch+
dfn[dfn$Baumart_kurz=="GEs",]$ETS.abgestorben.alt
dfn[dfn$Baumart_kurz=="GEs",]$Baumart_lang

dddd <- dfn[dfn$Baumart_kurz=="GEs",]
dddd[12,]
### Hier #####
view(df)


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

