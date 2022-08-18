# Section One ----
## Section One ----
### Section One ----


# random test date, to be deleted

x <- floor(runif(100, min=0, max=101))
y <- floor(runif(100, min=0, max=101))
plot(x,y)

z <- seq(1,100, 4)
points(z,z, pch=22)

### ==================================================================== ###
### GENERATE PLOTMAP
### Part: A: Import o relevant data
### Date: 2022-08-17
### 
### Submitted by:   	Schrewe, Falk R  	4090042
###                 
### Superviser:	      Sven Wagner
###                   Holger Fischer
### ==================================================================== ###




# =============================================================================-
# Table of content       ----
'
 └──┬┬── A: Import o relevant data
    ││
    │├── 0. Notes
    │├── 1. Install all required packages (Libraries)
    │├── 2. Data Import (Requirements)
    │├── 3. Data preparation
    ││   ├── 3.1 Schotten
    ││   │   ├── 3.1.1 Create Dataframe
    ││   │   ├── 3.1.2 Verification of data quality / Clean Data
    ││   │   ├── 3.1.3 Creation of data subsets by species
    ││   │   └── 4.2.3 Crown height
    ││   └── 3.2 TBA
    │└── 4. TBA 
    │
    └── B. TBA
'
## Change Log: 




# # # # # # # # # # # # # # # # # GENERATE PLOTMAP # # # # # # # # # # # # # # # 
# F. R. Schrewe
# 04.08.2022
# Status: Draft 
# # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # # 


# =============================================================================-
# 0. Notes                                                                  ----
"
#TODO: Im DF vermerken, wie viele Nachbarn pro Baumart der Subplot hat. 












Ich will von den Altbäumen berechnen ob sie in einem Host / Gruppe / Trupp stehen.

Ich brauche: DF mit Koordinaten der Altbäume, deren Höhe / BHD.
                                       ✓

Schleife: Überprüfen ob im Umkreis x dieses Baumes ein weiterer Baum selber Art steht. 
Falls ja: ?

"
# =============================================================================-
# 1. Install all required packages (LIBRARYS)                               ----
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


# =============================================================================-
# 2. Data Import (Requirements)                                             ----
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



# =============================================================================-
# 3. Data preparation                                                       ----

## _____________________________________________________________________________
## 3.1 Schotten                                                             ----

plot.all1(tree_data = data_tree_scho, plots_pos = plots_pos_scho,
         plots_ref = plots_ref_scho)
plots_ref_scho


# Test ob das plotten "auch so" zu plotten geht
plot(data_tree_scho$x, data_tree_scho$y, col=data_tree_scho$art, pch=16, bg=data_tree_scho$art)
length(unique(data_tree_scho$art))



### ............................................................................
### 3.1.1 Create Dataframe                                                  ----

# Create Dataframe with Treedata from Schotten
df <- data.frame(
	nr = c(data_tree_scho$nr),
	x = c(data_tree_scho$x),
	y = c(data_tree_scho$y),
	art = c(data_tree_scho$art.stammv),
	dia = c(data_tree_scho$dmess))


### ............................................................................
### 3.1.2 Verification of data quality / Clean Data                         ---- 
table(df$art)
# remove weird ones without Art
df <- df[!is.na(df$art),]
df <- df[df$art!=0,]


plot(df$x, df$y, col=1, pch=21, bg=adjustcolor(as.numeric(as.factor(df$art)), .5), cex = sqrt(df$dia/100))



# Create complex plane coordinates
df$xy <- df$x + 1i*df$y

plot(df$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(df$dia)/10)  
# text(df$xy, labels = df$nr)

# Remove entries without coordinates
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




### ............................................................................
### 3.1.3 Creation of data subsets by species                               ----

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

df$art_lat <- 0
df[df$art==999,]$art_lat <- "Unbekannte Baumart"      
df[df$art==110,]$art_lat <- "Quercus spec."           
df[df$art==111,]$art_lat <- "Quercus robur"
df[df$art==211,]$art_lat <- "Fagus sylvatica"           
df[df$art==221,]$art_lat <- "Carpinus betulus"         
df[df$art==311,]$art_lat <- "Fraxinus excelsior"        
df[df$art==321,]$art_lat <- "Acer pseudoplatanus"
df[df$art==322,]$art_lat <- "Acer platanoides"
df[df$art==323,]$art_lat <- "Acer campestre"
df[df$art==333,]$art_lat <- "Ulmus minor"
df[df$art==342,]$art_lat <- "Tilia  cordata"
df[df$art==354,]$art_lat <- "Prunus avium"
df[df$art==811,]$art_lat <- "Larix decidua"
df[df$art==555,]$art_lat <- "Weg"
df[df$art==666,]$art_lat <- "Hochsitz"


aggregate(list(Anz = df$art), by = list(Art = df$art_lat), FUN = length)

# Wie wichtig sind die Unbekannten Bauarten?
lines(df$x[df$art=="999"], df$y[df$art=="999"], col=1 , pch=21, bg=adjustcolor(as.numeric(as.factor(df$art)), .5), cex = sqrt(df$dia)/10)
# Das scheinen die Zäune zu sein.

df[df$art=="999",]
df$dia[df$art=="999"]
df[df$art=="999",]$dia


# Löschen von Daten die ich nicht brauche
rm("d555", "d666", "d999")

plot(df$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(df$dia)/10, pch = 1)



### ............................................................................
### 3.1.X Focus Esche                                                       ----

# Versuch mit der Art 311

# Test ob ich von allen Bäumen der Art 311 einzelnd auf die Umliegenden der Art 211 zugreifen kann
pb = txtProgressBar(min = 0, max = nrow(d311), initial = 0, char = ":", style = 3) 
for (cc in 1:nrow(d311)) {
	for (xx in 1:nrow(d211)) {
		if (abs(d311[cc,]$xy-d211[xx,]$xy) <= 6) {  # Maximaler Abstand zwischen Bäumen der betrachteten Art in Metern
			if (abs(d311[cc,]$xy-d211[xx,]$xy) > 0) { # Minimaler Abstand zwischen Bäumen der betrachteten Art in Metern
			arrows(Re(d311[cc,]$xy),Im(d311[cc,]$xy), Re(d211[xx,]$xy), Im(d211[xx,]$xy), length = 0, col = "darkorange", lwd = 3)
			}
		}
	}
	setTxtProgressBar(pb,cc)
	}; close(pb); rm(pb)


# Alle Baumarten suchen nach Artgenosssen im Umkreis x

plot(df$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(df$dia)/10, pch = 1, col=as.numeric(as.factor(df$art)), lwd=2, sub = "Eschen markiert")
points(df[df$art=="311",]$xy, cex=sqrt(df[df$art=="311",]$dia)/10, pch=21, bg=rgb(0,0,0,.5),col=NULL)



ff <- list(d110, d111, d211, d221, d311, d321, d322, d323, d333, d342, d354, d811)
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




d311$gruppierung <- 0
for (cc in 1:nrow(d311)) {
	for (xx in 1:nrow(d311)) {
		if (abs(d311[cc,]$xy-d311[xx,]$xy) <= 6) {
			if (abs(d311[cc,]$xy-d311[xx,]$xy) > 0) {
				d311[cc,]$gruppierung <- d311[cc,]$gruppierung+1
			}	
		}
	}
}
table(d311$gruppierung)

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
points(d311[d311$gruppierung > 0,]$xy, pch = 16, cex = sqrt(d311[d311$gruppierung > 0,]$dia)/10, col = adjustcolor(d311[d311$gruppierung > 0,]$gruppierung, .5))
legend("bottomleft", inset = .02, legend=c(sort(unique(d311[d311$gruppierung > 0,]$gruppierung), decreasing = F)),
			 bg = "white", 
			 pch=21, pt.bg = c(adjustcolor(sort(unique(d311[d311$gruppierung > 0,]$gruppierung), decreasing = F), .5)), 
			 pt.cex=1.5, y.intersp = .75, box.col = "aquamarine3", title = "Anzahl Nachbarn", text.font=11)


### ............................................................................
### 3.1.X Focus Subplots                                                    ----

 # Bekomme ich die Subplots hinein?


plots_pos_scho$xy <- plots_pos_scho$x + 1i*plots_pos_scho$y

plot(df$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(df$dia)/10, pch = 1)
points(plots_pos_scho$xy, pch = 22,bg=3, col=1, srt = 25) # srt geht nicht für pch
# text(plots_pos_scho$xy, labels = "\u25FB", srt = 25, fill=2) # Geht, aber kann man nicht farblich füllen.






# Fokus Subplots: Wie viele Eschen sind im Umkreis?
' Zu bunt '
# plot(df$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
# 		 xlab="Osten in Metern", ylab= "Norden in Metern",
# 		 cex=sqrt(df$dia)/10, pch = 1, col=as.numeric(as.factor(df$art)), lwd=2)
# text(plots_pos_scho$xy, labels = "\u25FB", srt = 25)

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
 0  1  2  3 
48 39 17  3 
48 Sublots haben keine BA 311 im Umkreis von 6m, 39 eine, 17 zwei und drei 3."

# Nun hab ich das aufgeschrieben und muss es noch visualisieren
pb = txtProgressBar(min = 0, max = nrow(d311), initial = 0, char = ":", style = 3) 
for (cc in 1:nrow(plots_pos_scho)) {
	for (xx in 1:nrow(d311)) {
		if (abs(plots_pos_scho[cc,]$xy-d311[xx,]$xy) <= 6) {  # Maximaler Abstand zwischen Bäumen der betrachteten Art in Metern
			arrows(Re(plots_pos_scho[cc,]$xy),Im(plots_pos_scho[cc,]$xy), Re(d311[xx,]$xy), Im(d311[xx,]$xy), length = 0, col = "purple", lwd = 2)
		}
	}
	setTxtProgressBar(pb,cc)
}; close(pb); rm(pb)

# Und nun für alle Baumarten nacheinander
plot(df$xy, main="Stem distribution map \nSchotten", #xlim=c(0,30),ylim = c(0,30),
		 xlab="Osten in Metern", ylab= "Norden in Metern",
		 cex=sqrt(df$dia)/10, pch = 1)
points(plots_pos_scho$xy, pch = 22,bg=3, col=1, srt = 25) # srt geht nicht für pch
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


#TODO: Im DF vermerken, wie viele Nachbarn pro Baumart der Subplot hat.


### ............................................................................
### 3.1.X Focus Subplots Naturverjüngung                                    ----


dfn <- data_nv[data_nv$Flaeche=="Schotten",]


#### Daten bereinigen

#### Remove rows with Baumart == "NA"
dfn[dfn$Baumart_kurz=="NA",]
aggregate(list(Anz = dfn$Baumart_lang), by = list(Art = dfn$Baumart_lang), FUN = length, na.action=na.exclude)
table(dfn$Baumart_kurz)

aggregate(dfn$Flaeche ~ dfn$Baumart_kurz, data=dfn, FUN=length)
table(dfn$Baumart_lang, useNA = "always")

library(dplyr)

dfn %>% 
	count(Baumart_lang)

dfn <- dfn[!is.na(dfn$Baumart_kurz),]

#### Remove rows of Eschen ohne Wert bei "Anzahl Triebe"

sum(is.na(dfn[dfn$Baumart_kurz=="GEs",]$Anzahl.Triebe))
dfn[dfn$Baumart_kurz=="GEs",] %>% 
	count(Anzahl.Triebe)

subset(dfn, is.na(dfn$Anzahl.Triebe) & dfn$Baumart_kurz=="GEs")[,c(1,4, 13, 15, 16, 17, 18)]

# Alle Einträge
nrow(dfn)
# Fehlerhafte
nrow(
dfn[dfn$Baumart_kurz=="GEs" & is.na(dfn$Anzahl.Triebe),]
)

nrow(
dfn[!is.na(dfn$Anzahl.Triebe) | dfn$Baumart_kurz!="GEs",]
# keep all rows that are not NA or where the species is no GEs 
)
dfn <- dfn[!is.na(dfn$Anzahl.Triebe) | dfn$Baumart_kurz!="GEs",]



#### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
#### 3.1.X.X Berechnung Anteil infizierter Triebe                           ----

dfn$Anzahl.Triebe


dfn$ETS.Anteil.Triebe <- 

rowSums(dfn[,c("ETS.lebend", "ETS.abgestorben.frisch", "ETS.abgestorben.alt")], na.rm=TRUE) /
	dfn$Anzahl.Triebe * 100

dfn[dfn$Baumart_kurz=="GEs",] %>% 	count(ETS.Anteil.Triebe)





#### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
#### 3.1.X.X Berechnung des durchschnittlichen ETS-Grades pro Subplot       ----

# Test mit Subplot 79
plots_pos_scho$Anteil.ETS.Triebe <- 0
plots_pos_scho[79,]$Anteil.ETS.Triebe <-
sum(dfn[dfn$Plotnummer=="79" & dfn$Baumart_kurz=="GEs",]$ETS.Anteil.Triebe) / # Alle ETS Werte perSubplot
nrow(dfn[dfn$Plotnummer=="79" & dfn$Baumart_kurz=="GEs",]) # Wie viele Eschen habe ich auf dem Subplot?


# Als schleife:
plots_pos_scho$Anteil.ETS.Triebe <- 0
for (cc in 1:nrow(plots_pos_scho)){
	plots_pos_scho[cc,]$Anteil.ETS.Triebe <-
		sum(dfn[dfn$Plotnummer==cc & dfn$Baumart_kurz=="GEs",]$ETS.Anteil.Triebe) / # Alle ETS Werte perSubplot
		nrow(dfn[dfn$Plotnummer==cc & dfn$Baumart_kurz=="GEs",]) # Wie viele Eschen habe ich auf dem Subplot?
}



#### . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . . 
#### 3.1.X.X Berechnung des Anteils infizierter Eschen pro Subplot          ----

# Test mit Subplot 79
plots_pos_scho$Anteil.ETS.indi <- 0
plots_pos_scho[79,]$Anteil.ETS.indi <-
sum(dfn[dfn$Plotnummer=="79" & dfn$Baumart_kurz=="GEs",]$ETS==TRUE) / # Alle Eschen auf Subplot mit ETS
nrow(dfn[dfn$Plotnummer=="79" & dfn$Baumart_kurz=="GEs",]) *100 # Alle Eschen auf Subplot


# Als schleife:
plots_pos_scho$Anteil.ETS.indi <- 0
for (cc in 1:nrow(plots_pos_scho)){
	plots_pos_scho[cc,]$Anteil.ETS.indi <-
		sum(dfn[dfn$Plotnummer==cc & dfn$Baumart_kurz=="GEs",]$ETS==TRUE) / # Alle Eschen auf Subplot mit ETS
		nrow(dfn[dfn$Plotnummer==cc & dfn$Baumart_kurz=="GEs",]) *100 # Alle Eschen auf Subplot
}












# Anzahl aller Verjüngungspflanzen pro Subplot
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
plots_pos_scho[79,] # Kontrolle


# Als schleife:
for (cc in 1:nrow(plots_pos_scho)){
	plots_pos_scho[cc,]$n.fe <- nrow(dfn[dfn$Plotnummer==cc & dfn$Baumart_kurz=="GEs",])
	plots_pos_scho[cc,]$n.ets <- nrow(dfn[dfn$Plotnummer==cc & dfn$ETS==TRUE,])
	plots_pos_scho[cc,]$n.ba <- nrow(dfn[dfn$Plotnummer==cc,])
}


# Da sind aber noch die ohne Bäume drin.
# Edit: Nicht mehr.

dfn[dfn$Plotnummer==2,]$keine.baeume==TRUE
for (cc in 1:nrow(plots_pos_scho)){
	if((dfn[dfn$Plotnummer==cc,]$keine.baeume==TRUE)[1]){ # [1] Weil wenn ein Subplot keine Bäume hat, also FALSE ist und meh-
		print(cc)                                           # rere Bäume hat nimmt er nur den ersten Eintrag.. sind ja eh alle FALSE
		plots_pos_scho[cc, names(plots_pos_scho) %in% c("n.fe", "n.ba")] <- plots_pos_scho[cc, names(plots_pos_scho) %in% c("n.fe", "n.ba")] -1
	} else{}
}
table(plots_pos_scho$n.fe)

plots_pos_scho
## Den bonitierschlüssel anwenden... sonst wird das nichts

# Ende (wirklich) ####

df$xy[df$art=="311"]

aa <- plots_pos_scho$xy[70]


dexp(abs(aa-df$xy[df$art=="311"]), rate = 0.01)*df$dia[df$art=="311"]

df$dis[df$art=="311"] <- abs(aa-df$xy[df$art=="311"])

	
plot(df$dexp[df$art=="311"] ~ df$dis[df$art=="311"])



# Ende





bb <- df[df$art=="311",][36,]
cc <- df[df$art=="311",][15,]

abs(aa-bb$xy)
abs(aa-cc$xy)

bb$dia*1/abs(aa-bb$xy)
cc$dia*1/abs(aa-cc$xy)



bb$dia
abs(aa-bb$xy)
sqrt(bb$dia)/abs(aa-bb$xy)

cc$dia
abs(aa-cc$xy)
sqrt(cc$dia)/abs(aa-cc$xy)





bb$dia
abs(aa-bb$xy)
bb$dia*log(abs(aa-bb$xy))

cc$dia
abs(aa-cc$xy)
cc$dia*log(abs(aa-cc$xy))

log(bb$dia*abs(aa-bb$xy))

log(cc$dia*abs(aa-cc$xy))

de <- data.frame(
	dia = c(rep(50,100)),
	dis = c(round(seq(2,101,1)))
)


de <- data.frame(
	dia = c(seq(10,75,1),rep(50,34)),
	dis = c(rep(10,66), round(seq(1,100,100/34)))
)


de$dis <- de$dis + 1i*1

aa <- 1 + 1i*1

abs(aa-de$dis)



plot(dexp(abs(aa-de$dis), rate = 0.05), type = "l")
lines(dexp(abs(aa-de$dis), rate = 0.04))
lines(dexp(abs(aa-de$dis), rate = 0.03))
lines(dexp(abs(aa-de$dis), rate = 0.02))
lines(dexp(abs(aa-de$dis), rate = 0.01))

plot(
dexp(abs(aa-de$dis))*de$dia
, type ="l")












#  Ende

plot(seq(1,100,1), type = "l")
points(sqrt(seq(1,100,1)), col = 2, type = "l")
points(log(seq(1,100,1)), col = 3, type = "l")


















# Bonitur anhand Anteil infizierter Triebe


#TODO 24 Bäume haben keine Art. Klären.
aggregate(list(Anz = dfn$Baumart_lang), by = list(Plot = dfn$Baumart_lang), FUN = length)
sum(aggregate(list(Anz = dfn$Baumart_lang), by = list(Plot = dfn$Baumart_lang), FUN = length)[2])
nrow(dfn)
sum(is.na(dfn$Baumart_kurz))

dfn$Anzahl.Triebe[dfn$Baumart_kurz=="GEs"]
sum(!is.na(dfn$Anzahl.Triebe[dfn$Baumart_kurz=="GEs"]))


length(dfn$ETS.abgestorben.alt[dfn$Baumart_kurz=="GEs"])
	dfn$ETS.abgestorben.frisch[dfn$Baumart_kurz=="GEs"]
	dfn$ETS.lebend[dfn$Baumart_kurz=="GEs"]








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

