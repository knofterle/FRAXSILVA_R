#============================ ETS_ANTEIL_HOEHE =================================#
# J.Osewold
# 01.07.22
# STATUS = NEW
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/Goe_Lau/Load and Clean NV Data.R")
source(file = "SCRIPT/IBF/LOAD_NV.R")
# 
# data_nv
# nv 
# 

## LIBRARYS --------------------------------------------------------------------
library(dplyr)

## NOTES -----------------------------------------------------------------------
# Das hier bastelt einen Plot aus den NV Daten aus Goe/Lau und IBF. Der 
# funktioniert ganz ähnlich wie der Graph ETS Anteil ~ Hoehe ~ Zuwachs  für die 
# IBF Daten.

## HARMONIZE DATESET  ----------------------------------------------------------
nv_gl <- nv
nv_gl$Hoehe <- nv_gl$Hoehe*10 # cm to mm

nv_ibf <- data_nv
nv_gl$Versuch <- "Goe_Lau"
nv_ibf$Versuch <- "IBF"
nv_all <- bind_rows(nv_gl, nv_ibf) 
# This is similar to rbind() but all missing columns are filled with NA
# count(nv_all) 24609

## DATA FILTER  ----------------------------------------------------------------
tmp <-
	nv_all %>%
	filter(Baumart_kurz == "GEs") %>%
	filter(Hoehe <= 500) %>%
	filter(!(
		Versuch == "IBF" &
			eigentlich.aelter == F &
			is.na(Hoehe.Vorvorjahr)
	)) %>%   # All three must be TRUE to exclude this line
	filter(!(Versuch == "Goe_Lau" & Einjaehriger.Saemling == T))

# start 24609
# count(tmp) 7869

## SUMMARIZE DATA BY HEIGHT GROUPS  --------------------------------------------
steps <- seq(from = 0, to = 500, by = 12.5)
table_ets_hist_gl <-
	tmp %>%
	filter(Versuch == "Goe_Lau") %>% 
	mutate(hist_step = cut(
		x = Hoehe,
		breaks = steps,
		labels = steps[1:40] + 3.75
	)) %>%
	group_by(hist_step, .drop = F) %>%
	summarise(n = n(), ETS_ratio = sum(ETS) / n() * 100) %>% 
	mutate(Versuch = "Goe_Lau") %>% 
	mutate(hist_step = as.numeric(as.character(hist_step))) %>% 
	mutate(hist_step_point = hist_step + 2.5)
	# Es war nötig die hist_steps für die Punkte zu erhöhen damit zumindest die 
	# Punkte auf einer x-Höhe liegen, die Columns mussten ein bisschen nach rechts
	# bzw links damit sie sich nicht überdecken

table_ets_hist_ibf <- 
	tmp %>% 
	filter(Versuch == "IBF") %>% 
	mutate(hist_step = cut(
		x = Hoehe,
		breaks = steps,
		labels = steps[1:40] + 8.75
	)) %>% 
	group_by(hist_step, .drop = F) %>% 
	summarise(n = n(), ETS_ratio = sum(ETS)/ n() * 100) %>% 
	mutate(Versuch = "IBF") %>% 
	mutate(hist_step = as.numeric(as.character(hist_step))) %>% 
	mutate(hist_step_point = hist_step - 2.5)

table_ets_hist <- rbind(table_ets_hist_gl, table_ets_hist_ibf)

## GGPLOT  ---------------------------------------------------------------------
plot <-
	ggplot(table_ets_hist) +
	geom_col(aes(x = hist_step,
							 y = n,
							 color = Versuch, 
							 fill = Versuch),
					 width = 5) +
	geom_point(
		aes(x = hist_step_point,
				y = ETS_ratio * 10,
				fill = Versuch),
		shape =  21,
		color = "black",
		size = 2
	)+
	scale_y_continuous(name = "Anzahl [total = 7869]",
										 sec.axis =  sec_axis(trans = ~ . / 10,
										 										 name = "ETS Anteil [%]")) +
	scale_x_continuous(
		name = "Hoehenklassen [mm]",
		breaks = seq(0, 500, 50),
		labels = seq(0, 500, 50)
	) +
	 scale_fill_manual(
	# 	name = "Versuch",
	 	values = c("cadetblue", "chocolate"),
	# 	labels = c("Goe_Lau", "IBF")
	 ) +
	 scale_color_manual(
	# 	name = "Versuch",
	 	values = c("cadetblue", "chocolate"),
	# 	labels = c("Goe_Lau", "IBF")
	 ) +
	annotate(
		geom = "text",
		x = 150,
		y = 850,
		hjust = 0,
		label =
"> filter (hoehe < 500)
> filter (alter > 2j) oder filter (alter != 1j)",
		size = 4
	)
plot
ggsave(
	plot = plot,
	filename = "EXPORT/NV_combined/figures/ETS_Anteil_Hoehe.pdf",
	units = "mm",
	width = 300,
	height = 150
)


## TIDY UP  --------------------------------------------------------------------
rm(table_ets_hist, table_ets_hist_gl, table_ets_hist_ibf, tmp, plot, nv_gl, 
	 steps, nv_all, nv_ibf)

## OUTPUT ----------------------------------------------------------------------
# plot at EXPORT/Combined/figures/ETS_Anteil_Hoehe.pdf



## JUNK ------------------------------------------------------------------------
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

