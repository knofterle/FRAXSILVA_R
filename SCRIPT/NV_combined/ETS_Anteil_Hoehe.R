#============================ ETS_ANTEIL_HOEHE =================================#
# J.Osewold
# 01.07.22
# STATUS = NEW
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/NV_combined/Combine_NV.R")
# nv_all
# 

## LIBRARYS --------------------------------------------------------------------
library(dplyr)
library(ggplot2)

## NOTES -----------------------------------------------------------------------
# Das hier bastelt einen Plot aus den NV Daten aus Goe/Lau und IBF. Der 
# funktioniert ganz ähnlich wie der Graph ETS Anteil ~ Hoehe ~ Zuwachs  für die 
# IBF Daten.


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
steps <- seq(from = 0, to = 500, by = 10)
table_ets_hist_gl <-
	tmp %>%
	filter(Versuch == "Goe_Lau") %>% 
	mutate(hist_step = cut(
		x = Hoehe,
		breaks = steps,
		labels = steps[1:50] + 3
	)) %>% 
	# Die labels müssen etwas versetzt zu den breaks und versetzt zueinander 
	# (siehe table_ets_hist_ibf) sein damit die columns am Ende an der richtigen 
	# Stelle und dich beieinander stehen.
	group_by(hist_step, .drop = F) %>%
	summarise(n = n(), ETS_ratio = sum(ETS) / n() * 100) %>% 
	mutate(Versuch = "Goe_Lau") %>% 
	mutate(hist_step = as.numeric(as.character(hist_step))) %>% 
	mutate(hist_step_point = hist_step + 2)
	# Es war nötig die hist_steps für die Punkte zu erhöhen damit zumindest die 
	# Punkte auf einer x-Höhe liegen, die Columns mussten ein bisschen nach rechts
	# bzw links damit sie sich nicht überdecken

table_ets_hist_ibf <- 
	tmp %>% 
	filter(Versuch == "IBF") %>% 
	mutate(hist_step = cut(
		x = Hoehe,
		breaks = steps,
		labels = steps[1:50] + 7
	)) %>% 
	group_by(hist_step, .drop = F) %>% 
	summarise(n = n(), ETS_ratio = sum(ETS)/ n() * 100) %>% 
	mutate(Versuch = "IBF") %>% 
	mutate(hist_step = as.numeric(as.character(hist_step))) %>% 
	mutate(hist_step_point = hist_step - 2)

table_ets_hist <- rbind(table_ets_hist_gl, table_ets_hist_ibf)

## GGPLOT  ---------------------------------------------------------------------
plot <-
	ggplot(table_ets_hist) +
	geom_col(aes(x = hist_step,
							 y = n,
							 color = Versuch, 
							 fill = Versuch),
					 width = 4) +
	geom_point(
		aes(x = hist_step_point,
				y = ETS_ratio * 5,
				fill = Versuch),
		shape =  21,
		color = "black",
		size = 2
	) +
	geom_smooth(aes(
		x = hist_step_point,
		y = ETS_ratio * 5,
		color = Versuch,
		fill = Versuch
	),
	alpha = 0.2,
	method = "lm") +
	scale_y_continuous(name = "Anzahl [total = 7869]",
										 sec.axis =  sec_axis(trans = ~ . / 5,
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
		y = 450,
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

