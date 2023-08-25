#============================ ETS_ANTEIL_HOEHE =================================#
# J.Osewold
# 01.07.22
# STATUS = NEW
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/NV_combined/Combine_NV.R", encoding = "UTF-8")
# nv_all
# Die NV gl Daten sind von 2022

## LIBRARYS --------------------------------------------------------------------
library(dplyr)
library(ggplot2)

## NOTES -----------------------------------------------------------------------
# Das hier bastelt einen Plot aus den NV Daten aus Goe/Lau und IBF. Der 
# funktioniert ganz ähnlich wie der Graph ETS Anteil ~ Hoehe ~ Zuwachs  für die 
# IBF Daten.
# 
# Es gibt einen Plot bei dem der ETS Anteil von gl 2022 und ibf kombiniert wird und 
# einen bei dem beides getrennt steht 

## DEFINE HEIGHTCLASSES AND MAXIMA ---------------------------------------------

maxHoehe <- 500

heightStep <- 10


## DATA FILTER  ----------------------------------------------------------------
tmp <- nv_all

# Die Höhen sollen nach den toten Höhen sortiert werden.
# Zuerst hatte ich es mit den grünen getestet aber dann, sind sehr Eschen die
# bis ganz runter abgestorben sind plötzlich in den sehr "jungen" Säulen
 tmp$Hoehe[!is.na(tmp$tote.Hoehe)] <- tmp$tote.Hoehe[!is.na(tmp$tote.Hoehe)]

tmp <-
	tmp %>%
	filter(Baumart_kurz == "GEs") %>%
	filter(Hoehe <= maxHoehe) %>%
	filter(!(
		Versuch == "IBF" &
			eigentlich.aelter == F &
			is.na(Hoehe.Vorvorjahr)
	)) %>%   # All three must be TRUE to exclude this line
	filter(!(Versuch == "Goe_Lau" & Einjaehriger.Saemling == T))  %>% 
	filter(tot == F | is.na(tot)) %>% 
	filter(Tot_22 == F| is.na(Tot_22))
	# Die Daten der jeweils anderen Versuchsfläche sind ja mit NA aufgefüllt
	# daher der kompliziertere Filter

# start 24609
# count(tmp) 10751

## SUMMARIZE DATA BY HEIGHT GROUPS  --------------------------------------------
steps <- seq(from = 0, to = maxHoehe, by = heightStep)
table_ets_hist_gl <-
	tmp %>%
	filter(Versuch == "Goe_Lau") %>% 
	mutate(hist_step = cut(
		x = Hoehe,
		breaks = steps,
		labels = steps[1:(maxHoehe/heightStep)] + 3
	)) %>% 
	# Die labels müssen etwas versetzt zu den breaks und versetzt zueinander 
	# (siehe table_ets_hist_ibf) sein damit die columns am Ende an der richtigen 
	# Stelle und dich beieinander stehen.
	group_by(hist_step, .drop = F) %>%
	summarise(n = n(), ETS_ratio = sum(ETS, na.rm = T) / n() * 100) %>% 
	mutate(Versuch = "Goe_Lau") %>% 
	mutate(hist_step = as.numeric(as.character(hist_step))) %>% 
	mutate(hist_step_point = hist_step + 2)
	# Es war nötig die hist_steps für die Punkte zu erhöhen damit zumindest die 
	# Punkte auf einer x-Höhe liegen, die Columns mussten ein bisschen nach rechts
	# bzw links damit sie sich nicht überdecken

table_ets_hist_gl <- table_ets_hist_gl[1:(maxHoehe/heightStep),] 
	# Aus unerfindlichen Gründen hat sich am Ende noch eine Zeile entwickelt die
	# bei Steps NA hat. Und ich habe gerade keine Zeit zu ergründen wo die her kommt.
	# Ich konnte das nicht reproduzieren, jetzt tut diese Zeile nichts mehr...

table_ets_hist_ibf <- 
	tmp %>% 
	filter(Versuch == "IBF") %>% 
	mutate(hist_step = cut(
		x = Hoehe,
		breaks = steps,
		labels = steps[1:(maxHoehe/heightStep)] + 7
	))  %>% 
	group_by(hist_step, .drop = F) %>% 
	summarise(n = n(), ETS_ratio = sum(ETS, na.rm = T)/ n() * 100) %>% 
	mutate(Versuch = "IBF") %>% 
	mutate(hist_step = as.numeric(as.character(hist_step))) %>% 
	mutate(hist_step_point = hist_step - 2)


table_ets_hist <- rbind(table_ets_hist_gl, table_ets_hist_ibf)

## GGPLOT gl ibf getrennt  ---------------------------------------------------------------------
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
	scale_y_continuous(name = paste0("Anzahl [total = ", 
																	 nrow(tmp), 
																	 "]"),
										 sec.axis =  sec_axis(trans = ~ . / 5,
										 										 name = "ETS Anteil [%]")) +
	scale_x_continuous(
		name = "Hoehenklassen [mm]",
		breaks = seq(0, maxHoehe, (maxHoehe/heightStep)),
		labels = seq(0, maxHoehe, (maxHoehe/heightStep))
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
			paste0("> filter (hoehe < ", 
						 maxHoehe, 
						 ")
> filter (alter > 2j) oder filter (alter != 1j)"),
		size = 4
	)
plot
ggsave(
	plot = plot,
	filename = "EXPORT/NV_combined/figures/ETS_Anteil_Hoehe_all_2022.pdf",
	units = "mm",
	width = 300,
	height = 150
)

## GGPLOT gl ibf kombiniert  ---------------------------------------------------------------------

# Ab hier werden die beiden ETS Anteile kombiniert

table_ets_combined_hist <- table_ets_hist %>% 
	filter(Versuch == "IBF") %>% 
	select(ETS_ratio, hist_step_point)
# Der Filter Versuch ist nur dazu da die hälfte der Tabelle zu exportieren

df <-	table_ets_hist %>% filter(Versuch == "IBF") %>% select(ETS_ratio) * 0.5 +
	table_ets_hist %>% filter(Versuch == "Goe_Lau") %>% select(ETS_ratio) * 0.25 * 0.5
table_ets_combined_hist$ETS_ratio <- df$ETS_ratio
# Da gab es wieder eines der üblichen Dataframe zu Vektor Problemen, daher der 
# Zwischenschritt

plot <-
	ggplot(table_ets_hist) +
	geom_col(aes(x = hist_step,
							 y = n,
							 color = Versuch, 
							 fill = Versuch),
					 width = 4) +
	geom_point(
		data = table_ets_combined_hist,
		aes(x = hist_step_point,
				y = ETS_ratio * 5),
		shape =  21,
		color = "black",
		fill = "gray",
		size = 2
	) +
	geom_smooth(
		data = table_ets_combined_hist,
		aes(x = hist_step_point,
		y = ETS_ratio * 5
	),
	color = "black",
	fill = "gray",
	alpha = 0.2,
	method = "lm") +
	scale_y_continuous(name = "Anzahl [total = 10887]",
										 sec.axis =  sec_axis(trans = ~ . / 5,
										 										 name = "ETS Anteil [%]")) +
	scale_x_continuous(
		name = "Hoehenklassen [mm]",
		breaks = seq(0, maxHoehe, (maxHoehe/heightStep)),
		labels = seq(0, maxHoehe, (maxHoehe/heightStep))
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
			paste0("> filter (hoehe < ", 
						 maxHoehe, 
						 ")
> filter (alter > 2j) oder filter (alter != 1j)"),
size = 4
	)
plot
ggsave(
	plot = plot,
	filename = "EXPORT/NV_combined/figures/ETS_Anteil_Hoehe_all_2022_combined.pdf",
	units = "mm",
	width = 300,
	height = 150
)


## TIDY UP  --------------------------------------------------------------------
rm(table_ets_hist, table_ets_hist_gl, table_ets_hist_ibf, tmp, plot, nv_gl, 
	 steps, nv_all, nv_ibf, df)

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

