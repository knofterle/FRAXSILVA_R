#================== ETS_Anteil_Hoehe_pValue ===================================#
# J.Osewold
# DATE
# STATUS
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
# This runs all previously necessary scripts
source(file = "SCRIPT/NV_combined/ETS_Anteil_Hoehe.R", encoding = "UTF-8")
# 
# The data and variables from the previous scripts can be listed here

## LIBRARYS -------------------------------------------------------------------
require(mgcv)

## NOTES -----------------------------------------------------------------------


## STATISTICS  ---------------------------------------------------------------------

tmp$Flaeche <- as.factor(tmp$Flaeche)

m1 <- gam(ETS ~ s(Hoehe) + s(Flaeche, bs = 're'), data = tmp, family = binomial)
summary(m1)
plot(m1, pages = 1, all.terms = T)

m2 <- gam(ETS ~ s(Hoehe), data = tmp, family = binomial)
summary(m2)
plot(m2, pages = 1, all.terms = T)

m3 <- gam(ETS ~ Hoehe, data = tmp, family = binomial)
summary(m3)

m4 <- gam(ETS ~ I(poly(Hoehe, degree = 2)) + s(Flaeche, bs = 're'), family = binomial, data = tmp)
summary(m4)
plot(m4, pages = 1, all.terms = T)

m5 <- gam(ETS ~ I(poly(Hoehe, degree = 2)), family = binomial, data = tmp)
summary(m5)
plot(m5, pages = 1, all.terms = T)

m6 <- gam(ETS ~ I(Hoehe^2) + s(Flaeche, bs = 're'), family = binomial, data = tmp)
summary(m6)
plot(m6, pages = 1, all.terms = T)

m7 <- gam(ETS ~ I(Hoehe^2), family = binomial, data = tmp)
summary(m7)
plot(m7, pages = 1, all.terms = T)

## DATA PREDICTION ---------------------------------------------------------------------

nd <- data.frame('Hoehe' = 0:maxHoehe)
pre <- predict(m2, newdata = nd, exclude = 's(Flaeche)', newdata.guaranteed = T, se.fit = T)

nd$pre <- as.numeric(plogis(pre$fit))
nd$lo <- as.numeric(plogis(pre$fit+1.96*pre$se.fit))  
nd$up <- as.numeric(plogis(pre$fit-1.96*pre$se.fit))
# Die 1.96  sind für das 95% Konfidenzintervall.

ggplot() + 
	# geom_point(aes(x = Hoehe, y = ETS)) +
 	geom_ribbon(data = nd, aes(x = Hoehe, ymin = lo, ymax = up)) + 
	geom_line(data = nd, aes(x = Hoehe, y = pre)) +
	geom_point(data = table_ets_hist_comb, aes(x = hist_step_point, y = ETS_ratio/100))


## GRAPH ---------------------------------------------------------------------
# Statt nach Datenherkunft werden die beiden Säulenfarben nun nach ETS T/F 
# sortiert das ist ja wesentlich logischer.

table_ets_hist_T <-
	tmp %>%
	filter(ETS == T) %>% 
	mutate(hist_step = cut(
		x = Hoehe,
		breaks = steps,
		labels = steps[1:(maxHoehe/heightStep)] + 3
	)) %>% 
	# Die labels müssen etwas versetzt zu den breaks und versetzt zueinander 
	# (siehe table_ets_hist_ibf) sein damit die columns am Ende an der richtigen 
	# Stelle und dich beieinander stehen.
	group_by(hist_step, .drop = F) %>%
	summarise(n = n()) %>% 
	mutate(ETS = TRUE) %>% 
	mutate(hist_step = as.numeric(as.character(hist_step))) %>% 
	mutate(hist_step_point = hist_step + 2)
# Es war nötig die hist_steps für die Punkte zu erhöhen damit zumindest die 
# Punkte auf einer x-Höhe liegen, die Columns mussten ein bisschen nach rechts
# bzw links damit sie sich nicht überdecken

# table_ets_hist_T <- table_ets_hist_T[1:(maxHoehe/heightStep),] 
# Aus unerfindlichen Gründen hat sich am Ende noch eine Zeile entwickelt die
# bei Steps NA hat. Und ich habe gerade keine Zeit zu ergründen wo die her kommt.
# Ich konnte das nicht reproduzieren, jetzt tut diese Zeile nichts mehr...

table_ets_hist_F <- 
	tmp %>% 
	filter(ETS == F) %>% 
	mutate(hist_step = cut(
		x = Hoehe,
		breaks = steps,
		labels = steps[1:(maxHoehe/heightStep)] + 7
	))  %>% 
	group_by(hist_step, .drop = F) %>% 
	summarise(n = n()) %>% 
	mutate(ETS = FALSE) %>% 
	mutate(hist_step = as.numeric(as.character(hist_step))) %>% 
	mutate(hist_step_point = hist_step - 2)

table_ets_hist_TF <- rbind(table_ets_hist_T, table_ets_hist_F)
table_ets_hist_TF$ETS <- 
	factor(table_ets_hist_TF$ETS, levels = c("TRUE", "FALSE")) 

######################

plot <-
	ggplot(data = table_ets_hist_TF) +
	geom_col(aes(x = hist_step_point,
							 y = n,
							 color = ETS, 
							 fill = ETS),
					 width = 6,
					 position = "stack") +
	geom_ribbon(data = nd, aes(
		x = Hoehe,
		ymin = lo * 1000,
		ymax = up * 1000
	),
	fill = "grey",
	alpha = .5) +
	geom_line(data = nd, aes(x = Hoehe, y = pre * 1000),
						alpha = .5) +
	geom_point(
		data = table_ets_hist_comb,
		aes(x = hist_step_point,
				y = ETS_ratio * 10),
		shape =  21,
		color = "black",
		fill = "black",
		size = 2
	) +
	# geom_smooth(
	# 	data = table_ets_hist_comb,
	# 	aes(
	# 		x = hist_step_point,
	# 		y = ETS_ratio * 10
	# 	),
	# 	alpha = 0.2,
	# 	method = "glm"
	# ) +
	scale_y_continuous(name = paste0("Individuals [n = ", 
																	 nrow(tmp), 
																	 "]"),
										 sec.axis =  sec_axis(trans = ~ . / 10,
										 										 name = "Ashdieback ratio [%]")) +
	scale_x_continuous(
		name = "Heighclasses [mm]",
		breaks = seq(0, maxHoehe, (maxHoehe/heightStep)),
		labels = seq(0, maxHoehe, (maxHoehe/heightStep))
	) +
	scale_fill_manual(
		name = "ADB",
		values = c("#e5420f", "#a2c617"),
		labels = c("TRUE", "FALSE")
	) +
	scale_color_manual(
		name = "ADB",
		values = c("#e5420f", "#a2c617"),
		labels = c("TRUE", "FALSE")
	) 
plot
ggsave(
	plot = plot,
	filename = "EXPORT/NV_combined/figures/ETS_Anteil_Hoehe_pValue.pdf",
	units = "mm",
	width = 300,
	height = 150
)


## TEST BY AREA ---------------------------------------------------------------------
# Hier habe ich mal getestet ob sich der Zuwachs des ETS Anteils auch auf allen
# Versuchsflächen wiederholen lässt. UNd es sieht so weit sehr gut aus. Ist aber 
# nun auskommentiert, weil war ja nur ein Test
# 
# nv_all2 <- nv_all
# 
# nv_all <- nv_all2 %>% filter(Flaeche == "Greifswald")
# nv_all <- nv_all2 %>% filter(Flaeche == "Leutzsch")
# nv_all <- nv_all2 %>% filter(Flaeche == "Mollenfelde")
# nv_all <- nv_all2 %>% filter(Flaeche == "Plattenwald")
# nv_all <- nv_all2 %>% filter(Flaeche == "Schotten")
# nv_all <- nv_all2 %>% filter(Flaeche == "Stegelitz")
# nv_all <- nv_all2 %>% filter(Flaeche == "Weisweil")
# 
# nv_all <- nv_all2
# ggplot(data = tmp) +
#  	geom_violin(aes(x = Hoehe, y = Flaeche), scale = "count") +
#  	facet_grid(cols = vars(ETS))

## TIDY UP  --------------------------------------------------------------------
rm()

## OUTPUT ----------------------------------------------------------------------
# 



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

