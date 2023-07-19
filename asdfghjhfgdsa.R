

plot_height_growth_distr <- 
	ggplot(data = table_ets_height, aes(x = xmid, y = n_ash)) +
	geom_col(aes(y = n_ash)) +
	geom_point(aes(y = growth_mean * 5)) +
	scale_y_continuous(
		name = tmp2,
		# Add a second axis and specify its features
		sec.axis = sec_axis(trans = ~ . / 5,
												name = "Zuwachs [mm]")) +
	labs(title = "Eschen nach Hoehenklassen und jeweiliger Hoehenzuwachs",
			 subtitle = tmp,
			 x = ("Hoehe [mm]"))

plot_height_growth_distr


str(data_nv_tmp)



data_nv_tmp2 <- data_nv_tmp 
data_nv_tmp2$Flaeche [data_nv_tmp2$Flaeche %in% c("Weisweil", "Greifswald", "Mollenfelde")] <-
	"other"
data_nv_tmp2 <- data_nv_tmp2 %>% 
	arrange(Flaeche)

ggplot() +
	geom_histogram(data = data_nv_tmp2 %>% filter(Hoehe < 500), aes(x = Hoehe, fill = Flaeche))

data_nv_tmp2$Flaeche <- factor(data_nv_tmp2$Flaeche, levels = order, ordered = T)
order <- c("Ettersberg", "Huy", "Leutzsch", "Plattenwald", "Schotten", "Stegelitz", "other" )
