#======================= NV GROWTH  ~ ETS =====================================#
# J.Osewold
# 22.04.2022
# NEW
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------
library(dplyr)
library(ggplot2)

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/IBF/LOAD_NV.R")
# data_nv
# 

## NOTES -----------------------------------------------------------------------


## GROWTH HISTOGRAM ------------------------------------------------------------
tmp_data_nv <- 
  data_nv %>% 
  filter(Baumart_kurz == "GEs") 

tmp_data_nv$growth1 <- tmp_data_nv$Hoehe - tmp_data_nv$Hoehe.Vorjahr
tmp_data_nv$growth2 <- tmp_data_nv$Hoehe.Vorjahr - tmp_data_nv$Hoehe.Vorvorjahr

nrow(tmp_data_nv)
tmp_data_nv <-
  tmp_data_nv %>% 
  filter(growth1 >= 0) %>% 
  filter(growth2 >= 0) %>% 
  filter(growth1 < 400 & growth2 < 400) %>% 
  filter(!is.na(growth1)) %>% 
  filter(!is.na(growth2)) %>% 
  filter(mehr.ressourcen == F & nicht.gerade == F)
nrow(tmp_data_nv)


# FUCK so ungefähr 1200 Eschen haben ein NA in den Hoehen... 
# und 1083 in dem älteren Höhenwachstum
# und nur 475 im jüngeren Wachstum
# d.h. etwa 608 sind nur zwei Jahre alt
# außerdem wurden 4 Stück mit einem Zuwachs von mehr als 40 cm ausgeschlossen
# 19 Eschen haben ein negatives wachstum die sind auch raus
# 80 Eschen haben einen Marker bei nicht.gerade oder mehr.ressourcen, die sind
# auch raus

boxplot(tmp_data_nv$growth1, tmp_data_nv$growth2)
wilcox.test(tmp_data_nv$growth1, tmp_data_nv$growth2, paired = T) 
# p-value = 0.01238

tmp_data_nv$growth_mean <- (tmp_data_nv$growth1 + tmp_data_nv$growth2) / 2
tmp_data_nv <- 
  tmp_data_nv %>%
  filter(growth_mean < 100)
nrow(tmp_data_nv)
# Fuer die Asthetik fliegen nochmal 27 mit mehr als 100 Zuwachs raus

plot_growth_hist <-
  tmp_data_nv %>%
  ggplot(aes(x = growth_mean)) +
  geom_histogram(binwidth = 2, center = 1) +
  labs(title = "Histogramm des durchschnittlichen Höhenzuwachses aller Eschen",
       subtitle = "Ausgeschlossen wurden insgesamt 1213 Eschen aus diversen Gründen",
       y = "n" ,
       x = "Zuwachs [mm]"
  )
plot_growth_hist
ggsave(
  plot = plot_growth_hist,
  filename = "EXPORT/IBF/figures/Zuwachsverteilung_Esche.pdf",
  units = "mm",
  width = 250,
  height = 150
)

## ETS DISTRIBUTION BY GROWTH CLASS  -------------------------------------------
# The file plot_growth_hist contains the x limits I chose to draw the 
# histogram, they can be exported using layer_data (very useful!)
# 
# The goal was to combine the histogram with the ratio of ets 
table_ets_growth <- data.frame(xmin = layer_data(plot_growth_hist, 1)['xmin'],
                               xmax = layer_data(plot_growth_hist, 1)['xmax'])
table_ets_growth$xmid <- (table_ets_growth$xmin + table_ets_growth$xmax) / 2

ets_tmp <- c()

for (i in 1:nrow(table_ets_growth)) {
  ets_tmp <-
    tmp_data_nv$ETS[tmp_data_nv$growth_mean >= table_ets_growth$xmin[i] &
                      tmp_data_nv$growth_mean <= table_ets_growth$xmax[i]]
  table_ets_growth$n_ash[i] <- length(ets_tmp)
  table_ets_growth$n_ets[i] <- sum(ets_tmp)
  table_ets_growth$ratio[i] <- sum(ets_tmp) / length(ets_tmp)
}

# The filters need to be displayed as a little legend

plot_growth_ets <- 
  ggplot(data = table_ets_growth, aes(x = xmin, y = ratio)) +
  geom_col(aes(y = n_ash)) +
  geom_point(aes(y = ratio * 300)) +
  scale_y_continuous(
    name = "n",
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~ . / 3,
                        name = "ETS Ratio [%]")) +
  labs(title = "Eschen nach Zuwachsklassen und ETS Anteil",
       subtitle = "",
       x = ("Zuwachs [mm]"))
plot_growth_ets

ggsave(
   plot = plot_growth_ets,
   filename = "EXPORT/IBF/figures/Zuwachsverteilung_ETS.pdf",
   units = "mm",
   width = 250,
   height = 150
)
## TIDY UP  --------------------------------------------------------------------
# rm(plot_growth_ets, plot_growth_hist, table_ets_growth, tmp_data_nv, ets_tmp, i)

## OUTPUT ----------------------------------------------------------------------
# 2 plots in the Export folder



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

