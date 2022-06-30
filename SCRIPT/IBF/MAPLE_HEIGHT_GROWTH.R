#============================ MAPLE_HEIGHT_GROWTH =============================#
# J.Osewold
# 16.05.22
# NEW BUT COPIED
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/IBF/LOAD_NV.R")
# data_nv

## NOTES -----------------------------------------------------------------------
# The code is more or less copied from the growth and height distribution of the 
# ashes.

## HEIGHT DISTRIBUTION MAPLE  ----------------------------------------------------
data_nv %>%
  filter(Baumart_kurz == "BAh") %>%
  filter(Hoehe < 500) %>% # -198
  count() # total 3725

plot_height_distr <- 
  data_nv %>%
  filter(Baumart_kurz == "BAh") %>%
  filter(Hoehe < 500) %>%
  ggplot(aes(x = Hoehe)) +
  geom_histogram(binwidth = 12.5, boundary = 0) +
  labs(
    y = "Anzahl [total = 3725]",
    title = "Verteilung aller Bergahorne nach Hoehenklassen",
    subtitle = "",
    x = ("Hoehe [mm]")
  ) +
  annotate(geom = "text", x = 200, y = 450, label = "> filter(Hoehe < 500)",
           size = 4)


plot_height_distr
ggsave(
  plot = plot_height_distr,
  filename = "EXPORT/IBF/figures/BAh_Hoehe.pdf",
  units = "mm",
  width = 250,
  height = 150
)

table_bah_height <- data.frame(xmin = layer_data(plot_height_distr, 1)['xmin'],
                               xmax = layer_data(plot_height_distr, 1)['xmax'])
table_bah_height$xmid <- (table_bah_height$xmin + table_bah_height$xmax) / 2



data_nv_tmp <-
  data_nv %>% 
  filter(Baumart_kurz == "BAh")

## GROWTH DISTRIBUTION BY HEIGHT CLASS  ----------------------------------------
data_nv_tmp$growth1 <- data_nv_tmp$Hoehe - data_nv_tmp$Hoehe.Vorjahr
data_nv_tmp$growth2 <- data_nv_tmp$Hoehe.Vorjahr - data_nv_tmp$Hoehe.Vorvorjahr
data_nv_tmp$growth_mean <-  (data_nv_tmp$growth1 + data_nv_tmp$growth2) / 2 
# Any NA in these calculations results in NA
# 
# The growth needs to filtered according to filters in NV_Growth_ETS.R

for (i in 1:nrow(table_bah_height)) {
  growth_tmp <-
    data_nv_tmp$growth_mean[data_nv_tmp$Hoehe >= table_bah_height$xmin[i] &
                              data_nv_tmp$Hoehe <= table_bah_height$xmax[i]]
  table_bah_height$growth_median[i] <- median(growth_tmp, na.rm = T)
  table_bah_height$growth_mean[i] <- mean(growth_tmp, na.rm = T)
  table_bah_height$n_bah[i] <- length(growth_tmp)
}


table_bah_height <- table_bah_height[-(1:2), ]
plot_height_growth_distr <- 
  ggplot(data = table_bah_height, aes(x = xmid, y = n_bah)) +
  geom_col(aes(y = n_bah)) +
  geom_point(aes(y = growth_mean * 5)) +
  scale_y_continuous(
    name = "Anzahl [total 3725]",
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~ . / 5,
                        name = "Zuwachs [mm]")) +
  labs(title = "Bergahorn nach Hoehenklassen und jeweiliger durchschnittlicher Zuwachs",
       subtitle = "",
       x = ("Hoehe [mm]"))+
  annotate(
    geom = "text",
    x = 200,
    y = 450,
    label = "> filter(Hoehe < 500) \n> filter(Hoehe > 18.75)",
    size = 4 ,
    hjust = 0
  )
 
  
plot_height_growth_distr
ggsave(
  plot = plot_height_growth_distr,
  filename = "EXPORT/IBF/figures/BAh_Hoehe_Zuwachs.pdf",
  units = "mm",
  width = 250,
  height = 150
)


## TIDY UP  --------------------------------------------------------------------
rm(
  data_nv_tmp,
  table_bah_height,
  i,
  growth_tmp,
  plot_height_distr,
  plot_height_growth_distr
)

## OUTPUT ----------------------------------------------------------------------
# figure/BAh_Hoehe_Zuwachs.pdf
# figures/BAh_Hoehe.pdf



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

