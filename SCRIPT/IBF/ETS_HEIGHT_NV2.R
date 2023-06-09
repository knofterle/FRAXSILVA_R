#============================ ETS HEIGHT NV ===================================#
# J.Osewold
# 22.04.22
# VERY Good
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------
library(ggplot2)
## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/IBF/LOAD_NV.R", encoding = "UTF-8")
# data_nv
# 
## NOTES -----------------------------------------------------------------------


## HEIGHT DISTRIBUTION ASH  ----------------------------------------------------
plot_height_distr <- 
  data_nv %>%
  filter(Baumart_kurz == "GEs") %>%
  filter(Hoehe < 500) %>%
  filter((!is.na(Hoehe.Vorvorjahr))  & eigentlich.aelter == F) %>% 
  ggplot(aes(x = Hoehe)) +
  geom_histogram() +
  labs(
    y = "n",
    title = "Verteilung aller Eschen nach Hoehenklassen",
    subtitle = "Eschen über 500 mm wurden ausgeschlossen",
    x = ("Hoehe [mm]")
  )
ggsave(
  plot = plot_height_distr,
  filename = "EXPORT/IBF/figures/Hoehenverteilung_Esche2_all.pdf",
  units = "mm",
  width = 250,
  height = 150
)
plot_height_distr

## ETS DISTRIBUTION BY HEIGHT CLASS  -------------------------------------------
# The file plot_height_distr contains the x limits ggplot chose to draw the 
# histogram, they can be exported using layer_data (very useful!)
# 
# The goal was to combine the histogram with the ratio of ets 
table_ets_height <- data.frame(xmin = layer_data(plot_height_distr, 1)['xmin'],
                               xmax = layer_data(plot_height_distr, 1)['xmax'])
table_ets_height$xmid <- (table_ets_height$xmin + table_ets_height$xmax) / 2

data_nv_tmp <-
  data_nv %>% 
  filter(Baumart_kurz == "GEs") %>% 
  filter((!is.na(Hoehe.Vorvorjahr))  & eigentlich.aelter == F)
ets_tmp <- c()

# Loop for ETS
for (i in 1:nrow(table_ets_height)) {
  ets_tmp <-
    data_nv_tmp$ETS[data_nv_tmp$Hoehe >= table_ets_height$xmin[i] &
                      data_nv_tmp$Hoehe <= table_ets_height$xmax[i]]
  table_ets_height$n_ash[i] <- length(ets_tmp)
  table_ets_height$n_ets[i] <- sum(ets_tmp)
  table_ets_height$ratio[i] <- sum(ets_tmp) / length(ets_tmp)
}

## GROWTH DISTRIBUTION BY HEIGHT CLASS  ----------------------------------------
data_nv_tmp$growth1 <- data_nv_tmp$Hoehe - data_nv_tmp$Hoehe.Vorjahr
data_nv_tmp$growth2 <- data_nv_tmp$Hoehe.Vorjahr - data_nv_tmp$Hoehe.Vorvorjahr
data_nv_tmp$growth_mean <-  (data_nv_tmp$growth1 + data_nv_tmp$growth2) / 2 
# Any NA in these calculations results in NA
# 
# The growth needs to filtered according to filters in NV_Growth_ETS.R

for (i in 1:nrow(table_ets_height)) {
  growth_tmp <-
    data_nv_tmp$growth_mean[data_nv_tmp$Hoehe >= table_ets_height$xmin[i] &
                              data_nv_tmp$Hoehe <= table_ets_height$xmax[i]]
  table_ets_height$growth_median[i] <- median(growth_tmp, na.rm = T)
  table_ets_height$growth_mean[i] <- mean(growth_tmp, na.rm = T)
}



##  BOTH PLOTS  ----------------------------------------------------------------
# ETS
tmp <- nrow(data_nv_tmp)
tmp <- paste0("Anzahl [total = ", tmp, "]")
plot_height_ets_distr <- 
  ggplot(data = table_ets_height, aes(x = xmid, y = ratio)) +
  geom_col(aes(y = n_ash)) +
  geom_point(aes(y = ratio * 300)) +
  scale_y_continuous(
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~ . / 3,
                        name = "ETS Ratio [%]")) +
  labs(x = "Hoehe [mm]", y = tmp) +
  annotate(
    geom = "text",
    x = 200,
    y = 200,
    label = "> filter (Hoehe < 500) \n> filter (alter >= 2j)",
    size = 4 ,
    hjust = 0
  )
plot_height_ets_distr
ggsave(
  plot = plot_height_ets_distr,
  filename = "EXPORT/IBF/figures/Hoehenverteilung_ETS2_all.pdf",
  units = "mm",
  width = 250,
  height = 150
)

# GROWTH
table_ets_height <- table_ets_height[-1, ]
plot_height_growth_distr <- 
  ggplot(data = table_ets_height, aes(x = xmid, y = n_ash)) +
  geom_col(aes(y = n_ash)) +
  geom_point(aes(y = growth_mean * 3)) +
  scale_y_continuous(
    # Add a second axis and specify its features
    sec.axis = sec_axis(trans = ~ . / 3,
                        name = "Zuwachs [mm]")) +
  labs(x = "Hoehe [mm]", y = tmp) +
  annotate(
    geom = "text",
    x = 200,
    y = 200,
    label = "> filter (Hoehe < 500) \n> filter (alter >= 2j)",
    size = 4 ,
    hjust = 0
  )
plot_height_growth_distr
ggsave(
  plot = plot_height_growth_distr,
  filename = "EXPORT/IBF/figures/Hoehenverteilung_Zuwachs2_all.pdf",
  units = "mm",
  width = 250,
  height = 150
)

## TIDY UP  --------------------------------------------------------------------
rm(table_ets_height,
   ets_tmp,
   plot_height_distr,
   plot_height_ets_distr,
   data_nv_tmp,
   plot_height_growth_distr,
   growth_tmp,
   i
   )

## OUTPUT ----------------------------------------------------------------------
# Three plots in the EXPORT folder:
# 
# figures/Hoehenverteilung_Esche2.pdf
# 
# Hoehenverteilung_Zuwachs2.pdf
# Hoehenverteilung_ETS2.pdf



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

