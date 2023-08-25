#============================ TREES PER PLOT ==================================#
# J.Osewold
# 29.06.22
# SOON TO BE CLEANED
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
source("SCRIPT/Goe_Lau/Aggregated tables NV.R")
source("SCRIPT/Goe_Lau/Generate Map ANSITZ.R")
source("SCRIPT/Goe_Lau/Generate Map POLTER.R")


## LIBRARYS --------------------------------------------------------------------
library(dplyr)

## NOTES -----------------------------------------------------------------------


## HOW MANY TREES PER PLOT  ----------------------------------------------------
# This is super messy and I only want to keep it until I tidied up the whole 
# mess with ansitz polter und lau
# 
# Wenn ich das richtig verstehe ist das hier wirklich nicht mehr aktuell, es 
# diente unter anderem dazu die Tabellen zu produzieren die es fürs Finden der
# markierten Eschen 2022 brauchte. 
# Und ein paar längst veraltete Analysen.



nv_plots_coord <- rbind(nvplots_ansitz, nvplots_polter)
tmp <- nv_plots
nv_plots <-
  merge(
    x = nv_plots,
    y = nv_plots_coord,
    by.x = "Plotnummer",
    by.y = "nr",
    all.x = T,
    all.y = T
  )
nrow(nv_plots)
anti_join(nv_plots, tmp, by = "Plotnummer")

nv_plots$ash_ratio <- nv_plots$n_ash / nv_plots$n_trees 
nv_plots$ash_ratio

tmp <- nv_plots %>% 
  filter(location == "Goe_Polter")
ggplot(data = tmp) +
  geom_point(aes(x = x, y = y, alpha = n_trees), pch = 15, size = 6)
ggplot(data = tmp) +
  geom_point(aes(x = x, y = y, alpha = ash_ratio), pch = 15, size = 6)


tmp <- nv_plots %>% 
  filter(location == "Goe_Ansitz")
ggplot(data = tmp) +
  geom_point(aes(x = x, y = y, alpha = n_trees), pch = 15, size = 6)
ggplot(data = tmp) +
  geom_point(aes(x = x, y = y, alpha = ash_ratio), pch = 15, size = 6)


tmp <- nv_plots %>% 
  arrange(ash_ratio) %>% 
  select(location, Plotnummer, n_trees, ash_ratio, n_ash)
write.csv(tmp, file = "EXPORT/Goe_Lau/tables/Auswahlhilfe_halbe_Plots.csv")

## TIDY UP  --------------------------------------------------------------------
rm(nvplots_ansitz, nvplots_polter, nv_plots_coord, tmp)

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

