#============================ ETS_GROWTH_HEIGHT ===============================#
# J.Osewold
# 23.05.22
# LOOKS GOOD
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------
library(ggplot2)

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/IBF/LOAD_NV.R")
# data_nv
# and data_nv$ETS

## NOTES -----------------------------------------------------------------------
# This Script should compare the growth of ashes compared between resistant and
# vulnerable ones. I try to compare them by height class to get rid of the
# masking from more growth for higher trees.

## DATA FILTER  ----------------------------------------------------------------
tmp <- 
  data_nv %>% 
  filter(Baumart_kurz == "GEs") %>% 
  filter(Hoehe <= 500) %>% 
  filter((!is.na(Hoehe.Vorvorjahr))  & eigentlich.aelter == F) %>% 
  mutate(growth1 = Hoehe - Hoehe.Vorjahr) %>% 
  mutate(growth2 = Hoehe.Vorjahr - Hoehe.Vorvorjahr) %>% 
  mutate(growth_mean = (growth1 + growth2) / 2)  %>% 
  filter(growth1 >= 0) %>% 
  filter(growth2 >= 0) 

# start 2908
# after filter 1764

## SUMMARIZE DATA BY HEIGHT GROUPS  --------------------------------------------
steps <- seq(from = 0, to = 500, by = 12.5)
table_ets_hist <- 
  tmp %>% 
  filter(ETS == T) %>% 
  mutate(hist_step = cut(
    x = Hoehe,
    breaks = steps,
    labels = steps[1:40] + 6.25
  )) %>% 
  group_by(hist_step, .drop = F) %>% 
  summarise(growth = mean(growth_mean, na.rm = T), n = n()) %>% 
  mutate(ETS = T) 

table_hist <-
  tmp %>% 
  filter(ETS == F) %>% 
  mutate(hist_step = cut(
    x = Hoehe,
    breaks = steps,
    labels = steps[1:40] + 6.25
  )) %>% 
  group_by(hist_step, .drop = F) %>% 
  summarise(growth = mean(growth_mean, na.rm = T), n= n()) %>% 
  mutate(ETS = F) 

table_both_hist <- rbind(table_hist, table_ets_hist)
table_both_hist$hist_step <- as.numeric(as.character(table_both_hist$hist_step))

## GGPLOT  ---------------------------------------------------------------------
plot <-
  ggplot(table_both_hist) +
  geom_col(aes(
    x = hist_step,
    y = n,
    fill = ETS
  )) +
  geom_point(aes(x = hist_step, y = growth, color = ETS)) +
  scale_y_continuous(name = "Anzahl [total = 1764]",
                     sec.axis =  sec_axis(trans = ~ .,
                                          name = "Zuwachs [mm pro Jahr]")) +
  scale_x_continuous(
    name = "Hoehenklassen [mm]",
    breaks = seq(0, 500, 50),
    labels = seq(0, 500, 50)
  ) +
  scale_fill_manual(name = "Anzahl",
                    values = c("gray60", "red4"),
                    labels = c("Ohne ETS", "Mit ETS")) +
  scale_color_manual(name = "Zuwachs", 
                     values = c("black", "red3"),
                     labels = c("Ohne ETS", "Mit ETS")) +
  annotate(
    geom = "text",
    x = 200,
    y = 150,
    hjust = 0,
    label = "> filter (Hoehe < 500)\n> filter (alter > 2j)\n> filter (zuwachs >= 0)",
    size = 4
  )
ggsave(plot = plot, 
       filename = "EXPORT/IBF/figures/Hoehenverteilung_Zuwachs_ETS.pdf",
       units = "mm", width = 300, height = 150)
plot
       



## TIDY UP  --------------------------------------------------------------------
rm(tmp, plot, steps, table_both_hist, table_ets_hist, table_hist)

## OUTPUT ----------------------------------------------------------------------
# figure/Hoehenverteilung_Zuwachs_ETS.pdf
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

