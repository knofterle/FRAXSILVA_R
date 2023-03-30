#============================ BASIC OVERVIEW NV ===============================#
# J.Osewold
# 20.04.22
# NEW
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------
require(dplyr)
## REQUIRES --------------------------------------------------------------------
source("SCRIPT/IBF/SAVE_PLOTPOSITION.R", encoding = "UTF-8")
# data_nv
# plots_pos


## NOTES -----------------------------------------------------------------------
# This script produces aggregated tables for the nv data. Some steps will be 
# similar to the data of Lau/Goe, but I expect the data to be too different to
# share the code. And I was right.
# 
# Special caution has to be spend when dealing with the NAs.

## PRODUCE AGGREGATED PLOTS TABLE  ---------------------------------------------
data_nv_plots <- subset(data_nv, select = c(ID_plot, Plotnummer, Flaeche, Rand, 
                                            Zaun, Rueckegasse, keine.baeume, 
                                            Flaeche.bedeckt, x, y))
data_nv_plots <- unique(data_nv_plots)


for (i in 1:nrow(data_nv_plots)) {
  temp_ID_plot <- data_nv_plots$ID_plot[i]
  temp_rows <- (data_nv$ID_plot == temp_ID_plot)
  
  data_nv_plots$n_trees[i] <- sum(!is.na(data_nv$Baumart_kurz[temp_rows]))
  data_nv_plots$n_species[i] <-
    sum(!is.na(unique(data_nv$Baumart_kurz[temp_rows], na.rm = T)))
  data_nv_plots$n_ash[i] <-
    sum(data_nv$Baumart_lang[temp_rows] == "Fraxinus excelsior", na.rm = T)
  
  data_nv_plots$height_mean[i] <-
    round(mean(data_nv$Hoehe[temp_rows], na.rm = T), digits = 1)
  data_nv_plots$height_median[i] <- median(data_nv$Hoehe[temp_rows], na.rm = T)
  data_nv_plots$height_max[i] <- max(data_nv$Hoehe[temp_rows], na.rm = T)
  data_nv_plots$height_min[i] <- min(data_nv$Hoehe[temp_rows], na.rm = T)
  
  
  # How many ashes do either have nekroses OR (|) have just died? 
  temp_ets_new <-
    !is.na(data_nv$ETS.abgestorben.frisch[temp_rows]) |
    !is.na(data_nv$ETS.lebend[temp_rows])
  temp_ets_total <-
    !is.na(data_nv$ETS.abgestorben.frisch[temp_rows]) |
    !is.na(data_nv$ETS.lebend[temp_rows]) |
    !is.na(data_nv$ETS.abgestorben.alt[temp_rows])
  
  data_nv_plots$n_ets_new[i] <- sum(temp_ets_new, na.rm = T)
  data_nv_plots$n_ets_old[i] <-
    sum(!is.na(data_nv$ETS.abgestorben.alt[temp_rows]), na.rm = T)
  data_nv_plots$n_ets_total[i] <- sum(temp_ets_total, na.rm = T)
}
  
## PRODUCE AGGREGATED SPECIES TABLE  -------------------------------------------
data_nv_temp <- data_nv[!is.na(data_nv$Baumart_kurz),]
data_nv_species <-
  unique(subset(data_nv_temp, select = c(Baumart_kurz, Baumart_lang)))

for (i in 1:nrow(data_nv_species)) {
  temp_species <- data_nv_species$Baumart_kurz[i]
  data_nv_species$n[i] <-
    length(data_nv_temp$Baumart_kurz[data_nv_temp$Baumart_kurz == temp_species])
  
  data_nv_species$n_plots[i] <-
    length(unique(
      data_nv_temp$ID_plot[data_nv_temp$Baumart_kurz == temp_species]))
  
  data_nv_species$height_mean[i] <-
    round(
      mean(data_nv_temp$Hoehe[data_nv_temp$Baumart_kurz == temp_species], 
           na.rm = T),
      digits = 0)
  
  data_nv_species$height_median[i] <-
    median(data_nv_temp$Hoehe[data_nv_temp$Baumart_kurz == temp_species],
           na.rm = T)
  data_nv_species$height_max[i] <-
    max(data_nv_temp$Hoehe[data_nv_temp$Baumart_kurz == temp_species],
        na.rm = T)
  data_nv_species$height_min[i] <-
    min(data_nv_temp$Hoehe[data_nv_temp$Baumart_kurz == temp_species],
        na.rm = T)
}

## PRODUCE AGGREGATED SAMPLE AREA TABLE ----------------------------------------
data_nv_area <- data.frame(Flaeche = unique(data_nv_temp$Flaeche))

for (i in 1:nrow(data_nv_area)) {
  temp_Flaeche <- data_nv_area$Flaeche[i]
  temp_rows <- (data_nv_temp$Flaeche == temp_Flaeche)
  
  data_nv_area$n_trees[i] <- sum(!is.na(data_nv_temp$Baumart_kurz[temp_rows]))
  
  data_nv_area$n_species[i] <-
    sum(!is.na(unique(data_nv_temp$Baumart_kurz[temp_rows], na.rm = T)))
  
  data_nv_area$n_ash[i] <-
    sum(data_nv_temp$Baumart_lang[temp_rows] == "Fraxinus excelsior", na.rm = T)
  
  data_nv_area$n_plot_total[i] <-
    length(data_nv_plots$ID_plot[data_nv_plots$Flaeche == temp_Flaeche])
  
  data_nv_area$n_plot_rand[i] <-
    sum(data_nv_plots$Rand[data_nv_plots$Flaeche == temp_Flaeche])
  
  data_nv_area$n_plot_zaun[i] <-
    sum(data_nv_plots$Zaun[data_nv_plots$Flaeche == temp_Flaeche])
  
  data_nv_area$n_plot_keinebaeume[i] <-
    sum(data_nv_plots$keine.baeume[data_nv_plots$Flaeche == temp_Flaeche])
  
  data_nv_area$n_plot_baeume[i] <-
    data_nv_area$n_plot_total[i] -
    data_nv_area$n_plot_rand[i] -
    data_nv_area$n_plot_zaun[i] -
    data_nv_area$n_plot_keinebaeume[i]
  
  data_nv_area$baume_pro_plot[i] <- 
    round(data_nv_area$n_trees[i] / data_nv_area$n_plot_baeume[i], digits = 1)
  
  # How many ashes do either have necroses OR (|) have just died?
  temp_ets_new <-
    !is.na(data_nv_temp$ETS.abgestorben.frisch[temp_rows]) |
    !is.na(data_nv_temp$ETS.lebend[temp_rows])
  temp_ets_total <-
    !is.na(data_nv_temp$ETS.abgestorben.frisch[temp_rows]) |
    !is.na(data_nv_temp$ETS.lebend[temp_rows]) |
    !is.na(data_nv_temp$ETS.abgestorben.alt[temp_rows])
  
  data_nv_area$n_ets_new[i] <- sum(temp_ets_new, na.rm = T)
  data_nv_area$n_ets_old[i] <-
    sum(!is.na(data_nv$ETS.abgestorben.alt[temp_rows]), na.rm = T)
  data_nv_area$n_ets_total[i] <- sum(temp_ets_total, na.rm = T)
  data_nv_area$median_height[i] <- 
    median(data_nv$Hoehe[data_nv$Flaeche == temp_Flaeche], na.rm = T)
}

## ADD SUM ROW  ----------------------------------------------------------------
# I do this at the end, because the plot table was used as a index for the 
# following loops and a SUM row would disturb this

### plot data ------------------------------------------------------------------
colnames(data_nv_plots)
data_nv_plots <- 
  rbind(data_nv_plots, 
        c(1,
          nrow(data_nv_plots),
          length(unique(data_nv_plots$Flaeche)),
          sum(data_nv_plots$Rand),
          sum(data_nv_plots$Zaun),
          sum(data_nv_plots$Rueckegasse, na.rm = T),
          sum(data_nv_plots$keine.baeume),
          sum(data_nv_plots$Flaeche.bedeckt),
          NA,
          NA,
          sum(data_nv_plots$n_trees),
          length(unique(data_nv$Baumart_kurz[!is.na(data_nv$Baumart_kurz)])) -1,
          # -1 for the species "unkown"
          sum(data_nv_plots$n_ash),
          mean(data_nv$Hoehe, na.rm = T),
          median(data_nv$Hoehe, na.rm = T),
          max(data_nv$Hoehe, na.rm = T),
          min(data_nv$Hoehe, na.rm = T),
          sum(data_nv_plots$n_ets_new),
          sum(data_nv_plots$n_ets_old),
          sum(data_nv_plots$n_ets_total)
          )
  )
data_nv_plots$ID_plot[nrow(data_nv_plots)] <- "SUMME"

### species data ---------------------------------------------------------------
colnames(data_nv_species)
data_nv_species <-
  rbind(data_nv_species,
        c(
          1,
          nrow(data_nv_species),
          sum(data_nv_species$n, na.rm = T),
          length(unique(data_nv$Plotnummer[!is.na(data_nv$Baumart_kurz)])),
          mean(data_nv$Hoehe, na.rm = T),
          median(data_nv$Hoehe, na.rm = T),
          max(data_nv$Hoehe, na.rm = T),
          min(data_nv$Hoehe, na.rm = T)
        )
  )
        
data_nv_species$Baumart_kurz[nrow(data_nv_species)] <- "SUMME" 

### area data ------------------------------------------------------------------
colnames(data_nv_area)
data_nv_area <- 
  rbind(data_nv_area,
        c(
          1,
          sum(data_nv_area$n_trees),
          length(unique(data_nv$Baumart_lang[!is.na(data_nv$Baumart_kurz)])) -1,
          # -1 for the species "unkown"
          sum(data_nv_area$n_ash, na.rm = T),
          sum(data_nv_area$n_plot_total, na.rm = T),
          sum(data_nv_area$n_plot_rand, na.rm = T),
          sum(data_nv_area$n_plot_zaun, na.rm = T),
          sum(data_nv_area$n_plot_keinebaeume, na.rm = T),
          sum(data_nv_area$n_plot_baeume, na.rm = T),
          mean(data_nv_area$baume_pro_plot, na.rm = T),
          sum(data_nv_area$n_ets_new, na.rm = T),
          sum(data_nv_area$n_ets_old, na.rm = T),
          sum(data_nv_area$n_ets_total, na.rm = T),
          median(data_nv$Hoehe, na.rm = T)
        )
  )
data_nv_area$Flaeche[nrow(data_nv_area)] <- "SUMME"

## EXPORT CSV  -----------------------------------------------------------------
write.csv(
  data_nv_plots,
  file = "EXPORT/IBF/tables/data_nv_plots.csv",
  fileEncoding = "UTF-8",
  row.names = F
)
write.csv(
  data_nv_species,
  file = "EXPORT/IBF/tables/data_nv_species.csv",
  fileEncoding = "UTF-8",
  row.names = F
)
write.csv(
  data_nv_area,
  file = "EXPORT/IBF/tables/data_nv_area.csv",
  fileEncoding = "UTF-8",
  row.names = F
)


## TIDY UP  --------------------------------------------------------------------
rm(tmp, plotnumber, flaeche, i, temp_ets_new, temp_ets_total, temp_ID_plot,
   temp_rows, temp_species, data_nv_temp, temp_Flaeche)

## OUTPUT ----------------------------------------------------------------------
# data_nv_plots
# data_nv_species
# data_nv_area
# 
# and all three as csv

# from the earlier scripts:
# data_nv
# data_tree_Flächenname
# data_tree_ibf
# plots_pos 


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

