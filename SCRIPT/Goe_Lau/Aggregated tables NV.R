#============================ TITLE ===========================================#
# J.Osewold
# 24.06.22
# NEW
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------

## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/Goe_Lau/Load and Clean NV Data.R")
# plotnumbers_with_empty
# nv

## NOTES -----------------------------------------------------------------------


## PRODUCE AGGREGATED PLOTS TABLE  ---------------------------------------------
nv_plots <- data.frame("Plotnummer" = plotnumbers_with_empty)

for (i in 1:nrow(nv_plots)) {
  temp_plotnummer <- nv_plots$Plotnummer[i]
  temp_rows <- (nv$Plotnummer == temp_plotnummer)
  
  nv_plots$n_trees[i] <- sum(temp_rows, na.rm = T)
  nv_plots$n_species[i] <- length(unique(nv$Baumart[temp_rows]))
  nv_plots$n_ash[i] <- sum(nv$Baumart[temp_rows] == "GEs", na.rm = T)
  
  # The location can be taken from the plotnumber, each is unique
  if (temp_plotnummer %in% 1:315) {
    nv_plots$location[i] <- "Lau_Steinhorst"
  } else if (temp_plotnummer %in% 8701:8815) {
    nv_plots$location[i] <- "Goe_Ansitz"
  } else if (temp_plotnummer %in% 8900:9017) {
    nv_plots$location[i] <- "Goe_Polter"
  }
  
  # The empty plots are missing in the nv table, but instead of NA, max() etc
  # returns inf+ or NaN, therefore a check for emptiness had to be implemented
  if (length(nv$Hoehe[temp_rows]) != 0) {
    nv_plots$height_mean[i] <- mean(nv$Hoehe[temp_rows])
    nv_plots$height_median[i] <- median(nv$Hoehe[temp_rows])
    nv_plots$height_max[i] <- max(nv$Hoehe[temp_rows])
    nv_plots$height_min[i] <- min(nv$Hoehe[temp_rows])
  } else {
    nv_plots$height_mean[i] <- NA
    nv_plots$height_median[i] <- NA
    nv_plots$height_max[i] <- NA
    nv_plots$height_min[i] <- NA
  }
  
  # How many ashes do either have nekroses OR (|) have just died? In relation to 
  # the number of ashes on this plot
  temp_ets_new <-
    nv$ETS.abgestorben.frisch[temp_rows] != "" |
    nv$ETS.lebend[temp_rows] != ""
  temp_ets_total <-
    nv$ETS.abgestorben.frisch[temp_rows] != "" |
    nv$ETS.lebend[temp_rows] != "" |
    nv$ETS.abgestorben.alt[temp_rows] != ""
  
  nv_plots$n_ets_new[i] <- sum(temp_ets_new, na.rm = T)
  nv_plots$n_ets_old[i] <- sum(nv$ETS.abgestorben.alt[temp_rows] != "", na.rm = T)
  nv_plots$n_ets_total[i] <- sum(temp_ets_total, na.rm = T)
  
  # In most cases the first comment is related to the whole plot
  nv_plots$comment[i] <- (nv$Bemerkungen[temp_rows])[1]
}

# Now the information about logging trails from the empty plots is needed as 
# well
for (i in 1:nrow(nv_plots)) {
  temp_plotnummer <- nv_plots$Plotnummer[i]
  nv_plots$Rueckegasse <-
    unique(nv_with_empty$Rueckegasse[nv_with_empty$Plotnummer 
                                     == temp_plotnummer])
}

nv_plots$height_mean <- round(nv_plots$height_mean, 2) 

## PRODUCE AGGREGATED SPECIES TABLE  -------------------------------------------

nv_species <- data.frame("species" = unique(nv$Baumart))
for (i in 1:nrow(nv_species)) {
  temp_species <- nv_species$species[i]
  nv_species$n[i] <- length(nv$Baumart[nv$Baumart == temp_species])
  nv_species$n_plots[i] <-
    length(unique(nv$Plotnummer[nv$Baumart == temp_species]))
  nv_species$height_mean[i] <- mean(nv$Hoehe[nv$Baumart == temp_species])
  nv_species$height_median[i] <- median(nv$Hoehe[nv$Baumart == temp_species])
  nv_species$height_max[i] <- max(nv$Hoehe[nv$Baumart == temp_species])
  nv_species$height_min[i] <- min(nv$Hoehe[nv$Baumart == temp_species])
}
nv_species$height_mean <- round(nv_species$height_mean, 2)
nv_species <- nv_species[order(nv_species$n, decreasing = T), ]



## TIDY UP  --------------------------------------------------------------------
rm(i, plotnumbers_with_empty, temp_ets_new, temp_ets_total, temp_plotnummer,
   temp_rows, temp_species)

## OUTPUT ----------------------------------------------------------------------
# nv
# nv_plots
# nv_species
# nv_with_empty
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

