#========================== NV_LAST DIGIT =====================================#
# J.Osewold
# 22.04.22
# ONLY MINOR CHANGES NECESSARY
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------
library(ggplot2)
## REQUIRES --------------------------------------------------------------------
source(file = "SCRIPT/IBF/LOAD_NV.R")
# data_nv

## NOTES -----------------------------------------------------------------------


## ANALYSIS OF THE LAST DIGIT OF ALL HEIGHTS MEASURED --------------------------

heights <- c(data_nv$Hoehe, data_nv$Hoehe.Vorjahr, data_nv$Hoehe.Vorvorjahr)

digits <- c()
for (i in 1:length(heights)) {
  tmp <- heights[i]
  digit <- substr(tmp, start = nchar(tmp), stop = nchar(tmp))
  digits <- c(digits, digit)
}

digits <- data.frame(digit = as.numeric(digits))

plot_last_digit <-
  ggplot(data = digits, aes(x = digit)) +
  geom_bar(width = 0.5) +
  scale_x_continuous(breaks = c(0:9)) +
  labs(
    title = "Histogram der letzten Ziffer aller NV Messungen",
    subtitle = "Baeumchen = 9,813 Hoehen = 24,733",
    x = "Letzte Ziffer",
    y = "n"
  )
ggsave(plot = plot_last_digit, filename = "EXPORT/IBF/figures/last_digit.pdf", 
       units = "mm", width = 250, height = 150) 

ggsave(plot = plot_last_digit, filename = "EXPORT/IBF/figures/last_digit.png", 
       units = "mm", width = 250, height = 150)


## TIDY UP  --------------------------------------------------------------------
rm(heights, digits, tmp, i, digit, plot_last_digit)

## OUTPUT ----------------------------------------------------------------------
# two plots in the EXPORT folder



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

