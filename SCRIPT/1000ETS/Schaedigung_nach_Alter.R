#============================ TITLE ===========================================#
# J.Osewold
# DATE
# STATUS
#==============================================================================#

## LIBRARYS --------------------------------------------------------------------

## REQUIRES --------------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here

## NOTES -----------------------------------------------------------------------


## STEP 1  ---------------------------------------------------------------------



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




### 5. Schaedigung nach Alter ------------------------------------------------

EtsStufenVector <- unname(unlist(EtsStufen))
EtsStufenVector [EtsStufenVector == 8] <- NA 
EtsStufenVector [EtsStufenVector == 9] <- NA 

AlterVector <- c(data$alt_ets13, data$alt_ets14, data$alt_ets15, data$alt_ets16, 
                 data$alt_ets16+1,data$alt_ets16+2, data$alt_ets16+3, 
                 data$alt_ets16+4)
length(EtsStufenVector) == length(AlterVector)

pdf(file = "tables_and_figures/ETS-Stufe zu Alter_jitter.pdf",
    width=10, height=5, paper='special')

par(mfrow=c(1,1), mar = c(5,5,3,2))
plot(jitter(AlterVector, factor = 0), jitter(EtsStufenVector, factor = 2), 
     ylab = "ETS Stufe gestreut", xlab = "Alter", pch = 16, col="#00000011", 
     cex = 2)
dev.off()

for (i in c(1:5)) {
  assign(paste0("EtsStufe", i), AlterVector [EtsStufenVector == i] )
}

pdf(file = "tables_and_figures/ETS-Stufe zu Alter_boxplot.pdf",
    width=10, height=5, paper='special')
boxplot(EtsStufe1,EtsStufe2,EtsStufe3,EtsStufe4,EtsStufe5, ylab ="Alter",
        xlab = "ETS-Stufe", names = c("1":"5"), 
        main = "Verteilung der ETS-Stufen zum Alter")
dev.off()
#  Hierzu muss gesagt werden, dass jeder Baum mehrfach vorkommt obwohl er einfach
# nur mehrfach gemessen wurde. Ein NO GO aber fuer die reine Visualisierung hier 
# mal benutzt.
par(opar)
rm(AlterVector, EtsStufe1, EtsStufe2, EtsStufe3, EtsStufe4, EtsStufe5, 
   EtsStufenVector, i)