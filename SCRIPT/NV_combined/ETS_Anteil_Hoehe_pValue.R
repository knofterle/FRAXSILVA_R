#================== ETS_Anteil_Hoehe_pValue ===================================#
# J.Osewold
# DATE
# STATUS
#==============================================================================#

## REQUIRES --------------------------------------------------------------------
# This runs all previously necessary scripts
# source()
# 
# The data and variables from the previous scripts can be listed here

## LIBRARYS -------------------------------------------------------------------
require(mgcv)

## NOTES -----------------------------------------------------------------------


## STEP 1  ---------------------------------------------------------------------

ETS_Anteil_Hoehe_pValue

table(tmp$ETS)
sum(is.na(tmp$ETS))
hist(tmp$Hoehe)
str(tmp)
table(nv_all2$Flaeche)
sum(is.na(tmp$Flaeche))

tmp2 <- tmp 
tmp$Flaeche <- as.factor(tmp$Flaeche)

m <- gam(ETS ~ s(Hoehe) + s(Flaeche, bs = 're'), data = tmp, family = binomial)
summary(m)
plot(m)

m <- gam(ETS ~ s(Hoehe), data = tmp, family = binomial)
summary(m)
plot(m)


n <- gam(ETS ~ Hoehe, data = tmp, family = binomial)
summary(n)
plot(n)

tmp2 <- tmp
tmp2$Hoehe2 <- tmp2$Hoehe^2

n <- gam(ETS ~ Hoehe2, data = tmp2, family = binomial)
summary(n)
plot(n)


m <- gam(ETS ~ s(Hoehe) + s(FlaechenID, bs = 're'), family = binomial, data = df)

str(df)

nv_all2 <- nv_all

nv_all <- nv_all2 %>% filter(Flaeche == "Greifswald")
nv_all <- nv_all2 %>% filter(Flaeche == "Leutzsch")
nv_all <- nv_all2 %>% filter(Flaeche == "Mollenfelde")
nv_all <- nv_all2 %>% filter(Flaeche == "Plattenwald")
nv_all <- nv_all2 %>% filter(Flaeche == "Schotten")
nv_all <- nv_all2 %>% filter(Flaeche == "Stegelitz")
nv_all <- nv_all2 %>% filter(Flaeche == "Weisweil")

nv_all <- nv_all2

tmp3 <- tmp2 %>% filter(!is.na(ETS))
ggplot(data = tmp3) +
	geom_violin(aes(x = Hoehe, y = Flaeche), scale = "count") +
	facet_grid(cols = vars(ETS))


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

