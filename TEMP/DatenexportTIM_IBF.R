
data_tree_scho <- subset (data_tree_scho, auf == 3)
data_tree_tim <-
  data.frame(subset(data_tree_huy, select = c(nr, art, d)),
             id_flaeche = rep(6, times = nrow(data_tree_huy)))
data_tree_tim <-
  rbind(data_tree_tim,
        data.frame(subset(data_tree_mol, select = c(nr, art, d)),
                   id_flaeche = rep(8, times = nrow(data_tree_mol))))
data_tree_tim <-
  rbind(data_tree_tim, 
        data.frame(subset(data_tree_scho, select = c(nr, art, d)),
                   id_flaeche = rep(9, times = nrow(data_tree_scho))
        ))


colnames(data_tree_tim) <- c("BaumNr", "baumart", "Durchmesser", "id_flaeche")
data_tree_tim$BaumNr <- gsub(" ", "", data_tree_tim$BaumNr)
data_tree_tim <- subset(data_tree_tim, !(baumart %in% c(0, 999, 555)))


write.csv(data_tree_tim, file = "TEMP/Export_Tim.csv")


