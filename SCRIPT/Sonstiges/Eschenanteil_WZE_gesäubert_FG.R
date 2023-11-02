#library
library(dplyr)
library(tidyr)
library(ggplot2)

#data (neu von UweK 23.10.) Eschen in WZE 
df_Eschen_WZE <- read.csv("DATA/Sonstiges/EschenanteilWZE.csv")
colnames(df_Eschen_WZE) <- c("Jahr","nBäume","nEschen","Anteil_Esche")
 df_Eschen_WZE <- df_Eschen_WZE %>%
   filter(Jahr > 1990)

dist <- 2 # inklusive vorher und nachher also bei 3 ein mean(1:7)
start <- 0
end <- 0

for (i in 1:nrow(df_Eschen_WZE)) {
  start <- i - dist
  end <- i + dist
  if (start < 0) {start <- 0}
  if (end > nrow(df_Eschen_WZE)) {end <- nrow(df_Eschen_WZE)}
  df_Eschen_WZE$avg[i] <- 
    sum(df_Eschen_WZE$nEschen[start:end]) /
    sum(df_Eschen_WZE$nBäume[start:end])
}

df_Eschen_WZE$avg <- df_Eschen_WZE$avg * 100

plot <- 
  ggplot(data = df_Eschen_WZE, aes(x=Jahr, y=Anteil_Esche)) +
  geom_line(aes(x = Jahr, y = avg), color = "#00979a", size = 2) +
  geom_point(size = 2) +
  theme_classic() +
  theme(text=element_text(family= "sans")) +
  labs(x = "", y = "Anteil Esche [% Individuen]") +
  scale_x_continuous(
    breaks = seq(1990, 2023, by = 5),
    labels = seq(1990, 2023, by = 5)
  ) +
  scale_y_continuous(
    breaks = seq(1.3, 2.2, by = .1)
  ) 
plot
ggsave(
  plot = plot,
  filename = "EXPORT/Sonstige/WZE_NWFVA.png",
  width = 150,
  height = 100,
  units = "mm",
  dpi = 600
)
ggsave(
  plot = plot,
  filename = "EXPORT/Sonstige/WZE_NWFVA.pdf",
  width = 150,
  height = 100,
  units = "mm"
)

##### Dieser Teil ist jetzt irrelevant und wurde von Felix und Sebastian 
##### übernommen

  
# Zu den bundesweiten Daten fehlen die Anzahl von Flächen die im jeweiligen Bezugsjahr in den WZEs erhoben wurden
# Daher ist hier nur eine Schätzung mit den Anteil von Eschen auf WZE Flächen mit Esche möglich
Eschen_WZE_Bund <- read.csv("DATA/Sonstiges/WZE_Bund_Eschen_Jahr.csv")

Eschen_WZE_Bund$Anteil <- round(Eschen_WZE_Bund$nBaum/24,2)
Eschen_WZE_Bund$Anteil <- Eschen_WZE_Bund$Anteil*100


Eschen_WZE_Bund <- Eschen_WZE_Bund %>%
  group_by(jahr) %>%
  summarise(Anteil_Jahr = mean(Anteil)) %>%
  filter(jahr > 1990)

ggplot(data= Eschen_WZE_Bund, aes(x=jahr, y=Anteil_Jahr)) + 
  geom_point() + 
  geom_smooth(method = "loess", span =1.5 , se =FALSE) +
  labs(title = "Anteil Eschen auf WZE Flächen mit Esche (bundesweit)", x= "Jahr", y= "Durschnittlicher Anteil Eschen [%]")
