library(tidyverse)
library(magrittr)
datos <- readRDS("datosClustering.Rds")
datos %<>%
  select(BARRIO, GRAVEDAD_ACCIDENTE, CLASE_ACCIDENTE) %>%
  group_by(BARRIO) %>%
  summarise(
    ACCIDENTES_DIARIOS = n(),
    TASA_GRAVES = mean(GRAVEDAD_ACCIDENTE == "Con muertos"),
    MAYOR_PROBABILIDAD = mean(CLASE_ACCIDENTE %in% c("Choque",
                                                     "Atropello"))
  ) %>%
  ungroup()

zto <- function(x) {
  return((x - min(x))/(max(x) - min(x)))
}

library(factoextra)

datos[, 2] <- zto(datos[, 2])

km1 <- kmeans(datos[,c(-1,-4)], centers = 4, nstart = 25)
fviz_cluster(km1, data = datos[,c(-1,-4)])

km2 <- kmeans(datos[,c(-1,-3)], centers = 4, nstart = 25)
fviz_cluster(km2, data = datos[,c(-1, -3)])

km3 <- kmeans(datos[,c(-1,-4)], centers = 3, nstart = 25)
fviz_cluster(km3, data = datos[,c(-1,-4)])

km4 <- kmeans(datos[,c(-1,-3)], centers = 3, nstart = 25)
fviz_cluster(km4, data = datos[,c(-1,-3)])

#Se decide quedarse con km4, se usaron ACCIDENTES DIARIOS Y TASA GRAVES

datos %<>%
  mutate(Cluster = factor(km4$cluster)) %>%
  arrange(desc(ACCIDENTES_DIARIOS))

datos %>%
  grou