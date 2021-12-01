library(tidyverse)
library(magrittr)
datos <- readRDS("datosClustering.Rds")
datos <- datos %>%
  select(BARRIO, GRAVEDAD_ACCIDENTE, CLASE_ACCIDENTE) %>%
  group_by(BARRIO) %>%
  summarise(ACCIDENTES_DIARIOS = n(), 
            TASA_GRAVES = mean(GRAVEDAD_ACCIDENTE == "Con muertos"),
            MAYOR_PROBABILIDAD = mean(CLASE_ACCIDENTE %in% c("Choque",
                                                             "Atropello"))) %>%
  ungroup()

library(factoextra)

datos[, 2] <- scale(datos[, 2])

km1 <- kmeans(datos[,c(-1,-4)], centers = 4, nstart = 25)
fviz_cluster(km1, data = datos[,c(-1,-4)])

km2 <- kmeans(datos[,c(-1,-3)], centers = 4, nstart = 25)
fviz_cluster(km2, data = datos[,c(-1, -3)])



km3 <- kmeans(datos[,c(-1,-4)], centers = 3, nstart = 25)
fviz_cluster(km3, data = datos[,c(-1,-4)])

km4 <- kmeans(datos[,c(-1,-3)], centers = 3, nstart = 25)
fviz_cluster(km3, data = datos[,c(-1,-3)])
