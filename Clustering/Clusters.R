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

datos %<>%
  mutate(choqueszto = zto(ACCIDENTES_DIARIOS))

idx <- combn(3:5, 2)

lista.clusters <- list()
for (i in 1:ncol(idx)) {
  lista.clusters[[i]] <- kmeans(datos[, idx[, i]], centers = 3, nstart = 25)
}

fviz_cluster(lista.clusters[[1]], data = datos[,idx[,1]])
fviz_cluster(lista.clusters[[2]], data = datos[,idx[,2]])
fviz_cluster(lista.clusters[[3]], data = datos[,idx[,3]])

datos %<>%
  mutate(clusterthree = lista.clusters[[3]]$cluster)
datos %<>%
  mutate(clusterone = lista.clusters[[1]]$cluster)
datos %<>%
  mutate(clustertwo = lista.clusters[[2]]$cluster)



datos %>%
  arrange(desc(ACCIDENTES_DIARIOS)) %>%
  select(BARRIO, ACCIDENTES_DIARIOS, clustertwo) %>%
  View()

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

saveRDS(datos, "Datosclustersfinal.Rds")
