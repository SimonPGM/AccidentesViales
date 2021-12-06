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

#Mirando los clusters

datos <- readRDS("Datosclustersfinal.Rds")

datos %>%
  arrange(desc(ACCIDENTES_DIARIOS)) %>%
  select(BARRIO, ACCIDENTES_DIARIOS, clustertwo) %>%
  View()

#Exportando los datos
datos %>%
  mutate(Cluster = factor(if_else(clustertwo == 1, "Riesgo alto", if_else(clustertwo == 2, "Riesgo medio",
                                                                   "Riesgo bajo")))) %>%
  select(BARRIO, ACCIDENTES_DIARIOS, Cluster) %>%
  saveRDS("BarrioCluster.Rds")
