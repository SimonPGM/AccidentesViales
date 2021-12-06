library(tidyverse)
library(rgdal)
library(leaflet)
datos.med <- readOGR("./Rendering/BDM/Barrio_Vereda.shp", layer = "Barrio_Vereda")
datos.mapa <- readRDS("BarrioCluster.Rds")

located <- unlist(purrr::map(datos.mapa$BARRIO, function(x) x %in% noms))
datos.mapa$BARRIO[!located] <- c("El Velódromo", "López de  Mesa", "Nueva Villa del Aburrá", 
                                 "Nueva Villa de La Iguaná", "San José de La Montaña")
idx <- unlist(purrr::map(noms, function(x) x %in% datos.mapa$BARRIO))
datos.med@data <- datos.med@data %>%
  filter(NOMBRE %in% datos.mapa$BARRIO) %>%
  distinct(NOMBRE, .keep_all = T)

datos.mapa <- datos.mapa %>%
  rename(NOMBRE = BARRIO)

datos.med@data <- inner_join(datos.med@data, datos.mapa, by = "NOMBRE")

datos.med@data <- datos.med@data %>%
  mutate(Color = if_else(Cluster == "Riesgo alto", "#f00",
                         if_else(Cluster == "Riesgo medio", "#ff0", "#0f0")),
         Accidentes = paste("Total de accidentes = ", ACCIDENTES_DIARIOS, sep = ""))

mapa <- leaflet(datos.med) %>%
        addTiles() %>%
          addPolygons(popup = paste(datos.med@data$NOMBRE, datos.med@data$Cluster,
                                    datos.med@data$Accidentes, sep = "</br>"),
              color = datos.med@data$Color) %>%
        addTiles()

saveRDS(mapa, "mapa.Rds")
