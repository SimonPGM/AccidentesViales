library(tidyverse)
datos <- readRDS("AccidentesMDE.Rds")
datos <- datos %>%
  select(BARRIO, GRAVEDAD_ACCIDENTE, CLASE_ACCIDENTE) %>%
  group_by(BARRIO) %>%
  summarise(ACCIDENTES_DIARIOS = n(), 
            TASA_GRAVES = mean(GRAVEDAD_ACCIDENTE == "Con muertos"),
            MAYOR_PROBABILIDAD = mean(CLASE_ACCIDENTE %in% c("Choque",
                                                             "Atropello"))) %>%
  ungroup()
