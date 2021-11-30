datos <- readRDS("DBtoshow.Rds")
datos.mapa <- readRDS("Maptoshow.Rds")

generate.db <- function(date.lower, date.upper,
                         acctype = "Sin filtro", accgrav = "Sin filtro", db = datos) {
  dates <- sort(c(as.Date(date.lower), as.Date(date.upper)))
  if (acctype == "Sin filtro") {
    temp <- db %>%
      filter(between(FECHA_ACCIDENTE, dates[1], dates[2]))
    if (accgrav != "Sin filtro") {
      accgrav <- stringi::stri_trans_general(accgrav, 'Latin-ASCII')
      temp <- temp %>%
        filter(GRAVEDAD_ACCIDENTE == accgrav)
    }
    return(temp)
  } else {
    acctype <- stringi::stri_trans_general(acctype, 'Latin-ASCII')
    temp <- db %>%
      filter(between(FECHA_ACCIDENTE, dates[1], dates[2]),
             CLASE_ACCIDENTE == acctype)
    if (accgrav != "Sin filtro") {
      accgrav <- stringi::stri_trans_general(accgrav, 'Latin-ASCII')
      temp <- temp %>%
        filter(GRAVEDAD_ACCIDENTE == accgrav)
    }
    return(temp)
  }
}

generate.db.map <- function(date.lower, date.upper,
                            acctype = "Sin filtro", accgrav = "Sin filtro", db = datos.mapa) {
  temp <- generate.db(date.lower, date.upper, acctype, accgrav, db) %>%
    select(lng, lat) %>%
      mutate(lng = as.numeric(lng), lat = as.numeric(lat))
  m <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=temp$lng, lat=temp$lat, clusterOptions = markerClusterOptions()) 
  return(m)
}
