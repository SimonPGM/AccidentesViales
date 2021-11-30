datos <- readRDS("DBtoshow.Rds")
datos.mapa <- readRDS("Maptoshow.Rds")
lista.iconos <- iconList("Con muertos" = makeIcon(iconUrl = "https://i.imgur.com/kiydjCM.png",
                                                  iconWidth = 16, iconHeight = 16),
                         "Con heridos" = makeIcon(iconUrl = "https://i.imgur.com/kiydjCM.png",
                                                  iconWidth = 16, iconHeight = 16),
                         "Solo danos" = makeIcon(iconUrl = "https://i.imgur.com/kiydjCM.png",
                                                 iconWidth = 16, iconHeight = 16))


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
    select(lng, lat, GRAVEDAD_ACCIDENTE) %>%
      mutate(lng = as.numeric(lng), lat = as.numeric(lat))
  print(colnames(temp))
  m <- leaflet(temp) %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=lng, lat=lat, clusterOptions = markerClusterOptions(),
                   icon = lista.iconos[GRAVEDAD_ACCIDENTE]) 
  return(m)
}
