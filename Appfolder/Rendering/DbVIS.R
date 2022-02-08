datos <- readRDS("DBtoshow.Rds")
datos.mapa <- readRDS("Maptoshow.Rds")
lista.iconos <- iconList("Con muertos" = makeIcon(iconUrl = "./www/risk-skull.png",
                                                  iconWidth = 24, iconHeight = 24),
                         "Con heridos" = makeIcon(iconUrl = "./www/bandage.png",
                                                  iconWidth = 24, iconHeight = 24),
                         "Solo danos" = makeIcon(iconUrl = "./www/car-crash.png",
                                                 iconWidth = 48, iconHeight = 48))
html.legend <- "<img src='risk-skull.png'>Con muertos<br/>
<img src='bandage.png'>Con heridos<br/>
<img src='car-crash.png'>Solo daños"

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
  m <- leaflet() %>%
        addTiles() %>%  # Add default OpenStreetMap map tiles
        addMarkers(lng=temp$lng, lat=temp$lat, clusterOptions = markerClusterOptions(),
                   icon = lista.iconos[temp$GRAVEDAD_ACCIDENTE]) %>%
    addControl(html = html.legend, position = "bottomright")
  return(m)
}
