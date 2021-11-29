generate.vis <- function(db, date.lower, date.upper,
                         keyword = "Sin filtro") {
  dates <- sort(c(as.Date(date.lower), as.Date(date.upper)))
  if (keyword == "Sin filtro") {
    temp <- db %>%
      filter(between(FECHA_ACCIDENTE, dates[1], dates[2])) %>%
      group_by(FECHA_ACCIDENTE) %>%
      summarise(ACCIDENTES_DIARIOS = n()) %>%
      ungroup()
    p <- ggplot(temp, aes(FECHA_ACCIDENTE, ACCIDENTES_DIARIOS)) +
      geom_path() +
      labs(x = "Fecha", y = "Cantidad de accidentes diarios",
           title = paste("Cantidad de accidentes diarios")) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = .5))
    return(plotly::ggplotly(p))
  } else {
    aux <- keyword
    keyword <- stringi::stri_trans_general(keyword, 'Latin-ASCII')
    temp <- db %>%
      filter(between(FECHA_ACCIDENTE, dates[1], dates[2]),
             CLASE_ACCIDENTE == keyword) %>%
      group_by(FECHA_ACCIDENTE) %>%
      summarise(ACCIDENTES_DIARIOS = n()) %>%
      ungroup()
    p <- ggplot(temp, aes(FECHA_ACCIDENTE, ACCIDENTES_DIARIOS)) +
      geom_point() +
      geom_segment(aes(x = FECHA_ACCIDENTE, xend = FECHA_ACCIDENTE,
                       y = 0, yend = ACCIDENTES_DIARIOS)) +
      labs(x = "Fecha", y = "Cantidad de accidentes diarios",
           title = paste("Cantidad de accidentes diarios del tipo", aux)) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = .5))
    return(plotly::ggplotly(p))
  }
}