vis.train <- readRDS("predict_train.Rds")
vis.test <- readRDS("predict_test.Rds")

shinytovar <- function(x, type = "acc") {
  if (type == "acc") {
    goal <- switch(x,
                   "Atropello" = "ATROPELLO",
                   "Caída de ocupante" = "CAIDA.OCUPANTE",
                   "Choque" = "CHOQUE",
                   "Volcamiento" = "VOLCAMIENTO",
                   "Incendio" = "INCENDIO",
                   "Otro" = "OTRO")
  } else {
    goal <- switch(x,
                   "Diario" = "FECHA",
                   "Semanal" = "SEMANA",
                   "Mensual" = "MES")
  }
}

generate.vis <- function(db, date.lower, date.upper, acctype, tunit) {
  dates <- sort(c(as.Date(date.lower), as.Date(date.upper)))
  auxacc <- acctype
  auxtunit <- tunit
  keywordacc <- shinytovar(acctype)
  keywordtunit <- shinytovar(tunit, ".")
  
  if (keywordtunit == "SEMANA") {
    temp <- db %>%
      arrange(ANIO, SEMANA.D) %>%
      select(keywordacc, keywordtunit) %>%
      group_by(across({{ keywordtunit }})) %>%
      #summarise(ACCIDENTES = sum(across({{ keywordacc }}))) %>%
      ungroup()
    return(temp)
  } else if (keywordtunit == "MES") {
    temp <- db %>%
      arrange(ANIO, MES.D) %>%
      select(keywordacc, keywordtunit) %>%
      group_by(across({{ keywordtunit }})) %>%
      #summarise(ACCIDENTES = sum(across({{ keywordacc }}))) %>%
      ungroup()
    return(temp)
  } else {
      temp <- db %>%
        filter(between(FECHA, dates[1], dates[2])) %>%
        select(keywordacc, keywordtunit)
  }
  temp <- temp %>%
    select(keywordtunit, everything()) %>%
    arrange(across({{ keywordtunit }}))
  return(temp)
}


# p <- ggplot(temp, aes_string(keywordtunit, "ACCIDENTES")) +
#   geom_point() +
#   geom_path(aes(group = 1)) +
#   labs(x = "Fecha", y = paste("Cantidad de accidentes [", tolower(auxtunit), "]", sep = ""),
#        title = paste("Cantidad de accidentes del tipo", tolower(auxacc))) +
#   theme_minimal() +
#   theme(plot.title = element_text(hjust = .5))
# return(ggplotly(p))