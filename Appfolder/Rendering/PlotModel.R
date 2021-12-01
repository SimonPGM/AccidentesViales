options(dplyr.summarise.inform = FALSE)
vis.train <- readRDS("predict_train.Rds")
vis.test <- readRDS("predict_test.Rds")

shinytovar <- function(x, type = "acc") {
  if (type == "acc") {
    goal <- switch(
      x,
      "Atropello" = "ATROPELLO",
      "Caída de ocupante" = "CAIDA.OCUPANTE",
      "Choque" = "CHOQUE",
      "Volcamiento" = "VOLCAMIENTO",
      "Incendio" = "INCENDIO",
      "Otro" = "OTRO"
    )
  } else {
    goal <- switch(x,
                   "Diario" = "FECHA",
                   "Semanal" = "SEMANA",
                   "Mensual" = "MES")
  }
}

generate.vis <-
  function(db, date.lower, date.upper, acctype, tunit) {
    dates <- sort(c(as.Date(date.lower), as.Date(date.upper)))
    auxacc <- acctype
    auxtunit <- tunit
    keywordacc <- shinytovar(acctype)
    keywordtunit <- shinytovar(tunit, ".")
    
    temp <- db %>%
      filter(between(FECHA, dates[1], dates[2]))
    if (keywordtunit == "SEMANA") {
      temp <- temp %>%
        group_by(ANIO, SEMANA.D) %>%
        summarise(ACCIDENTES = sum(across({
          {
            keywordacc
          }
        }))) %>%
        mutate(SEMANA = paste(SEMANA.D, "-", ANIO, sep = "")) %>%
        ungroup() %>%
        select(SEMANA, ACCIDENTES) %>%
        mutate(SEMANA = factor(SEMANA, levels = SEMANA))
    } else if (keywordtunit == "MES") {
      temp <- temp %>%
        group_by(ANIO, MES.D) %>%
        summarise(ACCIDENTES = sum(across({
          {
            keywordacc
          }
        }))) %>%
        mutate(MES = paste(MES.D, "-", ANIO, sep = "")) %>%
        ungroup() %>%
        select(MES, ACCIDENTES) %>%
        mutate(MES = factor(MES, levels = MES))
    } else {
      temp <- temp %>%
        select(all_of(keywordtunit), all_of(keywordacc)) %>%
        rename(ACCIDENTES = {
          {
            keywordacc
          }
        })
    }
    p <- ggplot(temp, aes_string(keywordtunit, "ACCIDENTES")) +
      geom_point() +
      geom_path(aes(group = 1)) +
      labs(
        x = "Fecha",
        paste("Cantidad de accidentes [", tolower(auxtunit), "]", sep = ""),
        title = paste("Cantidad de accidentes del tipo", tolower(auxacc))
      ) +
      theme_minimal() +
      theme(plot.title = element_text(hjust = .5), axis.text.x = element_blank())
    
    return(ggplotly(p))
  }
