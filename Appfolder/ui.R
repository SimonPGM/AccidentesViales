library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
Ui <- navbarPage(
  "Choquentérese",
  tags$head(tags$link(rel = "icon", href = "logo.png")),
  includeCSS("./www/styles.css"),
  tabPanel(
    "Página Principal",
    fluidPage(
      fluidRow(
        column(
          12,
          includeHTML("./www/header.html")
        )
      ),
      fluidRow(
        column(
          12,
          includeHTML("./www/Intro.html")
        )
      )
    )
  ),
  tabPanel(
    "Mapa de accidentalidad en Medellín",
    fluidPage(
      fluidRow(column(
        12,
        includeHTML(
          "./www/header.html"
        )
      )),
      fluidRow(
        column(
          2,
          includeHTML(
            "./www/mapvis.html"
          ),
          dateInput(
            "dateinputstartVis",
            "Seleccione la fecha de inicio para la visualización",
            value = "2014-07-04",
            min = "2014-07-04",
            max = "2020-09-01",
            format = "dd-mm-yyyy",
            language = "es"
          ),
          dateInput(
            "dateinputendVis",
            "Seleccione la fecha de fin para la visualización",
            value = "2020-08-31",
            min = "2014-07-04",
            max = "2020-09-01",
            format = "dd-mm-yyyy",
            language = "es"
          ),
          selectInput(
            "acctypeVis",
            "Elija el tipo de accidente para filtrar",
            c(
              "Sin filtro",
              "Atropello",
              "Caída de Ocupante",
              "Choque",
              "Volcamiento",
              "Incendio",
              "Otro"
            ),
            selected = "Sin filtro"
          ),
          selectInput(
            "gravtypeVis",
            "Elija la gravedad del accidente para filtrar",
            c(
              "Sin filtro", "Con heridos", "Solo daños",
              "Con muertos"
            ),
            selected = "Sin filtro"
          )
        ),
        column(
          10,
          leafletOutput("mapVis", width = "100%", height = 500)
        )
      )
    )
  ),
  navbarMenu(
    "Predicción de la cantidad de accidentes",
    tabPanel(
      "Periodo 2014-2017",
      fluidPage(
        fluidRow(column(
          12,
          includeHTML(
            "./www/header.html"
          )
        )),
        fluidRow(
          column(
            2,
            includeHTML(
              "./www/plotvis1.html"
            ),
            dateInput(
              "dateinputstartPO",
              "Seleccione la fecha de inicio para la visualización",
              value = "2014-07-04",
              min = "2014-07-04",
              max = "2017-12-31",
              format = "dd-mm-yyyy",
              language = "es"
            ),
            dateInput(
              "dateinputendPO",
              "Seleccione la fecha de fin para la visualización",
              value = "2017-12-31",
              min = "2014-07-04",
              max = "2017-12-31",
              format = "dd-mm-yyyy",
              language = "es"
            ),
            selectInput(
              "acctypePO",
              "Elija el tipo de accidente para filtrar",
              c(
                "Atropello",
                "Caída de ocupante",
                "Choque",
                "Volcamiento",
                "Incendio",
                "Otro"
              ),
              selected = "Atropello"
            ),
            selectInput(
              "freqPO",
              "Elija la unidad de tiempo para la visualización",
              c("Diario", "Semanal", "Mensual"),
              selected = "Diario"
            )
          ),
          column(
            10,
            plotlyOutput("plotPO")
          )
        )
      )
    ),
    tabPanel(
      "Periodo 2018-2019",
      fluidPage(
        fluidRow(column(
          12,
          includeHTML(
            "./www/header.html"
          )
        )),
        fluidRow(
          column(
            2,
            includeHTML(
              "./www/plotvis2.html"
            ),
            dateInput(
              "dateinputstartPT",
              "Seleccione la fecha de inicio para la visualización",
              value = "2018-01-01",
              min = "2018-01-01",
              max = "2019-12-31",
              format = "dd-mm-yyyy",
              language = "es"
            ),
            dateInput(
              "dateinputendPT",
              "Seleccione la fecha de fin para la visualización",
              value = "2019-12-31",
              min = "2018-01-01",
              max = "2019-12-31",
              format = "dd-mm-yyyy",
              language = "es"
            ),
            selectInput(
              "acctypePT",
              "Elija el tipo de accidente para filtrar",
              c(
                "Atropello",
                "Caída de ocupante",
                "Choque",
                "Volcamiento",
                "Incendio",
                "Otro"
              ),
              selected = "Atropello"
            ),
            selectInput(
              "freqPT",
              "Elija la unidad de tiempo para la visualización",
              c("Diario", "Semanal", "Mensual"),
              selected = "Diario"
            )
          ),
          column(
            10,
            plotlyOutput("plotPT")
          )
        )
      )
    ),
    tabPanel(
      "Periodo 2020",
      fluidPage(
        fluidRow(column(
          12,
          includeHTML(
            "./www/header.html"
          )
        )),
        fluidRow(
          column(
            2,
            includeHTML("./www/tts.html")
          ),
          column(
            10,
            plotlyOutput("plotPTh")
          )
        )
      )
    )
  ),
  tabPanel(
    "¿Cuál es el riesgo de accidentarse en los barrios de Medellín?",
    fluidPage(
      fluidRow(column(
        12,
        includeHTML(
          "./www/header.html"
        )
      )),
      fluidRow(
        column(
          4,
          includeHTML(
            "./www/sidemap.html"
          )
        ),
        column(
          8,
          leafletOutput("mapacluster")
        )
      )
    )
  ),
  tabPanel(
    "Acerca de nosotros",
    includeHTML("./www/Info.html")
  )
)