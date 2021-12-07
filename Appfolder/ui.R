library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
Ui <- navbarPage(
  "Choquentérese",
  tags$head(tags$link(rel = "icon", href = "logo.png")),
  includeCSS("./www/styles.css"),
  tabPanel("Introducción",
           fluidPage(fluidRow(
             column(12,
                    includeHTML("./www/header.html"))
           ),
           fluidRow(
             column(12,
                    includeHTML("./www/Intro.html"))
           ))),
  tabPanel("Visualización de datos",
           fluidPage(
             fluidRow(column(12,
                             includeHTML(
                               "./www/header.html"
                             ))),
             fluidRow(
               column(
                 2,
                 radioButtons("plottypeVis",
                              "Tipo de visualización",
                              c("Datos", "Mapa")),
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
                   c("Sin filtro", "Con heridos", "Solo daños",
                     "Con muertos"),
                   selected = "Sin filtro"
                 )
               ),
               column(
                 10,
                 conditionalPanel(condition = "input.plottypeVis == 'Datos'",
                                  dataTableOutput("dataVis")),
                 conditionalPanel(condition = "input.plottypeVis == 'Mapa'",
                                  leafletOutput(
                                    "mapVis", width = "100%", height = 500
                                  ))
               )
             )
           )),
  navbarMenu(
    "Modelo predictivo",
    tabPanel("Periodo 2014-2018",
             fluidPage(
               fluidRow(column(12,
                               includeHTML(
                                 "./www/header.html"
                               ))),
               fluidRow(
                 column(
                   2,
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
                 column(10,
                        plotlyOutput("plotPO"))
               )
             )),
    tabPanel("Periodo 2018-2019",
             fluidPage(
               fluidRow(column(12,
                               includeHTML(
                                 "./www/header.html"
                               ))),
               fluidRow(
                 column(
                   2,
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
                 column(10,
                        plotlyOutput("plotPT"))
               )
             )),
    tabPanel("Periodo 2020-2021",
             fluidPage(
               fluidRow(column(12,
                               includeHTML(
                                 "./www/header.html"
                               ))),
               fluidRow(
                 column(
                   2,
                   dateInput(
                     "dateinputstartPTh",
                     "Seleccione la fecha de inicio para la visualización",
                     value = "2020-01-01",
                     min = "2020-01-01",
                     max = "2021-12-31",
                     format = "dd-mm-yyyy",
                     language = "es"
                   ),
                   dateInput(
                     "dateinputendPTh",
                     "Seleccione la fecha de fin para la visualización",
                     value = "2021-12-31",
                     min = "2020-01-01",
                     max = "2021-12-31",
                     format = "dd-mm-yyyy",
                     language = "es"
                   ),
                   selectInput(
                     "acctypePTh",
                     "Elija el tipo de accidente para filtrar",
                     c(
                       "Sin filtro",
                       "Atropello",
                       "Caída de ocupante",
                       "Choque",
                       "Volcamiento",
                       "Incendio",
                       "Otro"
                     ),
                     selected = "Sin filtro"
                   )
                 ),
                 column(10,
                        plotlyOutput('plotPTh'))
               )
             ))
  ),
  tabPanel("Clasificación de los barrios",
           fluidPage(
             fluidRow(column(12,
                             includeHTML(
                               "./www/header.html"
                             ))),
             fluidRow(column(4,
                             includeHTML(
                               "./www/sidemap.html"
                             )),
                      column(8,
                             leafletOutput("mapacluster")))
           )),
  tabPanel("Información adicional",
           includeHTML("./www/Info.html"))
)