Ui <- navbarPage("Análisis de accidentalidad en Medellín",
                 tabPanel("Introducción"),
                 tabPanel("Presentación de los datos"),
                 navbarMenu("Modelo predictivo",
                            tabPanel("Periodo 2014-2018"),
                            tabPanel("Periodo 2018-2019"),
                            tabPanel("Periodo 2020-2021")
                            ),
                 tabPanel("Clasificación de los barrios"),
                 tabPanel("Información adicional")
)