source("./Rendering/DbVIS.R")
source("./Rendering/PlotModel.R")
library(shiny)
library(tidyverse)
library(leaflet)
library(plotly)
SERVER <- function(input, output) {
  output$mapVis <- renderLeaflet({
    generate.db.map(
      input$dateinputstartVis,
      input$dateinputendVis,
      input$acctypeVis,
      input$gravtypeVis
    )
  })
  output$plotPO <- renderPlotly({
    generate.vis(
      vis.train,
      input$dateinputstartPO,
      input$dateinputendPO,
      input$acctypePO,
      input$freqPO
    )
  })
  output$plotPT <- renderPlotly({
    generate.vis(
      vis.test,
      input$dateinputstartPT,
      input$dateinputendPT,
      input$acctypePT,
      input$freqPT
    )
  })
  output$plotPTh <- renderPlotly({
    graph <- readRDS("plotfit2020.Rds")
    graph
  })
  output$mapacluster <- renderLeaflet({
    clustermap <- readRDS("mapa.Rds")
    clustermap
  })
}