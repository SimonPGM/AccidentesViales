source("./Rendering/DbVIS.R")
SERVER <- function(input, output) {
  output$dataVis <- renderDataTable({
    generate.db(input$dateinputstartVis, input$dateinputendVis,
                input$acctypeVis, input$gravtypeVis)
  }, options = list(pageLength = 10, lengthChange = F))
  output$mapVis <- renderLeaflet({
    generate.db.map(input$dateinputstartVis, input$dateinputendVis,
                input$acctypeVis, input$gravtypeVis)
  })
  output$plotPO <- renderPlot({
    plot(1:5, main = input$acctypePO)
  })
  output$plotPT <- renderPlot({
    plot(1:5, main = input$acctypePT)
  })
  output$plotPTh <- renderPlotly({
    graph <- readRDS("plotfit2020.Rds")
    graph
  })
}