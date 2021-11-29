SERVER <- function(input, output) {
  output$plotVis <- renderPlot({
    plot(1:5, main = input$plottypeVis)
  })
  output$plotPO <- renderPlot({
    plot(1:5, main = input$acctypePO)
  })
  output$plotPT <- renderPlot({
    plot(1:5, main = input$acctypePT)
  })
  output$plotPTh <- renderPlot({
    plot(1:5, main = input$acctypePTh)
  })
}