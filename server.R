#############################################################
# confint - visualize confindence interval by michael moon  #
#                                                           #
# Shiny server                                              #
# rsconnect::deployApp(".") to deploy                       #
#############################################################
library(shiny)

shinyServer(function(input, output) {
  library(RColorBrewer)
  source("cicalc.R")
  palette(brewer.pal(8, "Set2"))
  output$distPlot <- renderPlot(plotdist(input))
  output$ciPlot <- renderPlot(plotcint(input))
  output$countci <- renderText(paste(nciinc[1], "/", 
                                     input$niter, "=", 
                                     round(rciinc[1], 2)))
  output$freqPlot <- renderPlot(plotciinc(input))
  
  observeEvent(input$sim, 
               {
                 output$ciPlot <- renderPlot(plotcint(input))
                 output$countci <- renderText(paste(nciinc[1], "/", 
                                                    input$niter, "=", 
                                                    round(rciinc[1], 2)))
                 output$freqPlot <- renderPlot(plotciinc(input))
               }
               )
  observeEvent(input$dist, output$freqPlot <- renderPlot(plotciinc(input)))
  observeEvent(input$parm1, output$freqPlot <- renderPlot(plotciinc(input)))
  observeEvent(input$parm2, output$freqPlot <- renderPlot(plotciinc(input)))
  observeEvent(input$cilvl, output$freqPlot <- renderPlot(plotciinc(input)))
  observeEvent(input$nsamp, output$freqPlot <- renderPlot(plotciinc(input)))
  observeEvent(input$niter, output$freqPlot <- renderPlot(plotciinc(input)))
})
