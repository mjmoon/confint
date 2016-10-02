#############################################################
# confint - visualize confindence interval by michael moon  #
#                                                           #
# Shiny server                                              #
# rsconnect::deployApp(".") to deploy                       #
#############################################################
shinyServer(function(input, output, session) {
  library(RColorBrewer)
  source("cicalc.R")
  palette(brewer.pal(8, "Set2"))
  output$distPlot <- renderPlot(plotdist(input))
  output$ciPlot <- renderPlot(isolate(plotci(input$niter, input$nsamp, input$cilvl)))
  output$countci <- renderText(paste(nciinc[1], "/", 
                                     isolate(input$niter), "=", 
                                     round(rciinc[1], 2)))
  output$freqPlot <- renderPlot({plotciinc(isolate(input$cilvl), rciinc)})
  
  observeEvent(input$sim, 
               {
                 output$ciPlot <- renderPlot(isolate(plotci(input$niter, input$nsamp, input$cilvl)))
                 output$countci <- renderText(paste(nciinc[1], "/", 
                                                    isolate(input$niter), "=", 
                                                    round(rciinc[1], 2)))
                 output$freqPlot <- renderPlot({plotciinc(isolate(input$cilvl), rciinc)})
                 }
               )
  observeEvent(input$reset, {
    nciinc <<- NULL; rciinc <<- NULL
    output$freqPlot <- renderPlot(plotciinc(input$cilvl, -1))
    })
})
