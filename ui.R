#############################################################
# confint - visualize confindence interval by michael moon  #
#                                                           #
# Shiny user interface                                      #
#############################################################
library(shiny)

shinyUI(fluidPage(

  # Application title
  titlePanel("Confidence interval demonstration"),

  # Sidebar with a inputs 
  sidebarLayout(
    sidebarPanel(
      selectInput("dist", "Distribution", 
                  list("Normal" = 1),
                  width = "100%"
      ),
      numericInput("parm1", "Mean", 0),
      numericInput("parm2", "Standard Deviation", 1),
      sliderInput("cilvl", "Confidence Level", 0.55, 0.95, 0.95, 0.05),
      sliderInput("nsamp", "Sample Size (n)", 50, 1000, 50, 10),
      sliderInput("niter", "Number of Samples", 10, 100, 50, 10)
    ),

    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("ciPlot", height = "350px"),
      plotOutput("distPlot", height = "100px"),
      br(),
      fluidRow(column(3, actionButton("sim", HTML("<b>Simulate</br>New Data</b>"), width = "100%",
                                      style='height:100px; ')),
               column(9, strong("Frequency of observing the parameter in CI"), align = "center",
                      fluidRow(column(4,  textOutput("countci"), style = "padding:30px"),
                               column(8, plotOutput("freqPlot", height = "80px")))
                      )
               )
    ),
    
    position = "right"
  )
))
