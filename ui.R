#############################################################
# confint - visualize confindence interval by michael moon  #
#                                                           #
# Shiny user interface                                      #
#############################################################
shinyUI(
  fluidPage(
  # Application title
  titlePanel("Confidence Interval"),
  # Sidebar with a inputs 
  sidebarLayout(
    sidebarPanel(
      sliderInput("cilvl", "Confidence Level", 0.55, 0.95, 0.95, 0.05),
      br(),
      sliderInput("nsamp", "Sample Size per Simulation", 10, 1000, 100, 10),
      br(),
      sliderInput("niter", "Number of Simulations", 10, 100, 50, 10),
      br(),
      actionButton("sim", strong("Simulate"), 
                   width = "100%", height = "150px",
                   style = "background-color:#2377BA; color:white;")
    ),
    # Show a plot of the generated distribution
    mainPanel(
      h5("Confidence intervals of simulated sample means"),
      plotOutput("ciPlot", height = "350px"),
      h5("Asymptotic distribution of sample mean"),
      plotOutput("distPlot", height = "100px"),
      br(),
      fluidRow(strong("Frequency of observing the parameter in CI"), align = "center"),
      fluidRow(column(8, plotOutput("freqPlot", height = "80px"), actionLink("reset", "Reset")),
               column(4,  textOutput("countci"), style = "padding:30px"))
      ),
    position = "right"
  )
))
