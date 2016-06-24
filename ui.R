library(shiny)

fluidPage(
  # Application title
  titlePanel("Word Cloud of Presidential Candidates"),
  
  sidebarLayout(
    # Sidebar with a slider and selection inputs
    sidebarPanel(
      selectInput("selection", "Choose a candidate:",
                  choices = candidates),
      actionButton("update", "Change"),
      hr(),
      sliderInput("freq",
                  "Minimum Frequency:",
                  min = 1,  max = 50, value = 15),
      sliderInput("max",
                  "Maximum Number of Words:",
                  min = 1,  max = 300,  value = 100),
      hr()
      
    ),
    
    # Show Word Cloud and freq plot 
    mainPanel(
      plotOutput("word_cloud"),
      #plotOutput("freq_plot"),
      conditionalPanel(condition = "input.density == true",
                       sliderInput(inputId = "bw_adjust",
                                   label = "Bandwidth adjustment:",
                                   min = 0.2, max = 2, value = 1, step = 0.2)
      )
      
    )
  )
)