library(shiny)

# Define UI for application that plots a ccgraph 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Collaborative Composite Graphs"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    sliderInput("n_s", label="Number of Researchers:"
                , min = 1, max = 100, value = 10)
    
    , helpText("Note: The more researchers, the merrier!")
    
    , sliderInput("p_s", label="Probability of Social Edge:"
                  , min = 0, max = 1, value = 0.2)
    , sliderInput("n_t", label="Number of Tasks:"
                  , min = 1, max = 100, value = 10)
    , sliderInput("r", label="Probability of Task Assignment:"
                  , min = 0, max = 1, value = 0.5)
    
  ),
  
  # Show a plot of the generated distribution
  mainPanel(
    tabsetPanel(
      tabPanel("Plot", plotOutput("plot")),
      tabPanel("Summary", verbatimTextOutput("summary")),
      tabPanel("Print", verbatimTextOutput("print"))
    )
  )
))