library(shiny)

# Define UI for application that plots a ccgraph 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Collaborative Composite Graphs"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    selectInput("ccg", "Choose a Collaborative Composite Graph:", 
                choices = c("example1", "example2", "random"), selected="random")
    
    , sliderInput("n_s", label="Number of Researchers:"
                , min = 1, max = 100, value = 10)
#    , helpText("Note: The more researchers, the merrier!")
    , sliderInput("capacity", label="Multitask Capacity:"
                  , min = 1, max = 100, value = 2)
    , sliderInput("p_s", label="Probability of Social Edge:"
                  , min = 0, max = 1, value = 0.2)
    , sliderInput("n_t", label="Number of Tasks:"
                  , min = 1, max = 100, value = 10)
    , sliderInput("r", label="Probability of Task Assignment:"
                  , min = 0, max = 1, value = 0.1)
    
    , selectInput("ctype", "Choose a Collaboration Function:", 
                choices = c("additive", "social-density", "multiplicative"), selected="social-density")
    
    , selectInput("alg", "Choose an Assignment Algorithm:", 
                choices = c("random", "greedy", "greedy-smart"))
    , checkboxInput("blind", "Make the assignments blindly?", TRUE)   
    
    # Animation with custom interval (in ms) to control speed, plus looping
    , sliderInput("edges", "Edges to Add:", 1, 10, 1, step = 1, 
                animate=animationOptions(interval=3000, loop=T))
    , checkboxInput("reset", "Build a new graph", FALSE)   
#    , submitButton("New Graph")
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