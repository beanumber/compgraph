library(shiny)

# Define UI for application that plots a ccgraph 
shinyUI(pageWithSidebar(
  
  # Application title
  headerPanel("Collaborative Composite Graphs"),
  
  # Sidebar with a slider input for number of observations
  sidebarPanel(
    selectInput("ccg", "Choose a Collaborative Composite Graph:", 
                choices = c("example1", "example2", "random")),
    
    selectInput("c", "Choose a Collaboration Function:", 
                choices = c("additive", "social-density")),
    
    selectInput("alg", "Choose an Assignment Algorithm:", 
                choices = c("random", "greedy"))
    
#    submitButton("Add Assignment")
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