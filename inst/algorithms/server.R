library(shiny)

# hard-coded examples
ccg1 = ccg.game(n1 = 10, p1 = 0.5, n2 = 2, r = 0.2)
ccg2 = ccg.game(n1 = 20, p1 = 0.2, n2 = 1, r = 0.2)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested ccgraph. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  ccg <- reactive({  
    if (input$ccg == "example1") {
      return(ccg1)
    } 
    if (input$ccg == "example2") {
      return(ccg2)
    } 
    return(ccg.game(n1 = 50, p1 = 0.3, n2 = 3, r = 0.1))
  })
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$plot <- renderPlot({
    plot(ccg())
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(ccg())
  })
  
  # Print out the ccgraph
  output$print <- renderPrint({
    print(ccg())
  })
})