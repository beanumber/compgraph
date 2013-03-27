library(shiny)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  # Reactive expression to generate the requested ccgraph. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  ccg <- reactive({  
    ccg.game(n1 = input$n_s, p1 = input$p_s, n2 = input$n_t, r = input$r)
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