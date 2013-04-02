library(shiny)
require(compgraph)
require(igraph)
require(mosaic)

# hard-coded examples
ccg1 = ccg.game(n1 = 10, p1 = 0.5, n2 = 2, r = 0.2)
ccg2 = ccg.game(n1 = 20, p1 = 0.2, n2 = 1, r = 0.2)

# Define server logic required to generate and plot a random distribution
shinyServer(function(input, output) {
  
  ccg = reactiveValues()
  ccg = ccg1
  # Reactive expression to generate the requested ccgraph. This is 
  # called whenever the inputs change. The renderers defined 
  # below then all use the value computed from this expression
  
   get.ccg <- reactive({  
     if (input$reset) {
       ccg <<- switch(input$ccg
                         , example1 = ccg1
                         , example2 = ccg2
              , random = ccg.game(n1 = input$n_s, p1 = input$p_s, n2 = input$n_t, r = input$r)
       )
       break;
#     } else {
     }
     cat(input$edges)
        ccg <<- switch(input$alg
            , random = add.random.assignments(ccg)
            , greedy = add.greedy.assignments(ccg)                      
        )
#    }
    return(ccg)
  })
  
  # Expression that generates a plot of the distribution. The expression
  # is wrapped in a call to renderPlot to indicate that:
  #
  #  1) It is "reactive" and therefore should be automatically 
  #     re-executed when inputs change
  #  2) Its output type is a plot 
  #
  output$plot <- renderPlot({
    plot(get.ccg())
  })
  
  # Generate a summary of the data
  output$summary <- renderPrint({
    summary(get.ccg())
  })
  
  # Print out the ccgraph
  output$print <- renderPrint({
    print(get.ccg())
  })
})