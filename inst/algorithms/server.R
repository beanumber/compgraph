library(shiny)
require(compgraph)
# require(compgraph, lib.loc="/Users/bbaumer/R/library")
require(igraph)
# require(igraph, lib.loc="/Users/bbaumer/R/library")
# Why are these necessary?
# source("R/compgraph.R")
# source("R/ccgraph.R")

# hard-coded examples
ccg2 = ccg.game(n1 = 20, p1 = 0.2, n2 = 1, r = 0.2)

# Greedy is not better than 3/2
g1 = graph(edges = c(2,3), directed=FALSE)
V(g1)$expertise = c(2,1,1)

g2 = graph(NULL, n=1, directed=TRUE)
V(g2)$difficulty = c(4)

ccg1 = ccgraph(compgraph(g1, g2, name="Greedy", r=0))

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
              , random = ccg.game(n1 = input$n_s, p1 = input$p_s, n2 = input$n_t, r = input$r, capacity = input$capacity)
       )
       break;
#     } else {
     }
     cat(input$edges)
     ccg$ctype <<- input$ctype
        ccg <<- add.assignments(ccg, input$alg, blind=input$blind)
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