library(shiny)
library(shinythemes)


ui <- fluidPage(
  
  titlePanel("Inventory Control Simulation"),
  
  theme = shinytheme("readable"),
  
  sidebarLayout(
    sidebarPanel(
      
      sliderInput(inputId = "num_days",
                  label = "Number of Days:",
                  min = 30,
                  max = 365,
                  value = 100,
                  step = 1),
      
      sliderInput(inputId = "start_inventory",
                  label = "Initial Inventory:",
                  min = 0,
                  max = 100,
                  value = 20,
                  step = 1),

      sliderInput(inputId = "restock_prob",
                  label = "Probability of overnight restock:",
                  min = .01,
                  max = .99,
                  value = .3,
                  step = .01),
      
      selectInput(inputId = "num_restock",
                  label = "Amount of Restock:",
                  choices = c(1:20),
                  selected = 6),
      
      numericInput(inputId = "seed",
                   label = "Random Number Seed:",
                   value = 533,
                   min = 1,
                   max = 99999)
      
    ),
    
    mainPanel(
      plotOutput(outputId = "time_plot"),
      paste("Summary of Number of Missed Demands"),
      verbatimTextOutput(outputId = "summary"),
      plotOutput(outputId = "histogram")
    )
  )
  
)

server <- function(input, output, session) {
  
  INVENTORY <- reactive({
    set.seed(input$seed)  
    days <- input$num_days
    beginningofday <- rep(0,days)
    endofday <- rep(0,days)
    demands <- sample(0:8,size=days,replace=TRUE)
    restock <- sample( c(as.numeric(input$num_restock),0), size=days, replace=TRUE, prob=c(input$restock_prob,(1 - input$restock_prob) ) )
    missed <- rep(0,days)
    #Code that runs the simulation and creates beginningofday, endofday, missed
    
    beginningofday[1] <- input$start_inventory
    
    for ( i in 1:days ) {
      
      if (demands[i] > beginningofday[i]) {
        missed[i] <- demands[i]-beginningofday[i]
        endofday[i] <- 0
      }else{
        endofday[i] <- beginningofday[i] - demands[i]
      }
      
      beginningofday[i+1] <- endofday[i] + restock[i] 
      
    }
    
    return( list(beginningofday=beginningofday,endofday=endofday,missed=missed) )
  })
  
  output$time_plot <- renderPlot({ 
    plot.ts(INVENTORY()$endofday)
  })
  
  output$summary <- renderPrint({
    summary(INVENTORY()$missed)
  })
  
  output$histogram <- renderPlot({
    hist(INVENTORY()$beginningofday,
         xlab="Demand at Beginning of Day",
         breaks=0:max(INVENTORY()$beginningofday),
         main="Histogram of Inventory Amount at Beginning of Day")
  })
}

shinyApp(ui, server)