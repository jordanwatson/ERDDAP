library(shiny)
library(ggplot2)
library(viridis)



mystats <- unique(stat$STAT_AREA)

shinyApp(
  shinyUI(
    fluidPage(
      titlePanel("Stat area"),
      sidebarPanel(
        selectInput('dstat', 'Stat area', mystats)
      ),
      mainPanel(
        plotOutput('mainplot')
      )
    )        
  ),
  
  shinyServer(function(input, output) {
    ## values <- reactiveValues()  # unused
    ## Your data should be reactive - and reference `input` 
    ## to get user-entered values
    rxData <- reactive({
      dat <- stat %>% 
        filter(STAT_AREA==input$dstat)
      
      dat
    })
    
    output$mainplot <- renderPlot({
      dataset <- rxData()  # this is the subsetted data
      p <- ggplot(dataset, aes(x = date, y = meantemp))
      p <- p + geom_line()
      print(p)
    })
  })
)



shinyApp(
  shinyUI(
    fluidPage(
      titlePanel("Sea surface temperature"),
      sidebarPanel(
        selectInput('dstat', 'Stat area (ctrl+click for multiple)', mystats, multiple=TRUE)
      ),
      mainPanel(
        plotOutput('mainplot')
      )
    )        
  ),
  
  shinyServer(function(input, output) {
    ## values <- reactiveValues()  # unused
    ## Your data should be reactive - and reference `input` 
    ## to get user-entered values
    rxData <- reactive({
      dat <- stat %>% 
        filter(STAT_AREA==input$dstat)
      
      dat
    })
    
    output$mainplot <- renderPlot({
      dataset <- rxData()  # this is the subsetted data
      p <- ggplot(dataset, aes(x = date, y = meantemp, color=factor(STAT_AREA)))
      p <- p + geom_line() + theme_bw() + scale_color_viridis_d()
      print(p)
    })
  })
)




