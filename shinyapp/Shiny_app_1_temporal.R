library(shiny)  
library(dplyr)
library(purrrlyr)

server <- function(input, output) {
    
    output$table <- renderTable({
        y <- c(input$level,"year")
        mydata %>% 
          mutate(date=as.character(date),
                 year=as.integer(year),
                 month=as.integer(month),
                 week=as.integer(week)) %>% 
          purrrlyr::slice_rows(y) %>% 
          summarise(meantemp=mean(sst.mean,na.rm=TRUE))
    })
}

ui <- fluidPage(
  titlePanel("Orders"),
  sidebarLayout(
    sidebarPanel(
      selectInput("Region_Input", label = h5("Choose a Region"), 
                  choices = list("A", "B")),
      radioButtons("level", "Average temperatures by:",
                   list("date", "week","month"))
      
    ),
    mainPanel(
      verbatimTextOutput("Level_Select"),
      tableOutput(outputId="table")
      
    )))

shinyApp(ui, server)
