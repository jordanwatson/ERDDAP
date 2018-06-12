library(dplyr)
library(ggplot2)
library(shiny)

mydata <- readRDS("Data/mur_SST_stat6_all_columns.RDS")
mystats <- unique(mydata$STAT_AREA)
myregions <- unique(mydata$FMP_AREA_C)
mygoa <- unique(mydata$GOA)
mynmfs <- unique(mydata$NMFSAREA)

server <- function(input, output) {
  raw <- mydata
  output$stat6list <- renderUI({
    stat6list <- sort(unique(raw$STAT_AREA), decreasing = FALSE)
    stat6list <- append(stat6list, "All", after =  0)
    selectizeInput("cutchoose", "Stat area:", stat6list)
  })
  
  output$nmfsarealist <- renderUI({
    nmfsarealist <- sort(unique(raw$NMFSAREA), decreasing = FALSE)
    nmfsarealist <- append(nmfsarealist, "All", 0)
    selectizeInput("colorchoose", "NMFS Area:", nmfsarealist)
  }) 
  
  data <- reactive({
    req(input$colorchoose)
    req(input$cutchoose)
    
    if(input$colorchoose == "All") {
      filt1 <- quote(NMFSAREA != "@?><")
    } else {
      filt1 <- quote(NMFSAREA == input$colorchoose) 
    }
    
    if (input$cutchoose == "All") {
      filt2 <- quote(STAT_AREA != "@?><")
    } else {
      filt2 <- quote(STAT_AREA == input$cutchoose)
    }
    
    columns = names(mydata)
    if (!is.null(input$select)) {
    columns = input$select
    }
    
    (raw %>%
      filter_(filt1) %>%
      filter_(filt2))[,columns,drop=FALSE]
  })
 
   output$table <- renderDataTable({
    data()
  })
  
  # Downloadable csv of selected dataset ----
  output$downloadData_csv <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".csv", sep = "")
    },
    content = function(file) {
      write.csv(data(), file, row.names = FALSE)
    }
  )
  
  # Downloadable RDS of selected dataset ----
  output$downloadData_RDS <- downloadHandler(
    filename = function() {
      paste(input$dataset, ".RDS", sep = "")
    },
    content = function(file) {
      saveRDS(data(), file=file)
    }
  )
}


ui <- fluidPage(
  # Application title
  titlePanel("Sea surface temperatures in the North Pacific"),

  sidebarLayout(
    sidebarPanel(
      uiOutput("stat6list"),
      uiOutput("nmfsarealist"),
      selectInput("select", "Select columns to display", names(mydata), multiple = TRUE),
      # Button
      downloadButton("downloadData_csv", "Download csv"),
      downloadButton("downloadData_RDS", "Download RDS"),
      width=3
    ),
    mainPanel(
      dataTableOutput("table")
    )
  )
)

shinyApp(ui, server)


