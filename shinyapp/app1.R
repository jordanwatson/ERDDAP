library(dplyr)
library(ggplot2)
library(shiny)

data <- readRDS("Data/mur_SST_stat6_grid.RDS") %>% 
  group_by(year,month,STAT_AREA) %>% 
  summarise(meantemp=mean(sst.mean))

sabrina <- readOGR(dsn="Data",layer="Groundfish_Statistical_Areas_2001")
stat <- sabrina@data %>% 
  select(STAT_AREA,
         FMP_AREA_C,
         IFQ_SABLEF,
         NMFSAREA=NMFS_REP_1,
         STATEFED=WATERS_COD) %>% 
  mutate(STAT_AREA=as.character(STAT_AREA),
         NMFSAREA=as.character(NMFSAREA)) %>% 
  inner_join(data)

mystats <- unique(stat$STAT_AREA)
myregions <- unique(stat$FMP_AREA_C)
myifq <- unique(stat$IFQ_SABLEF)
mynmfs <- unique(stat$NMFSAREA)


ui <- fluidPage(
  titlePanel(title=h4("Races", align="center")),
  sidebarPanel( 
    selectInput("variable","Variable:",
                c("Stat area"=as.list(mystats),
                  "Region"=as.list(myregions),
                  "Sablefish Regions"=as.list(myifq),
                  "NMFS Area"=as.list(mynmfs)))
  )
)


server <- function(input, output) {
  
  raw <- stat
  
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
    
    raw %>%
      filter_(filt1) %>%
      filter_(filt2)
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


  