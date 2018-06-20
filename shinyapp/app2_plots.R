library(shiny)
library(ggplot2)
library(viridis)

data <- readRDS("Data/mur_SST_stat6_all_columns.rds")

data <- data %>% mutate(newyr=ifelse(month<4,year-1,year),
                        month2=ifelse(month<4,month+12,month),
                        season=ifelse(month2%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
                        GOA=ifelse(is.na(GOA),0,GOA)) 
         
ui <- fluidPage(
  titlePanel("Stat area"),
  fluidRow(
    column(4,
             selectInput("STAT_AREA", 
                         label = "Choose statistical areas to compare", 
                         choices = data$STAT_AREA %>% unique %>% sort,
                         multiple = TRUE
             )
           ),
             plotOutput('mainplot')
           )
         )        

server <- shinyServer(function(input, output) {
    ## values <- reactiveValues()  # unused
    ## Your data should be reactive - and reference `input` 
    ## to get user-entered values
    rxData <- reactive({
      req(input$STAT_AREA)
      dat <- data %>% 
        filter(STAT_AREA%in%input$STAT_AREA)
      
      dat
    })
    
    output$mainplot <- renderPlot({
      #dataset <- rxData()  # this is the subsetted data
      p <- ggplot(rxData(), aes(x = date, y = sst.mean,color=factor(STAT_AREA)))
      p <- p + 
        geom_line() + 
        theme_bw() + 
        xlab("Date") + 
        ylab("Temperature") + 
        scale_color_viridis(discrete=TRUE)
      print(p)
    })
  })

shinyApp(ui, server)




dataset <- diamonds

shinyUI(fluidPage(
  
  title = "Diamonds Explorer",
  
  plotOutput('plot'),
  
  hr(),
  
  fluidRow(
    column(3,
           h4("Diamonds Explorer"),
           sliderInput('sampleSize', 'Sample Size', 
                       min=1, max=nrow(dataset), value=min(1000, nrow(dataset)), 
                       step=500, round=0),
           br(),
           checkboxInput('jitter', 'Jitter'),
           checkboxInput('smooth', 'Smooth')
    ),
    column(4, offset = 1,
           selectInput('x', 'X', names(dataset)),
           selectInput('y', 'Y', names(dataset), names(dataset)[[2]]),
           selectInput('color', 'Color', c('None', names(dataset)))
    ),
    column(4,
           selectInput('facet_row', 'Facet Row', c(None='.', names(dataset))),
           selectInput('facet_col', 'Facet Column', c(None='.', names(dataset)))
    )
  )
))


