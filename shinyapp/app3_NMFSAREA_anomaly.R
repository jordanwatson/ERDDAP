library(shiny)
library(tidyverse)
library(viridis)

data <- readRDS("Data/mur_SST_stat6_all_columns.rds")
  
data <- data %>% mutate(newyr=ifelse(month<4,year-1,year),
                        month2=ifelse(month<4,month+12,month),
                        season=ifelse(month2%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
                        GOA=ifelse(is.na(GOA),0,GOA)) 
  
mynmfs <- sort(unique(data$NMFSAREA))

ui <- fluidPage(
      titlePanel("Seasonal sea surface temperature anomaly"),
      sidebarPanel(
        selectInput('dnmfs', 'NMFS area', mynmfs, multiple=TRUE),
        sliderInput("range", "Depth:",min = -5000, max = 0,value = c(-200,0))
      ),
      mainPanel(
        plotOutput('mainplot')
      )
    )        

server <- shinyServer(function(input, output){
    ## values <- reactiveValues()  # unused
    ## Your data should be reactive - and reference `input` 
    ## to get user-entered values
    rxData <- reactive({
      #req(input$dnmfs)
      dat <- data %>% 
        filter(m.depth>=input$range[1] & m.depth<=input$range[2] & newyr>2002 & newyr<2018 & NMFSAREA%in%c(input$dnmfs)) %>% 
        group_by(NMFSAREA,season,newyr) %>% 
        summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
        ungroup %>% 
        group_by(NMFSAREA,season) %>% 
        mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE))
      dat
    })
    
    output$mainplot <- renderPlot({
      req(input$dnmfs)
      dataset <- rxData()  # this is the subsetted data
      p <- ggplot(dataset, aes(x = newyr, y = tempanom, fill=factor(NMFSAREA))) +
        geom_bar(stat="identity",position="dodge") + 
        facet_wrap(~season) + 
        theme_bw() + 
        scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
        geom_hline(yintercept=c(-0.5,0.5),linetype=2) + 
        xlab("Year") + 
        ylab("Temperature Anomaly")
      print(p)
    })
  })

shinyApp(ui, server)


