library(shiny)
library(tidyverse)
library(viridis)

data <- readRDS("Data/mur_SST_stat6_all_columns.rds")
  
data <- data %>% mutate(newyr=ifelse(month<4,year-1,year),
                        month2=ifelse(month<4,month+12,month),
                        season=ifelse(month2%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
                        GOA=ifelse(is.na(GOA),0,GOA)) 
  
data %>% 
  filter(m.depth>=(-200) & newyr>2002 & newyr<2018) %>% 
  group_by(NMFSAREA,season,newyr) %>% 
  summarise(meantemp=mean(sst.mean)) %>% 
  ungroup %>% 
  group_by(NMFSAREA,season) %>% 
  mutate(tempanom=(meantemp-mean(meantemp))/sd(meantemp)) %>% 
  ggplot(aes(newyr,tempanom,fill=factor(NMFSAREA))) + 
  geom_bar(stat="identity",position="dodge") + 
  facet_wrap(~season) + 
  theme_bw() + 
  scale_fill_viridis_d(name="NMFS Area") + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2)


data %>% 
  filter(m.depth>=(-200) & newyr>2002 & newyr<2018 & (NMFSAREA%in%c("610","620","630","640","650"))) %>% 
  group_by(NMFSAREA,season,newyr) %>% 
  summarise(meantemp=mean(sst.mean)) %>% 
  ungroup %>% 
  group_by(NMFSAREA,season) %>% 
  mutate(tempanom=(meantemp-mean(meantemp))/sd(meantemp)) %>% 
  ggplot(aes(newyr,tempanom,fill=factor(NMFSAREA))) + 
  geom_bar(stat="identity",position="dodge") + 
  facet_wrap(~season) + 
  theme_bw() + 
  scale_fill_viridis_d(name="NMFS Area") + 
  geom_hline(yintercept=c(-0.5,0.5),linetype=2)

mystats <- unique(data$STAT_AREA)
mynmfs <- sort(unique(data$NMFSAREA))
mygoa <- sort(unique(data$GOA[data$GOA!="0"]))


shinyApp(
  shinyUI(
    fluidPage(
      titlePanel("Seasonal sea surface temperature anomaly"),
      sidebarPanel(
        #selectInput('dstat', 'Stat area (ctrl+click for multiple)', mystats, multiple=TRUE),
        selectInput('dnmfs', 'NMFS area', mynmfs, multiple=TRUE)
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
      #dat <- data %>% 
      #  filter(STAT_AREA==input$dstat)
      dat <- data %>% 
        filter(m.depth>=(-200) & newyr>2002 & newyr<2018 & NMFSAREA%in%c(input$dnmfs)) %>% 
        group_by(NMFSAREA,season,newyr) %>% 
        summarise(meantemp=mean(sst.mean)) %>% 
        ungroup %>% 
        group_by(NMFSAREA,season) %>% 
        mutate(tempanom=(meantemp-mean(meantemp))/sd(meantemp))
      dat
    })
    
    output$mainplot <- renderPlot({
      dataset <- rxData()  # this is the subsetted data
      p <- ggplot(dataset, aes(x = newyr, y = tempanom, fill=factor(NMFSAREA))) +
        geom_bar(stat="identity",position="dodge") + 
        facet_wrap(~season) + 
        theme_bw() + 
        scale_fill_viridis_d(name="NMFS Area") + 
        geom_hline(yintercept=c(-0.5,0.5),linetype=2)
      print(p)
    })
  })
)



shinyApp(
  shinyUI(
    fluidPage(
      titlePanel("Seasonal sea surface temperature anomaly"),
      sidebarPanel(
        #selectInput('dstat', 'Stat area', mystats, multiple=TRUE),
        selectInput('dnmfs', 'NMFS area', mynmfs, multiple=TRUE),
        selectInput('dgoa', 'GOA area', mygoa, multiple=TRUE),
        sliderInput("range", "Depth:",min = -5000, max = 0,value = c(-500,0))
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
      #dat <- data %>% 
      #  filter(STAT_AREA==input$dstat)
      dat <- data %>% 
        filter(m.depth>=input$range[1] & m.depth<=input$range[2] & newyr>2002 & newyr<2018 & NMFSAREA%in%c(input$dnmfs) & GOA%in%c(input$dgoa)) %>% 
        group_by(NMFSAREA,season,newyr) %>% 
        summarise(meantemp=mean(sst.mean)) %>% 
        ungroup %>% 
        group_by(NMFSAREA,season) %>% 
        mutate(tempanom=(meantemp-mean(meantemp))/sd(meantemp))
      dat
    })
    
    output$mainplot <- renderPlot({
      dataset <- rxData()  # this is the subsetted data
      p <- ggplot(dataset, aes(x = newyr, y = tempanom, fill=factor(NMFSAREA))) +
        geom_bar(stat="identity",position="dodge") + 
        facet_wrap(~season) + 
        theme_bw() + 
        scale_fill_viridis_d(name="NMFS Area") + 
        geom_hline(yintercept=c(-0.5,0.5),linetype=2)
      print(p)
    })
  })
)


shinyApp(
  shinyUI(
    fluidPage(
      titlePanel("Seasonal sea surface temperature anomaly"),
      sidebarPanel(
        #selectInput('dstat', 'Stat area', mystats, multiple=TRUE),
        selectInput('dnmfs', 'NMFS area', mynmfs, multiple=TRUE),
        selectInput('dgoa', 'GOA area', mygoa, multiple=TRUE),
        sliderInput("range", "Depth:",min = -5000, max = 0,value = c(-500,0))
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
      #dat <- data %>% 
      #  filter(STAT_AREA==input$dstat)
      dat <- data %>% 
        filter(m.depth>=input$range[1] & m.depth<=input$range[2] & newyr>2002 & newyr<2018 & GOA%in%c(input$dgoa)) %>% 
        group_by(GOA,season,newyr) %>% 
        summarise(meantemp=mean(sst.mean)) %>% 
        ungroup %>% 
        group_by(GOA,season) %>% 
        mutate(tempanom=(meantemp-mean(meantemp))/sd(meantemp))
      dat
    })
    
    output$mainplot <- renderPlot({
      dataset <- rxData()  # this is the subsetted data
      p <- ggplot(dataset, aes(x = newyr, y = tempanom, fill=factor(GOA))) +
        geom_bar(stat="identity",position="dodge") + 
        facet_wrap(~season) + 
        theme_bw() + 
        scale_fill_viridis_d(name="NMFS Area") + 
        geom_hline(yintercept=c(-0.5,0.5),linetype=2)
      print(p)
    })
  })
)

# Sample Data
area <- seq(from=10,to=100, by=10);peri <- seq(from=2710.1,to=2800.1, by=10)
shape <- seq(from=0.1,to=1, by=0.1);perm <- seq(from=1,to=100, by=10)
my_data <- as.data.frame(cbind(area,peri,shape,perm))

ui = fluidPage(
  sidebarPanel(sliderInput("range", "Select Range",min = 1, max = 10, value = c(2, 5)),width = 3),
  mainPanel(tableOutput("view"))
)

server = function(input, output) {
  output$view <- renderTable({
    test <- my_data[input$range[1]:input$range[2],]
    test},include.rownames=FALSE)
}
runApp(list(ui = ui, server = server)) 





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


