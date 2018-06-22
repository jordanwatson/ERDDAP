library(shiny)
library(tidyverse)
library(viridis)

data <- readRDS("Data/mur_SST_stat6_all_columns.rds")
  
data <- data %>% mutate(newyr=ifelse(month<4,year-1,year),
                        month2=ifelse(month<4,month+12,month),
                        season=ifelse(month2%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
                        GOA=ifelse(is.na(GOA),0,GOA),
                        monthname=month.name[month]) 
  
mynmfs <- sort(unique(data$NMFSAREA))

ui <- fluidPage(
      titlePanel("Seasonal sea surface temperature anomaly"),
      sidebarPanel(
        selectInput('dnmfs', 'NMFS area', mynmfs, multiple=TRUE),
        sliderInput("range", "Depth:",min = -5000, max = 0,value = c(-200,0))
      ),
      mainPanel(
        plotOutput('seasonal_nmfs_plot')
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
    
    output$seasonal_nmfs_plot <- renderPlot({
      req(input$dnmfs)
      dataset <- rxData()  # this is the subsetted data
      p <- ggplot(dataset, aes(x = newyr, y = tempanom, fill=factor(NMFSAREA))) +
        geom_bar(stat="identity",position="dodge") + 
        facet_wrap(~season) + 
        theme_bw() + 
        scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
        geom_hline(yintercept=c(-0.5,0.5),linetype=2) + 
        xlab("Year") + 
        ylab("Temperature Anomaly") + 
        theme(legend.position="top",
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              axis.text = element_text(size=13),
              axis.title = element_text(size=14))
      print(p)
    })
  })

shinyApp(ui, server)


#-----------------------------------------------------------------------
#  Plot anomalies monthly instead of seasonally
#-----------------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Monthly sea surface temperature anomaly"),
  sidebarPanel(
    selectInput('dnmfs', 'NMFS area', mynmfs, multiple=TRUE),
    selectInput('dmonth', 'Month', month.name),
    sliderInput("range", "Depth:",min = -5000, max = 0,value = c(-200,0))
  ),
  mainPanel(
    plotOutput('monthly_nmfs_plot')
  )
)        

server <- shinyServer(function(input, output){
  ## values <- reactiveValues()  # unused
  ## Your data should be reactive - and reference `input` 
  ## to get user-entered values
  rxData <- reactive({
    dat <- data %>% 
      filter(m.depth>=input$range[1] & m.depth<=input$range[2] & NMFSAREA%in%c(input$dnmfs) & monthname==input$dmonth) %>% 
      group_by(NMFSAREA,month,year) %>% 
      summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
      ungroup %>% 
      group_by(NMFSAREA) %>% 
      mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE))
    dat
  })
  
  output$monthly_nmfs_plot <- renderPlot({
    req(input$dnmfs)
    req(input$dmonth)
    dataset <- rxData()  # this is the subsetted data
    p <- ggplot(dataset, aes(x = year, y = tempanom, fill=factor(NMFSAREA))) +
      geom_bar(stat="identity",position="dodge") + 
      theme_bw() + 
      scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
      geom_hline(yintercept=c(-0.5,0.5),linetype=2) + 
      xlab("Year") + 
      ylab("Temperature Anomaly") + 
      ggtitle(paste(input$dmonth)) + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=13),
            axis.title = element_text(size=14),
            plot.title = element_text(size=16))
    print(p)
  })
})

shinyApp(ui, server)

#----------------------------------------------------
#  Multi-panel layout
#----------------------------------------------------

ui <- fluidPage(
  tabsetPanel(
    tabPanel("NMFS Seasonal Anomaly",
             titlePanel("Seasonal sea surface temperature anomaly"),
             p("Select NMFS reporting area(s) to view winter and summer temperature anomalies. October and November of each winter corresponds to the previous year. 2018 is omitted from summer anomalies because the data are incomplete.
               Depth filters remove statistical areas whose average depth is outside of the selected depth range. If the selected depth leads to a plotting error try a different depth range."),
    fluidRow(
      column(6,
             selectInput('dnmfs', 'NMFS area', mynmfs, multiple=TRUE)),
      column(6,
             sliderInput("range", "Depth:",min = -5000, max = 0,value = c(-200,0))
      ),
      plotOutput('seasonal_nmfs_plot')
    )),
    tabPanel("NMFS Monthly Anomaly",
             titlePanel("Monthly sea surface temperature anomaly"),
             p("Select NMFS reporting area(s) and month to view temperature anomalies for those areas and that month. Depth filters remove statistical areas whose average depth is outside of the selected depth range. If the selected depth leads to a plotting error try a different depth range."),
             fluidRow(
               column(4,
                      selectInput('dnmfs_month', 'NMFS area', mynmfs, multiple=TRUE)),
               column(4,
                      selectInput('dmonth', 'Month', month.name)),
               column(4,
                      sliderInput("range_month", "Depth:",min = -5000, max = 0,value = c(-200,0))
               )),
             plotOutput('monthly_nmfs_plot')
    )
  )
)


server <- shinyServer(function(input, output){
  ## values <- reactiveValues()  # unused
  ## Your data should be reactive - and reference `input` 
  ## to get user-entered values
  rxDataMonthly <- reactive({
    dat <- data %>% 
      filter(m.depth>=input$range_month[1] & m.depth<=input$range_month[2] & NMFSAREA%in%c(input$dnmfs_month) & monthname==input$dmonth) %>% 
      group_by(NMFSAREA,month,year) %>% 
      summarise(meantemp=mean(sst.mean,na.rm=TRUE)) %>% 
      ungroup %>% 
      group_by(NMFSAREA) %>% 
      mutate(tempanom=(meantemp-mean(meantemp,na.rm=TRUE))/sd(meantemp,na.rm=TRUE))
    dat
  })
  
  output$monthly_nmfs_plot <- renderPlot({
    req(input$dnmfs_month)
    req(input$dmonth)
    dataset <- rxDataMonthly()  # this is the subsetted data
    p <- ggplot(dataset, aes(x = year, y = tempanom, fill=factor(NMFSAREA))) +
      geom_bar(stat="identity",position="dodge") + 
      theme_bw() + 
      scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
      geom_hline(yintercept=c(-0.5,0.5),linetype=2) + 
      xlab("Year") + 
      ylab("Temperature Anomaly") + 
      ggtitle(paste(input$dmonth)) + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=13),
            axis.title = element_text(size=14),
            plot.title = element_text(size=16),
            strip.text = element_text(size=14)) + 
      scale_x_continuous(breaks = 2003:2018, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017",""))
    print(p)
  })
  
  rxDataSeasonal <- reactive({
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
  
  output$seasonal_nmfs_plot <- renderPlot({
    req(input$dnmfs)
    datasetSeasonal <- rxDataSeasonal()  # this is the subsetted data
    p <- ggplot(datasetSeasonal, aes(x = newyr, y = tempanom, fill=factor(NMFSAREA))) +
      geom_bar(stat="identity",position="dodge") + 
      facet_wrap(~season) + 
      theme_bw() + 
      scale_fill_viridis(name="NMFS Area",discrete=TRUE) + 
      geom_hline(yintercept=c(-0.5,0.5),linetype=2) + 
      xlab("Year") + 
      ylab("Temperature Anomaly") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=13),
            axis.title = element_text(size=14),
            strip.text = element_text(size=14)) + 
      scale_x_continuous(breaks = 2003:2018, labels = c("2003","","2005","","2007","","2009","","2011","","2013","","2015","","2017",""))
    print(p)
  })
})

shinyApp(ui, server)



