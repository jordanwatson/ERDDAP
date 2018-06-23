library(shiny)
library(ggplot2)
library(viridis)

data <- readRDS("Data/mur_SST_stat6_all_columns.rds")

data <- data %>% mutate(newyr=ifelse(month<4,year-1,year),
                        month2=ifelse(month<4,month+12,month),
                        season=ifelse(month2%in%c(4:9),"Summer (Apr - Sept)","Winter (Oct - Mar)"),
                        GOA=ifelse(is.na(GOA),0,GOA),
                        cumwk=(year-2003)*52+week,
                        cummo=(year-2003)*12+month) 
         
ui <- fluidPage(
  titlePanel("Daily temperatures by ADFG statistical area"),
  p("Select ADFG statistical area to plot a daily temperature time series. Select multiple areas to compare time series."),
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
      p <- ggplot(rxData() %>% mutate(date=as.POSIXct(date)), aes(x = date, y = sst.mean,color=factor(STAT_AREA)))
      p <- p + 
        geom_line() + 
        theme_bw() + 
        xlab("Date") + 
        ylab("Temperature") + 
        scale_color_viridis(discrete=TRUE,guide=guide_legend("Statistical Area")) + 
        scale_x_datetime(date_breaks="1 year",date_labels="%Y") + 
        theme(legend.position="top",
              legend.text=element_text(size=14),
              legend.title=element_text(size=14),
              axis.text = element_text(size=13),
              axis.title = element_text(size=14))
      print(p)
    })
  })

shinyApp(ui, server)


#---------------------------------------------
#  Multi panel
#---------------------------------------------



ui <- fluidPage(
  tabsetPanel(
    tabPanel("Daily temperatures",
             titlePanel("Daily temperatures by ADFG statistical area"),
             p("Select ADFG statistical area to plot a daily temperature time series. Select multiple areas to compare time series."),
             fluidRow(
               column(4,
                      selectInput("dstat",label = "Choose statistical areas to compare",choices = data$STAT_AREA %>% unique %>% sort,multiple = TRUE)
               ),
               plotOutput('daily_plot')
             )
    ),
    tabPanel("Weekly temperatures",
             titlePanel("Weekly temperatures by ADFG statistical area"),
             p("Select ADFG statistical area to plot a weekly temperature time series (daily temperatures averaged by week). Select multiple areas to compare time series."),
             fluidRow(
               column(4,
                      selectInput("wstat",label = "Choose statistical areas to compare",choices = data$STAT_AREA %>% unique %>% sort,multiple = TRUE)
               ),
               plotOutput('weekly_plot')
             )
    ),
    tabPanel("Monthly temperatures",
             titlePanel("Monthly temperatures by ADFG statistical area"),
             p("Select ADFG statistical area to plot a monthly temperature time series (daily temperatures averaged by month). Select multiple areas to compare time series."),
             fluidRow(
               column(4,
                      selectInput("mstat",label = "Choose statistical areas to compare",choices = data$STAT_AREA %>% unique %>% sort,multiple = TRUE)
               ),
               plotOutput('monthly_plot')
             )
    )))


server <- shinyServer(function(input, output) {
 
  rxData_daily <- reactive({
    req(input$dstat)
    dat <- data %>% 
      filter(STAT_AREA%in%input$dstat)
    dat
  })
  
  output$daily_plot <- renderPlot({
    p <- ggplot(rxData_daily() %>% mutate(date=as.POSIXct(date)), aes(x = date, y = sst.mean,color=factor(STAT_AREA)))
    p <- p + 
      geom_line() + 
      theme_bw() + 
      xlab("Date") + 
      ylab("Temperature") + 
      scale_color_viridis(discrete=TRUE,guide=guide_legend("Statistical Area")) + 
      scale_x_datetime(date_breaks="1 year",date_labels="%Y") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=13),
            axis.title = element_text(size=14))
    print(p)
  })
  
  rxData_weekly <- reactive({
    req(input$wstat)
    dat <- data %>% 
      filter(STAT_AREA%in%input$wstat) %>% 
      group_by(cumwk,STAT_AREA) %>% 
      summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
                date=min(date),
                year=year[1])
    dat
  })
  
  output$weekly_plot <- renderPlot({
    p <- ggplot(rxData_weekly() %>% mutate(date=as.POSIXct(date)), aes(x = date, y = sst.mean,color=factor(STAT_AREA)))
    p <- p + 
      geom_line() + 
      theme_bw() + 
      xlab("Date") + 
      ylab("Temperature") + 
      scale_color_viridis(discrete=TRUE,guide=guide_legend("Statistical Area")) + 
      scale_x_datetime(date_breaks="1 year",date_labels="%Y") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=13),
            axis.title = element_text(size=14))
    print(p)
  })
  
  rxData_monthly<- reactive({
    req(input$mstat)
    dat <- data %>% 
      filter(STAT_AREA%in%input$mstat) %>% 
      group_by(cummo,STAT_AREA) %>% 
      summarise(sst.mean=mean(sst.mean,na.rm=TRUE),
                date=min(date),
                year=year[1])
    dat
  })
  
  output$monthly_plot <- renderPlot({
    p <- ggplot(rxData_monthly() %>% mutate(date=as.POSIXct(date)), aes(x = date, y = sst.mean,color=factor(STAT_AREA)))
    p <- p + 
      geom_line() + 
      theme_bw() + 
      xlab("Date") + 
      ylab("Temperature") + 
      scale_color_viridis(discrete=TRUE,guide=guide_legend("Statistical Area")) + 
      scale_x_datetime(date_breaks="1 year",date_labels="%Y") + 
      theme(legend.position="top",
            legend.text=element_text(size=14),
            legend.title=element_text(size=14),
            axis.text = element_text(size=13),
            axis.title = element_text(size=14))
    print(p)
  })
})

shinyApp(ui, server)
