library(shiny)

# Define UI for miles per gallon app ----
ui <- fluidPage(
  
  # App title ----
  titlePanel("Miles Per Gallon"),
  
  # Sidebar panel for inputs ----
  sidebarLayout(
    sidebarPanel(
    
    selectInput("variable","Variable:",
                c("Cylinders"="cyl",
                  "Transmission"="am",
                  "Gears"="gear")),
    
    checkboxInput("outliers","Show outliers",TRUE)
  ),
  
  # Main panel for displaying outputs ----
  mainPanel(
    
    h3(textOutput("caption")),
    
    plotOutput("mpgPlot")
  )
  )
)

mpgData <- mtcars
mpgData$am <- factor(mpgData$am,labels=c("Automatic","Manual"))

# Define server logic to plot various variables against mpg ----
server <- function(input, output) {
  
  formulaText <- reactive({
    paste("mpg~",input$variable)
  })
  
  output$caption <- renderText({
    formulaText()
  })
  
  output$mpgPlot <- renderPlot({
    boxplot(as.formula(formulaText()),
            data=mpgData,
            outline=input$outliers,
            col="#75AADB",pch=19)
  })
}


shinyApp(ui, server)


