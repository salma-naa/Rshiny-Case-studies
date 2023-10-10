
## Load the package ----

library(shiny)
library(datasets)


## Create an UI object ----

ui <- fluidPage(
    
    # Give the page a title 
    titlePanel("Telephones by region"),
    
    # Generate a row with a panel
    sidebarLayout( position = "left" ,
        sidebarPanel(
            selectInput(inputId = "region", label = "Region" , choices = colnames(WorldPhones)),
            br(),
            helpText("Data from AT&T (1961) The World's Telephones.")
        ),
        
    # Create a main panel
    mainPanel(
        plotOutput("PhonePlot")
    )
    )
)


## Create a server object ---

server <- function(input, output) {
    output$PhonePlot <- renderPlot({
        
        #Render a barplot
        barplot(WorldPhones[, input$region] * 1000 ,
                main = input$region,
                xlab = "Years",
                ylab = "Number of phones")
    })
}


## Run the app ----

shinyApp(ui = ui , server = server)