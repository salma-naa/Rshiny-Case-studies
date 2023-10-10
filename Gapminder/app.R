

## Load the package ----

library(shiny)
library(ggplot2)
library(leaflet)
library(dplyr)

## Load the data : : the Gapminder project to visualise how GDP per capita and life expectancy have changed over time ----
# (this contains more data than that included in the gapminder R package)
# Note that when we run a shiny app, the working directory is the app.R's location

library(gapminder)

gapminder <- readRDS("C:\\Users\\Naamane Salma\\Desktop\\Shiny case studies\\Gapminder exercice\\gapminder.rds")
source("C:\\Users\\Naamane Salma\\Desktop\\Shiny case studies\\Gapminder exercice\\workshopFunctions.R")

## Read the data ----

print(gapminder)

levels(gapminder$continent)
levels(gapminder$country)


# produceGapminderPlot expects a single year of data
# Using pipes to do this:
gapminder %>% 
    filter(year == 2008 ) %>% 
    filter(continent %in% c("Europe", "Africa")) %>% 
    produceGapminderPlot()

# We can return a tibble showing the richest country and its GDP per capita
# using the getRichestCountry() function:
gapminder %>%
    filter(year == 1800) %>% 
    getRichestCountry()


# When we are producing a plot with a historic trace for a country, we 
# need to create a data set with that country's historic data:
historicdata <- gapminder %>% 
    filter(year <= 2000) %>% 
    filter(country == "United Kingdom")

gapminder %>% 
    filter(year == 2000) %>% 
    produceGapminderPlot(makeSemiTransparent = TRUE) %>% 
    addHistoricPlot(historicdata)
 
## User Interface Design ----

ui <- fluidPage(
     # The title of the page
    titlePanel("Gapminder Plot"),
     # Create the sidebarlayout
    fluidRow(
        column(12,
               
               plotOutput("gapminderPlot", click = "plotClick"), # add the option click so we can view coordinations on any point we touch
               tableOutput("GraphData") ,
               verbatimTextOutput("ClickData")
               
               )
    ),
    
    fluidRow(
        column(6,
               sliderInput(inputId = "year", label = "Choose a year",
                               min = min(gapminder$year), 
                               max = max(gapminder$year), 
                               value = min(gapminder$year),
                               sep = "" , # Hide thousands , separator
                               step = 1,
                               animate = animationOptions(interval = 1250) )
               
               
               
               ),
        column(6,
               checkboxGroupInput(inputId = "continent", label = "Choose a continent",
                                  choices = levels(gapminder$continent),
                                  selected = levels(gapminder$continent))
               )
    )
    
     )


## Build a server to showcase the gapminder plot ----

server <- function(input , output) {
    
    # Create a reactive value to give us the name of the country nearest to the clicked point
    
    activeCountry <- reactiveVal()
    
    historicData <- reactive({
        gapminder %>% 
            filter(year <= input$year) %>% 
            filter(country == activeCountry())
    })
    
    # Update the value of the country when we detect a click point event
    
    observeEvent(input$plotClick,
                 {
                     nearCountry <- nearPoints(df = plotData(), coordinfo = input$plotClick, maxpoints = 1) #Retaining the click data in here
                     activeCountry (as.character(nearCountry$country)) #EXtract just the country name and assign it to this
                 })
    
    # Create a reactive value to store the country we select
    
    activeCountry <- reactiveVal(value = NA)
    
     plotData <- reactive( {
         gapminder %>% 
             filter(year== input$year) %>% 
             filter(continent %in% input$continent)
     }
     )   

     
     output$gapminderPlot <- renderPlot({
        #Generate the gapminder plot using a function in the workerfunction
            observe(print(input$year))
         if (length(activeCountry()) == 0){ #No country is selected
             plotData() %>% 
                 produceGapminderPlot()
             
         } else {
             plotData() %>% 
                 produceGapminderPlot(makeSemiTransparent = TRUE) %>% 
                 addHistoricPlot(historicData())
         }
             
    }
    )
     
     output$GraphData <- renderTable({
         plotData() %>% 
             getRichestCountry()
     })
     
     output$ClickData <- renderPrint({
         # nearPoints(df= plotData() , coordinfo = input$plotClick , maxpoints = 1 ) # gives a name to the coordinates of the point we want
         activeCountry()
     })
    
}


## Run the app ----
shinyApp(ui = ui , server = server)

## To showcase code with the app : runApp(display.mode = "showcase")
