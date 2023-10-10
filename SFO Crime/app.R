## Load the package ----

library(shiny)
library(chron)
library(ggplot2)
library(dplyr)

## Load the Data :: Data about SFO crime ----

load("SFO incident reports.rdata")
save(SFO_incident_report , file = "SFO_incident_report.rdata")

load("SFO_incident_report.rdata")

## Read the Data ---

head(SFO_incident_report)

summary(SFO_incident_report)

## Define UI for application : User Interface design ---

ui <- fluidPage(

    # Application title
    titlePanel("SFO City Crime Analysis"),
    
    # Establish space between these parameters
    
    br(),
    
    # Create a fluidRow instead of a sidebarLayout
    
    fluidRow(
        column(3, # 3 is the width
               dateRangeInput(inputId = "date_time",
                              label = "Choose Date Range",
                              start = min(SFO_incident_report$`Incident Date`),
                              end = max(SFO_incident_report$`Incident Date`))
               ),
        column(8,
               h3("SFO City Crime Dataset Analysis"),
               tabsetPanel(
                   tabPanel("Summary", 
                            h4(verbatimTextOutput("dim")), 
                            h4("Structure of Dataset"), 
                            verbatimTextOutput("Structure"), 
                            h4("Summary of dataset"), 
                            verbatimTextOutput("Summary"), 
                            h4("Crime Categories"), 
                            verbatimTextOutput("categoryTables")))
               )
        )
    )


## Build a server to showcase the UI

server <- function(input, output) {

    # Reactive data frame that will dynamically change based on the date range chosen
    
    crime_date <- reactive({
        
        #Subset data frame by date range
        
        SFO_incident_report <- SFO_incident_report[SFO_incident_report$`Incident Date` %in% seq.Date(input$date_time[1], input$date_time[2], by = "day")]
        SFO_incident_report
        
        })  
    
    ### Summary section ###
    
    # This will print the number of rows and columns of this dataset
    
    output$dim <- renderPrint({
        crime_data_sets <- paste("There are", nrow(SFO_incident_report()), "rows and ", ncol(SFO_incident_report()), "columns for SFO City Crime Dataset")
        print(crime_data_sets)
    })
    
    # This will print the structure of the dataset
    
    output$Structure <- renderPrint({
        str(SFO_incident_report())
    })
    
    # This will print the summary of the dataset
    
    output$Summary <- renderPrint({
        summary(SFO_incident_report())
    })
    
    # This will print crime categories
    
    output$categoryTables <- renderPrint({
        table(SFO_incident_report()$`Incident Category`)
    })
    
}

## Run the application ---
shinyApp(ui = ui, server = server)
