# Load packages

library(shiny)
library(ggplot2)

# Get the data

Actual <- read.csv("~/General/New folder (3)/Material/YEAR 3/New Approach/Smart-City-Project/RealValues.csv")
Predictions <- read.csv("~/General/New folder (3)/Material/YEAR 3/New Approach/Smart-City-Project/Predictions.csv")

# Load data



# Define UI

ui <- fluidPage(
  
  titlePanel("Smart-City"),
  

  sidebarLayout(

    # Inputs: Select variables to plot
    sidebarPanel(
      
      
      # Select variable for y-axis
      selectInput(
        inputId = "feature",
        label = "Choose-feature",
        choices = c("Number of Bicycle Hire", 
                    "Bus Journeys", 
                    "Underground Journeys", 
                    "Overground Journeys", 
                    "CO-Pollution", 
                    "PM10-Pollution"),
        selected = "Number of Bicycle Hire"
      )
      ),
    

    # Output: Show matrix
    mainPanel(
      verbatimTextOutput("Confusion_Matrix"),
      
    )
)
  )


# Define server

server <- function(input, output, session) {
  
  Actual.input <- reactive({
    switch(input$feature,
           "Number of Bicycle Hire" = Actual$Number_of_Bicycle_Hires, 
           "Bus Journeys" = Actual$Bus_Journeys, 
           "Underground Journeys" = Actual$Underground_Journeys, 
           "Overground Journeys" = Actual$Overground_Jouneys, 
           "CO-Pollution" = Actual$CO_Pollution, 
           "PM10-Pollution" = Actual$PM10_Pollution)
  })
  
  Predicted.input <- reactive({
    switch(input$feature,
           "Number of Bicycle Hire" = Predictions$Number_of_Bicycle_Hires, 
           "Bus Journeys" = Predictions$Bus_Journeys, 
           "Underground Journeys" = Predictions$Underground_Journeys, 
           "Overground Journeys" = Predictions$Overground_Jouneys, 
           "CO-Pollution" = Predictions$CO_Pollution, 
           "PM10-Pollution" = Predictions$PM10_Pollution)
  })

  
  output$Confusion_Matrix <- renderPrint({
    confusionMatrix(data = factor(Predicted.input()), reference = factor(Actual.input()))
  })

  
}

# Create a Shiny app object

shinyApp(ui = ui, server = server)

