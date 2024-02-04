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
      #actionButton(
      #  inputId = "Correlation",
      #  label = "Show correlation graph of variables"
      #),
      #actionButton(
      #  inputId = "Confusion Matrix",
      #  label = "Show Confusion Matrix of variables"
      #)
    ),

    # Output: Show matrix
    mainPanel(
      #textOutput("Matrix")
      verbatimTextOutput("Confusion_Matrix"),
      
      plotOutput("Correlation_Graph")
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
  
  #output$Correlation_Graph <- renderPlot({
#    cbak <- colnames(data.final)
#    colnames(data.final) <- c("Cloud_Cov", "Sunshine", "Global_Rad", "max_temp", "mean_temp", "min_temp",
#                              "precipitation", "pressure", "Snow", "Bicycle No.", "CO", "NO", "NO2", "O3", "PM10",
#                              "SO2", "Bus", "UG", "DLR", "Tram", "OG", "Cable_car", "Rail")
#    cortable = cor(data.final)
#    corrplot(cortable, method = "circle")
#    colnames(data.final) <- cbak
#  })
}

# Create a Shiny app object

shinyApp(ui = ui, server = server)
