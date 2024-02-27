
library(shiny)
library(bnlearn)
cd <- read.csv("~/General/New folder (3)/Material/YEAR 3/New Approach/Datasets/CompleteData.csv")
#Actual <- read.csv("~/General/New folder (3)/Material/YEAR 3/New Approach/Smart-City-Project/RealValues.csv")
library(corrplot)
library(dplyr)
# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Conditional prediction"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
          selectInput(
            inputId = "event",
            label = "Select target variable",
            choices = c("Number.of.Bicycle.Hires",
                        "Bus.journeys..m.",
                        "Underground.journeys..m.",
                        "Tram.Journeys..m.",
                        "CO", 
                        "O3",
                        "PM10"),
            selected = "Number.of.Bicycle.Hires"
          ),
          numericInput(
            inputId = "cloud",
            label = "Enter Cloud Cover Value",
            value = 5
          ),
          numericInput(
            inputId = "sun",
            label = "Enter Sunshine Value",
            value = 5
          ),
          numericInput(
            inputId = "meantemp",
            label = "Enter Mean Temperature Value",
            value = 5
          ),
          numericInput(
            inputId = "bicycle",
            label = "Enter Bicycle usage Value",
            value = 5
          ),
          numericInput(
            inputId = "bus",
            label = "Enter Bus usage Value",
            value = 5
          ),
          numericInput(
            inputId = "tram",
            label = "Enter Tram usage Value",
            value = 5
          ),
          numericInput(
            inputId = "og",
            label = "Enter Overground usage Value",
            value = 5
          ),
          actionButton(
            inputId = "action",
            label = "Calculate expected value",
           ),
          actionButton(
            inputId = "corplot",
            label = "Plot Correlation graph of target feature"
          )
        ),
        
        
        # Output: Show matrix
        mainPanel(
          #textOutput("Matrix")
          textOutput("cp"),
          plotOutput("corrplot")
          
          #      plotOutput("Correlation_Graph")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  
  
  df <- eventReactive(input$action, {
    evlist = list(
      cloud_cover = input$cloud,
      sunshine = input$sun,
      mean_temp = input$meantemp,
      Number.of.Bicycle.Hires = input$bicycle,
      Bus.journeys..m. = input$bus,
      Tram.Journeys..m. = input$tram,
      Overground.Journeys..m. = input$og
    ) 
    if(input$event == "Number.of.Bicycle.Hires"){
      evlist = evlist[-4]
    } else if(input$event == "Bus.journeys..m."){
      evlist = evlist[-5]
    } else if(input$event == "Overground.Journeys..m."){
      evlist = evlist[-7]
    } else if(input$event == "Tram.Journeys..m."){
      evlist = evlist[-6]
    }else{
      evlist = evlist
    }

    sample <- cpdist(fitted, nodes = input$event, evlist, method = "lw", debug = TRUE, n=100)
    expected_output <- (sum(unlist(sample) * attr(sample, "weights")))/ sum(attr(sample, "weights"))
  })
  
  
  corplot <- eventReactive(input$corplot,{
    ylist <- select(cd,"cloud_cover", "sunshine", "mean_temp", "Number.of.Bicycle.Hires", 
                   "Bus.journeys..m.", "Tram.Journeys..m.", "Overground.Journeys..m.")
    graph <- cor(x = cd[input$event], y = ylist, use = "complete.obs")
    corrplot(graph, method = "circle", addgrid.col = T,type = 'upper', addCoef.col = T, number.cex = .7, diag = T , tl.cex = .9)
  })
    
  
  #print(Values)
  output$cp <- renderText({
    print(paste("The expected value of ", input$event, " given the selected variables is: ", df()))
  })
  output$corrplot <- renderPlot(
    corplot()
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
