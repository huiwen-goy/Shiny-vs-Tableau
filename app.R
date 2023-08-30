
# Read and prep data-------
bike_data <- read.csv("VTech_bike_helmets.csv", header=TRUE)

bike_data$Manufacturer <- sub(" .*", "", bike_data$Helmet) #extract manufacturer names as variable

bike_data$Manufacturer[bike_data$Manufacturer == "AIRNOGGIN"] <- "Airnoggin" #fix some names
bike_data$Manufacturer[bike_data$Manufacturer == "Sweet"] <- "Sweet Protection"
bike_data$Manufacturer[bike_data$Manufacturer == "Troy"] <- "Troy Lee Designs"
bike_data$Manufacturer[bike_data$Manufacturer == "Louis"] <- "Louis Garneau"
bike_data$Manufacturer[bike_data$Manufacturer == "100"] <- "100 Percent"

manu_list <- sort(unique(bike_data$Manufacturer), decreasing=FALSE)

# Load libraries-------
library(DT)
library(shiny)
library(shinydashboard)
library(shinyWidgets)

# Define user interface-------

ui <- dashboardPage(
  
  dashboardHeader(title = "VTech bike helmet data"), 
  title = "VTech bike helmet data",
  
  dashboardSidebar(
    
    pickerInput(inputId = "manu_select",
                label = "Select manufacturers", 
                choices = manu_list,
                multiple = TRUE,
                selected = manu_list,
                options = pickerOptions(actionsBox = TRUE, 
                                        noneSelectedText = "click to select", 
                                        liveSearch = TRUE, 
                                        liveSearchPlaceholder = "type to search", 
                                        deselectAllText = "Select None", 
                                        selectAllText = "Select All"), 
                choicesOpt = list(style = rep(("color: black; background: white;"), length(manu_list))) ),

    awesomeCheckboxGroup(inputId = "star_check",
                         label = "Star rating (higher is better)", 
                         choices = c(1:5),
                         selected = c(1:5),
                         inline = TRUE, 
                         status = "warning"),
        
        sliderInput(inputId = "score_slide", 
                    label="Helmet score (lower is better)", 
                    min=8, 
                    max=26, 
                    round=FALSE, 
                    value=c(8.55, 25.27),
                    step=NULL,
                    ticks=TRUE), 
        
        sliderInput(inputId="cost_slide", 
                    label="Approx cost", 
                    min=5, 
                    max=350, 
                    value=c(8, 350),
                    step=NULL,
                    ticks=TRUE), 
    
    setSliderColor(c("#ffa64d", "#ffa64d"), c(1, 2)),
    
   width = 300 #sidebar width

  ), #end sidebar

  dashboardBody( box(dataTableOutput('helmet_table'), width = 12) ) #end dashboardBody 
  
) #end dashboardPage

# Define server-------
server <- function(input, output) {
  
  selected_data <- reactive({ subset( bike_data,
                                      (Manufacturer %in% input$manu_select) & 
                                      (Stars %in% input$star_check) & 
                                      (Score >= input$score_slide[1]) &
                                      (Score <= input$score_slide[2]) &
                                      (Cost >= input$cost_slide[1]) &
                                      (Cost <= input$cost_slide[2]) ) })
  
  output$helmet_table <- DT::renderDataTable( { subset(selected_data(),
                                                       select = c(Helmet, Stars, Score, Cost)) } )
  
}  #end server


# Run app-------
shinyApp(ui = ui, server = server)




