#UI file
#Will contain the actual user interface - basically write the map name, choose background map, 
#move coord_fixed, move legend, name legend, choose indicator
library(shiny)
shinyUI(pageWithSidebar(
  titlePanel("Basic Map Making Tool - Do Not Publish!!!"),
  sidebarPanel(
    textInput("Title", label = "Enter Map Title", value = ""),
    selectInput("MapT", label = "Choose Map Type", choices = c("Scotland", "Custom LA")),
    uiOutput("LAChoose"),
    uiOutput("ZoomChoose")
  ),
  mainPanel()
))