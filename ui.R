#UI file
#Will contain the actual user interface - basically write the map name, choose background map, 
#move coord_fixed, move legend, name legend, choose indicator
library(shiny)
shinyUI(pageWithSidebar(
  titlePanel("Basic Map Making Tool - Do Not Publish!!!"),
  sidebarPanel(
    #Choose a data set to be used for the map making process
    fileInput("datup", "Choose Dataset to Read", accept = c(".rds")),
    textInput("Title", label = "Enter Map Title", value = ""),
    selectInput("MapT", label = "Choose Map Type", choices = c("Scotland", "Custom LA")),
    uiOutput("LAArea"),
    uiOutput("IndSelect"),
    uiOutput("CentreChoose"),
    uiOutput("ZoomChoose"),
    textInput("LegendT", label = "Enter Legend Title", value = ""),
    fluidRow(column(6, numericInput("LegX", "Legend Position X-axis", value = "0", min = 0, max = 1.1, step = 0.05)),
             column(6, numericInput("LegY", "Legend Position Y-axis", value = "0", min = 0, max = 1.1, step = 0.05))),
    fluidRow(column(6, numericInput("LimX1", "Left Side X Limit", value = "", min = -7.8, max = -0.6, step =0.01)),
             column(6, numericInput("LimX2", "Right Side X Limit", value = "", min = -7.8, max = -0.6, step =0.01))),
    fluidRow(column(6, numericInput("LimY1", "Bottom Side Y Limit", value = "", min = 54.55, max = 61, step =0.01)),
             column(6, numericInput("LimY2", "Top Side Y Limit", value = "", min = 54.55, max = 61, step =0.01))),
    fluidRow(column(6, numericInput("LegSize", "Legend Size", value = "6", min = 1, max = 10, step = 0.5)),
    column(6,actionButton("goButton", "Update Map"))
    )),
  mainPanel(h1("Map Output"),
            plotOutput("map"))
))