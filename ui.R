#UI file
#Will contain the actual user interface - basically write the map name, choose background map, 
#move coord_fixed, move legend, name legend, choose indicator
library(shiny)
library(shinyjs)
library(shinydashboard)

shinyUI(dashboardPage(dashboardHeader(title = "Basic Map Making Tool - Do Not Publish!!!"),
  dashboardSidebar(
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
    fluidRow(column(6, numericInput("LegSize", "Legend Size", value = "6", min = 1, max = 10, step = 0.5)),
    column(6,actionButton("goButton", "Update Map"))
    ),
    fluidRow(column(6, numericInput("rat", "Coordinate Ratio", value = 1.6, min = 0.01, max = 3, step = 0.1)),
             column(6, actionButton("resetButton", "Reset Zoom"))),
    fluidRow(column(6, textInput("dlNm", "Type File Name", value ="")),
             column(6, downloadButton("dl map", "Download Map")))),
  dashboardBody(useShinyjs(),
                h1("Map Output"),
            plotOutput("map", brush = "plotBrush", width = 800, height = 800))
))