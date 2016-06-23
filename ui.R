#UI file
#Will contain the actual user interface - basically write the map name, choose background map, 
#move coord_fixed, move legend, name legend, choose indicator
library(shiny)
library(shinyjs)
library(shinydashboard)

shinyUI(navbarPage("Basic Map Making Tool - Do Not Publish!!!",
  tabPanel("Data Source and Tidy",
  sidebarPanel(
    #Choose a data set to be used for the map making process
    fileInput("datup", "Choose Dataset to Read", accept = c(".xlsx")),
                     h3("Rename Columns"), h4 ("Type in the names of the columns to be changed"),
          fluidRow(column(6, textInput("FTEnm", "FTE Column", value = "Fulltimeequivalent")),
                   column(6, textInput("DZnm", "Data Zone Col", value = "Datazone2001"))),
          fluidRow(column(6, textInput("FTESlNm", "FTE Sal", value = "FTESalary")),
                   column(6, textInput("actnm", "Actual Salary", value = "ActualSalary"))),
          fluidRow(column(6, textInput("ref", "Employee Ref No.", value = "EmployeeIdentifier")),
                   column(6, actionButton("RenameButton", "Data Clean"))),
  tags$style(type='text/css', "#RenameButton { margin-top: 44px; }")),
  mainPanel("Data Explorer", dataTableOutput("theData"))
  ),
  
  tabPanel("Map",
    sidebarPanel(textInput("Title", label = "Enter Map Title", value = ""),
    selectInput("MapT", label = "Choose Map Type", choices = c("Scotland", "Custom LA"), selected = "Scotland"),
    uiOutput("LAArea"),
    uiOutput("IndSelect"),
    conditionalPanel("input.MapT == 'Custom LA'",
      textInput("LAMap", "Choose Centre of Map", value = "Livingston"),
      sliderInput("MZoom", "Select Zoom", min = 5, max = 12, value = 9)),
    textInput("LegendT", label = "Enter Legend Title", value = ""),
    fluidRow(column(6, numericInput("LegX", "Legend Position X-axis", value = "0", min = 0, max = 1.1, step = 0.05)),
             column(6, numericInput("LegY", "Legend Position Y-axis", value = "0", min = 0, max = 1.1, step = 0.05))),
    fluidRow(column(6, numericInput("LegSize", "Legend Size", value = "6", min = 1, max = 10, step = 0.5)),
             column(6, numericInput("rat", "Coordinate Ratio", value = 1.6, min = 0.01, max = 3, step = 0.1))),
    fluidRow(column(6,actionButton("goButton", "Update Map")),
             column(6,actionButton("resetButton", "Reset Zoom"))),
    fluidRow(column(6, textInput("dlNm", "Type File Name", value ="")),
             column(6, h4("Download Map"),downloadButton("downloadMap", "Save")))),
  
    mainPanel(useShinyjs(),
                h1("Map Output"),
       plotOutput("map", brush = "plotBrush", width = 800, height = 800)))
))