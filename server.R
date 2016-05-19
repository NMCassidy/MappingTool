#Server File
#Does the actual work - will need to collect maps, and query data based on ui inputs.

shinyServer(
  function(input, output){
    output$LAChoose <- renderUI({
      if(input$MapT == "Custom LA"){
        textInput("LAMap", "Choose Centre of Map", value = "Livingston")}
    })
    
    output$ZoomChoose <- renderUI({
      if(input$MapT == "Custom LA"){
      sliderInput("MZoom", "Select Zoom", min = 5, max = 12, value = 9)} 
    })
       ##Need to include the ability to press a button to actually fetch the map for us
  }
)