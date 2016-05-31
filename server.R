#Server File
#Does the actual work - will need to collect maps, and query data based on ui inputs.
library(pacman)
p_load("ggplot2","ggthemes","ggmap","maptools","rgdal", "dplyr")

shinyServer(
  function(input, output){
   
    dta <- reactive({
     DATALOAD <- input$datup
     data <- readRDS(DATALOAD$datapath)
     
     if(input$MapT == "Custom LA"){
       data <- filter(data, COUNCIL_AREA.NAME == as.character(input$CouncilChoice))
     }
##SO you need to have a data frame with summary stats already done on FTE, ActualSalary, and number of employees     
     dta_cln <- data %>%
       mutate_each(funs(niceCuts(., cuts = 8, thousands.separator = TRUE)), 
                 matches("FTE_mean|FTE_sum|FTESalary_mean|ActualSalary_mean|ActualSalary_sum|NoDistEmp_mean")) %>%
       ##calculate salary gaps between min and max
        #First round min and max values up to nearest 1000, so that we have a finite number of values
       mutate_each(funs(roundUp(., to = 1000)), matches("min|max")) %>%
       mutate(gapNetMinMax = FTESalary_max - FTESalary_min) %>%
       mutate(cutsGapNetMinMax = niceCuts(gapNetMinMax, cuts = 8, thousands.separator = TRUE)) %>%
       #keep only useful columns
       subset(., select = c("Datazone2001","FTE_mean","FTE_sum","FTESalary_mean","ActualSalary_mean","ActualSalary_sum","NoDistEmp_mean","cutsGapNetMinMax","COUNCIL_AREA.NAME")) %>%
       #merge with shapes
       left_join(y = shpsDzs01fort,
                 by = c("Datazone2001" = "id"))
     })
    
   
   #Add custom ui with list of Council areas to subset from
    output$LAArea <- renderUI({
      if(input$MapT == "Custom LA"){
        selectInput("CouncilChoice", label = "Select a Local Authority", choices = c("Aberdeen City", "Aberdeenshire", "Angus",
                                                            "Argyll & Bute", "Clackmannanshire","Dumfries & Galloway", "Dundee City",
                                                            "East Ayrshire", "East Dunbartonshire", "East Lothian","East Renfrewshire","Edinburgh, City of",
                                                            "Eilean Siar","Falkirk","Fife","Glasgow City", "Highland","Inverclyde","Midlothian", "Moray",
                                                            "North Ayrshire","North Lanarkshire","Orkney Islands","Perth & Kinross",
                                                            "Renfrewshire","Scottish Borders","Shetland Islands","South Ayrshire",
                                                            "South Lanarkshire", "Stirling","West Dunbartonshire", "West Lothian"))
      }
    })
    output$IndSelect <- renderUI({
      if(!is.null(input$datup)){
        selectInput("Ind", label = "Choose Indicator to Plot", as.list(names(dta())))
      }
    })
    
    #get maps
    backgrScot <- get_map("Scotland", zoom = 7, maptype = "roadmap",
                           source = "google")
    backgrScot <- ggmap(backgrScot)
    
    backgrLA <- reactive({
      CentreOfMap <- geocode(location = as.character(input$LAMap))
      backgrLA <- get_map(c(lon = CentreOfMap$lon,lat = CentreOfMap$lat), zoom = input$MZoom, 
              maptype = "roadmap", source = "google")
      backgrLA <- ggmap(backgrLA)})
    
    bgmap <- reactive({if(input$MapT == "Custom LA"){
      bgmap <- backgrLA()} else {
      bgmap <- backgrScot
      }
    })
    
    values <- reactiveValues(
      brush = NULL
    )
    observe({
      values$brush <- input$plotBrush
    })

   plotInput<- eventReactive(input$goButton, {
    dataset <- dta()
    bgmap <- bgmap()
    
    m <- bgmap + geom_polygon(data = dataset, aes_string(x = "long",
                          y = "lat", fill = input$Ind,
                          group = "group"), alpha = 0.9, colour = "black", size = 0.1)+
    scale_fill_brewer(name = as.character(input$LegendT), palette = "RdYlGn")+
    ggtitle(as.character(input$Title))+
    theme_map() +
    theme(plot.title = element_text(face = 'bold', size = 14),
          legend.background = element_rect(colour = 'black'),
          legend.justification = c(0,0),
          legend.position = c(as.numeric(input$LegX),as.numeric(input$LegY)),
          legend.title = element_text(face = "bold"),
          legend.key.size = unit(input$LegSize, "mm")) 
    if(!is.null(values$brush)){
     mup <- m + coord_fixed(ratio = input$rat,xlim = c(values$brush$xmax, values$brush$xmin), ylim = c(values$brush$ymax, values$brush$ymin))
    } else{mup <- m}
    return(mup)
  })
   plotInput2 <- function(){
     dataset <- dta()
     bgmap <- bgmap()
     
     m <- bgmap + geom_polygon(data = dataset, aes_string(x = "long",
                                                          y = "lat", fill = input$Ind,
                                                          group = "group"), alpha = 0.9, colour = "black", size = 0.1)+
       scale_fill_brewer(name = as.character(input$LegendT), palette = "RdYlGn")+
       ggtitle(as.character(input$Title))+
       theme_map() +
       theme(plot.title = element_text(face = 'bold', size = 14),
             legend.background = element_rect(colour = 'black'),
             legend.justification = c(0,0),
             legend.position = c(as.numeric(input$LegX),as.numeric(input$LegY)),
             legend.title = element_text(face = "bold"),
             legend.key.size = unit(input$LegSize, "mm")) 
     if(!is.null(values$brush)){
       mup <- m + coord_fixed(ratio = input$rat,xlim = c(values$brush$xmax, values$brush$xmin), ylim = c(values$brush$ymax, values$brush$ymin))
     } else{mup <- m}
     return(mup)
   }
   
   observeEvent(input$resetButton,{
     values$brush <- NULL
     runjs("document.getElementById('plotBrush_brush').remove()")
   })
    
    output$map <- renderPlot({
      return(plotInput())
    })
    
   output$downloadMap <- downloadHandler(
     filename = function() {paste0(input$dlNm, ".png")},
     content = function(file){
       ggsave(file, plot = plotInput2(), device = "png")
     }
   ) 
})