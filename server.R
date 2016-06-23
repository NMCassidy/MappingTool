#Server File
#Does the actual work - will need to collect maps, and query data based on ui inputs.
library(pacman)
p_load("ggplot2","ggthemes","ggmap","maptools","rgdal", "dplyr","magrittr", "readxl")

shinyServer(
  function(input, output){
   
  dta1 <- reactive({
     DATALOAD <- input$datup
     if(is.null(DATALOAD))
       return(NULL)
     data <- file.rename(DATALOAD$datapath, paste(DATALOAD$datapath, ".xlsx", sep = ""))
     data <- read_excel(paste(DATALOAD$datapath, ".xlsx", sep = ""), sheet = 1, col_names = FALSE) %>%
       setNames(gsub(".", "", make.names(.[1,]), fixed = TRUE)) %>%
       slice(-1)
     })

  output$theData <- renderDataTable({
    return(dta1())
     })   
     
  dta2 <- eventReactive(input$RenameButton,{
         data <- dta1()
         data <- data %>% rename_(., .dots = setNames(input$FTEnm, "Fulltimeequivalent")) %>%
           rename_(., .dots = setNames(input$DZnm, "Datazone2001")) %>%
           rename_(., .dots = setNames(input$FTESlNm, "FTESalary")) %>%
           rename_(., .dots = setNames(input$actnm, "ActualSalary")) %>%
           rename_(., .dots = setNames(input$ref, "Ref"))
       })
       
     
  #This bit will be separate and not depend upon the rename button  
  dta <- reactive({
         data <- dta2()
         data <- mutate_each(data, funs(destring), matches("FTESalary|ActualSalary|Fulltimeequivalent")) %>%
         left_join(y = dtaGeoHigher, by = c("Datazone2001" = "DATAZONE"))
         
         if(input$MapT == "Custom LA"){
         data <- filter(data, COUNCIL_AREA.NAME == as.character(input$CouncilChoice))
       }
       if(!is.null(data)){dta_cln <- data %>%
         group_by(Datazone2001) %>%
         #Summary Statistics
         mutate(NoDistEmp = n_distinct(Ref)) %>%
         summarise_each(funs(min, max, mean, sum), 
                        matches("FTESalary|ActualSalary|Fulltimeequivalent|NoDistEmp")) %>%
         ungroup() %>%
         ##calculate net values for salary indicators
         mutate_each(funs(computeNetPay(.)), matches("FTESalary","ActualSalary")) %>%
         #round these values to 2 decimal places
         mutate_each(funs(round(., 2)), matches("FTESalary|ActualSalary")) %>%
       mutate_each(funs(niceCuts(., cuts = 8, thousands.separator = TRUE)), 
                 matches("Fulltimeequivalent_mean|Fulltimeequivalent_sum|FTESalary_mean|ActualSalary_mean|ActualSalary_sum|NoDistEmp_mean")) %>%
       ##calculate salary gaps between min and max
        #First round min and max values up to nearest 1000, so that we have a finite number of values
       mutate_each(funs(roundUp(., to = 1000)), matches("min|max")) %>%
       mutate(gapNetMinMax = FTESalary_max - FTESalary_min) %>%
       mutate(cutsGapNetMinMax = niceCuts(gapNetMinMax, cuts = 8, thousands.separator = TRUE)) %>%
       #merge with shapes
       left_join(y = shpsDzs01fort,
                 by = c("Datazone2001" = "id"))
       }
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
      if(!is.null(dta())){
        dta <- dta()
        selectInput("Ind", label = "Choose Indicator to Plot", as.list(names(dta)[grep("sum|mean|min|max", names(dta), ignore.case = TRUE)]))
      }
      })
      
    #get maps
  backgrScot <- get_map("Scotland", zoom = 6, maptype = "roadmap",
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
   if(is.numeric(dataset[[input$Ind]])){
    dataset[[input$Ind]] <- niceCuts(dataset[[input$Ind]], 10)
    }
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
       ggsave(file, plot = plotInput(), device = "png")
     }
   ) 
})