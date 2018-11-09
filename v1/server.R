# Server logic ----
server <- function(input, output,session) {
  
  df <- datasets[['Hotels']]
  
  filteredData <- reactive({
    df[df$Altitude >= input$Altitude[1] & df$Altitude <= input$Altitude[2] & df$Classement.HOT %in% input$Classement ,]
  })

  colorpal <- reactive({
    colorNumeric(input$colors, quakes$mag)
  })
  
  # Create the map
  output$map <- renderLeaflet({
    print('render map')
    leaflet() %>% #addTiles() %>% 
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addLayersControl(baseGroups = c("OpenStreetmap"),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      addMarkers(data = filteredData())
  })
  
  # # A reactive expression that returns the set of zips that are
  # # in bounds right now
  # HotelsInBounds <- reactive({
  #   if (is.null(input$map_bounds))
  #     return(hoteldata[FALSE,])
  #   bounds <- input$map_bounds
  #   latRng <- range(bounds$north, bounds$south)
  #   lngRng <- range(bounds$east, bounds$west)
  #   
  #   subset(hoteldata,
  #          latitude >= latRng[1] & latitude <= latRng[2] &
  #            longitude >= lngRng[1] & longitude <= lngRng[2])
  # })
  
  # Show a popup at the given location
  showHotelDetails <- function(id, lat, lng) {
    selectedId <- df[df$id == id,]
    content <- as.character(tagList(
      tags$strong(HTML(sprintf("%s, %s",selectedId$Nom, selectedId$Commune
      ))), tags$br(),
      sprintf("Téléphone: %s", selectedId$telepgone)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = hotels)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy("map") %>% clearPopups()
    event <- input$map_shape_click
    if (is.null(event))
      return()
    
    isolate({
      showHotelDetails(event$id, event$lat, event$lng)
    })
  })
  
  
  output$barplot1 <- renderPlot({
    print('barplot')
    dfFiltered <- df[df$Altitude >= input$Altitude[1] & df$Altitude <= input$Altitude[2] & df$Classement.HOT %in% input$Classement ,]
    nbHotelClassement<- aggregate(dfFiltered$id~dfFiltered$Classement.HOT,data = dfFiltered, length)
    names(nbHotelClassement)<-c("classement","nb")
    barplot(height = nbHotelClassement$nb, names.arg = nbHotelClassement$classement, las = 2 )
    

  }
    
  )
  
  
}
