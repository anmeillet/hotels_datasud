# Server logic ----
server <- function(input, output,session) {
  
  df <- datasets[['Hotels']]
  
  filteredData <- reactive({
    df[df$Altitude >= input$Altitude[1] & df$Altitude <= input$Altitude[2] & df$Classement.HOT %in% input$Classement ,]
  })
  
  # colorpal <- reactive({
  #   colorNumeric(input$colors, quakes$mag)
  # })
  
  # Create the map
  output$map <- renderLeaflet({
    print('render map')
    leaflet() %>% #addTiles() %>% 
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addLayersControl(baseGroups = c("OpenStreetmap"),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F)) %>%
      addMarkers(data = filteredData(),
                 layerId = ~id)
  })
  
  # Show a popup at the given location
  showHotelDetails <- function(id, lat, lng) {
    selectedId <- df[df$id == id,]
    content <- as.character(tagList(
      tags$strong(HTML(sprintf("%s, %s",a(selectedId$Nom, href = selectedId$url, target="_blank"), selectedId$Commune
      ))), tags$br(),
      sprintf("Téléphone: %s", selectedId$Telephone)
    ))
    leafletProxy("map") %>% addPopups(lng, lat, content, layerId = id)
  }
  
  # When map is clicked, show a popup with city info
  observe({
    leafletProxy(mapId = "map") %>% 
      clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showHotelDetails(event$id, event$lat, event$lng)
    })
  })
  
  
  output$barplot1 <- renderAmCharts({
    print('barplot')
    dfFiltered <- df[df$Altitude >= input$Altitude[1] & df$Altitude <= input$Altitude[2] & df$Classement.HOT %in% input$Classement ,]
    nbHotelClassement<- aggregate(dfFiltered$id~dfFiltered$Classement.HOT,data = dfFiltered, length)
    names(nbHotelClassement)<-c("classement","nb")
    
    amBarplot(x = "classement", y = "nb", data = nbHotelClassement, horiz = TRUE, export = TRUE, exportFormat = 'PNG') 
    
    #plot_ly(data = nbHotelClassement,x=~nb, y = ~classement, type = 'bar', orientation = 'h')
  })
  
  
}
