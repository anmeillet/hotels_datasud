# Server logic ----
server <- function(input, output,session) {
  
  df <- datasets[['Hotels']]
  
  # makeReactiveBinding('df')
  # 
  # observeEvent(input$dataset,{
  #   print('dataset')
  #   leafletProxy('map')%>%clearShapes()
  #   df <<- datasets[[input$dataset]]  
  #   i.active <<- NULL
  #   
  # })
  
  # choix des filtres ----
  # quantitatifs
  colQuanti <- names(which(unlist(lapply(df@data, is.numeric))))
  output$quanti <- renderUI(selectInput('quanti',label='Filtre quantitatif',choices = colQuanti,selected =  colQuanti[1]))
  
  quanti_ <- reactive({if (is.null (input$quanti)) return(colQuanti[1]) else return(input$quanti)})
  
  output$slider <- renderUI({
    sliderInput("slider", label = quanti_(),
                min = min(df@data[,quanti_()],na.rm = T),
                max = max(df@data[,quanti_()],na.rm = T),
                value = c(
                  min(df@data[,quanti_()],na.rm = T), 
                  max(df@data[,quanti_()],na.rm = T))
    )
  })
  
  # qualitatifs
  output$quali <- renderUI(selectInput('quali',label='Filtre qualitatif',
                                       choices = names(QualiChoices[[input$dataset]]),
                                       selected =  names(QualiChoices[[input$dataset]])[1])
                           )
  
  quali_ <- reactive({if (is.null (input$quali)) return(names(QualiChoices[[input$dataset]])[1]) else return(input$quali)})
  
  output$checkbox <- renderUI(checkboxGroupInput('checkbox',label=quali_(),
                                                 choices = QualiChoices[[input$dataset]][[quali_()]],
                                                 selected =  choices[[1]]))
  
  # checkboxGroupInput("Classement",
  #                    label = "Nombre d'étoiles",
  #                    choices = as.list(liste_classement),
  #                    selected = choices[[1]])
  
  
  filteredData <- reactive({
    df[df@data[,quanti_()] >= input$slider[1] & df@data[,quanti_()] <= input$slider[2] & df@data[,quali_()] %in% input$checkbox ,]
  })
  
  # filteredData <- reactive({
  #   df[df$Altitude >= input$Altitude[1] & df$Altitude <= input$Altitude[2] & df$Classement.HOT %in% input$Classement ,]
  # })
  
  # colorpal <- reactive({
  #   colorNumeric(input$colors, quakes$mag)
  # })
  
  # Create the map ----
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
    #dfFiltered <- df[df$Altitude >= input$Altitude[1] & df$Altitude <= input$Altitude[2] & df$Classement.HOT %in% input$Classement ,]
    nbHotelQuali<- aggregate(id~filteredData()@data[,quali_()],data = filteredData(), length)
    names(nbHotelQuali)<-c("classement","nb")

    amBarplot(x = "classement", y = "nb", data = nbHotelQuali, horiz = TRUE, export = TRUE, exportFormat = 'PNG')

    ## plot_ly(data = nbHotelClassement,x=~nb, y = ~classement, type = 'bar', orientation = 'h')
  })
  
  
}
