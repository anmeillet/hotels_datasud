#Page de visualisation des hotels datasud

#Check if packages used by app are installed, installs those not installed yet----
#list.of.packages <- c("shiny", "shinydashboard","shinyjs","leaflet","ggvis","dplyr","RColorBrewer","raster","gstat","rgdal","Cairo")
#new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
#if(length(new.packages)) install.packages(new.packages)

# load packages ----
require(shiny)
require(shinydashboard)
require(shinyjs)
require(leaflet)
require(ggvis)
require(dplyr)
library(RColorBrewer)
require(raster)
require(gstat)
require(rgdal)
require(Cairo)

#load data ----
# hotels <- read.csv("data/hotels-region-sud-apidae-reference.csv",stringsAsFactors=FALSE)
hotels <- read.csv("../data/hotels-region-sud-apidae-reference_nobooking.csv",stringsAsFactors=FALSE) #version sans une des url du site booking.com comprenant des ,

rgc <-read.csv("../data/rgc2011.csv",stringsAsFactors=FALSE, sep = ';') # source : https://public.opendatasoft.com/explore/dataset/rgc2011/table/

#prepare data ----
#suppression des colonnes avec des NA
hotels <- hotels[,apply(hotels, 2, function(x) { sum(!is.na(x)) > 0 })]
hotels$Longitude <- as.numeric(hotels$Longitude)
hotels$Latitude <- as.numeric(hotels$Latitude)
hotels$Altitude <- as.numeric(hotels$Altitude)

# hotels with no localisation and validated geo-referencing
#hotels[which(is.na(hotels$Latitude) & hotels$Géolocalisation.validée=="1"),]
# we keep only the hotels with validated geo referencing
# hotels <- hotels[which(hotels$Géolocalisation.validée=="1"),]

# on remplace les altitudes manquantes par l'altitude min (d'après le RGC de l'IGN)
rgc$codeInsee <- ifelse(nchar(rgc$COM)==1,
                        paste0(rgc$DEP,'00',rgc$COM),
                        ifelse(nchar(rgc$COM)==2,paste0(rgc$DEP,'0',rgc$COM),
                               paste0(rgc$DEP,rgc$COM))
                        )
hotels$Code.inse <- ifelse(nchar(hotels$Code.inse)<5,
                           paste0('0',hotels$Code.inse),
                           hotels$Code.inse)

hotels <-merge(hotels,rgc[,c("codeInsee","ZMIN")],by.x="Code.inse",by.y="codeInsee",all.x=T)

hotels$Altitude <- ifelse(is.na(hotels$Altitude),hotels$ZMIN,hotels$Altitude)
# on supprime les hotels qui n'ont pas d'altitude
hotels <- hotels[which(!(is.na(hotels$Altitude)) & hotels$Géolocalisation.validée=="1"),]
# on attribue l'altitude moyenne aux hotels qui n'ont pas d'altitude
#alt_moy <- mean(hotels$Altitude,na.rm=T)
#completion altitude (à ameliorer !!!)
#hotels[which(is.na(hotels$Altitude) & hotels$Géolocalisation.validée=="1"),"Altitude"] <- alt_moy

#table(hotels$Classement.HOT)
#on attribue un classement aux hotels sans info de classement
hotels[which(hotels$Classement.HOT=="" ),"Classement.HOT"] <- "pas de classement"

#simplify data
hotels <- hotels[,c("id","Nom","Commune","Classement.HOT","Altitude","Longitude","Latitude")]

# transform to SpatialPointDataFrame
coordinates(hotels) <- ~ Longitude + Latitude
proj4string(hotels) <- "+init=epsg:4326"

# test affichage carte
# library(rworldmap)
# newmap <- getMap(resolution = "low")
# plot(newmap, xlim = c(4, 7), ylim = c(42, 46), asp = 1)
# points(hotels, col = "red", cex = .01)


# create dataset and base layer ----
datasets <- list(
  'Hotels'=hotels
)

baselayers <- list(
  'Hotels'='DarkMatter (CartoDB)'
)


# User interface ----
ui <- bootstrapPage(
  
  dashboardPage(
  
  skin = 'blue',
  
  dashboardHeader(title = HTML('Datasud')),

  dashboardSidebar(
    selectInput(inputId = 'dataset',label = 'Données',
                choices = c('Hotels')),
    sliderInput(inputId = "Altitude", label = "Altitude",
                min = 0, max = 2200, value = c(0, 2200)),
    
    checkboxGroupInput("Classement", 
                       label = "Nombre d'étoiles", 
                       choices = setNames(as.list(
                           c(1:length(unique(hotels$Classement.HOT)))
                         ), unique(hotels$Classement.HOT)[order(unique(hotels$Classement.HOT))]
                         ),
                       selected = 1)),
  
  dashboardBody(
    box(width = 6,status = 'warning',
        div(style = "height: 450px;",
            ggvisOutput("p")))
        
    ),                  
    
    box(width = 6,status = 'warning',
        leafletOutput("map",height = 650)
    )
  )
)

# Server logic ----
server <- function(input, output,session) {
  
  output$map <- renderLeaflet({
    print('render map')
    leaflet() %>% #addTiles() %>% 
      addProviderTiles("CartoDB.DarkMatter", group = "DarkMatter (CartoDB)") %>%
      addProviderTiles("OpenStreetMap.Mapnik", group = "OpenStreetmap") %>%
      addLayersControl(baseGroups = c("OpenStreetmap",'DarkMatter (CartoDB)'),
                       options = layersControlOptions(collapsed = TRUE, autoZIndex = F))
  })
  
  df <- datasets[['Hotels']]
  makeReactiveBinding('df')
  
  # Respond to "event-like" reactive inputs, values, and expressions.
  observeEvent(input$dataset,{
    print('dataset')
    leafletProxy('map')%>%clearShapes()
    df <<- datasets[[input$dataset]]  
    i.active <<- NULL
    
  })
    
    coords <- reactive({
      print('coords')
      
      crds <- data.frame(coordinates(df))
      leafletProxy('map')%>%fitBounds(lng1=min(crds[,1]),lng2=max(crds[,1]),
                                      lat1=min(crds[,2]),lat2=max(crds[,2]))
      crds
      
    })
    
    #output$Altitude <- renderUI(selectInput('Altitude',label='Altitude',choices = names(df),selected =  names(df)[1]))
    #output$Classement <- renderUI(selectInput('Classement',label='Classement',choices = names(df),selected = names(df)[2]))
    

    
    ggvisdf <- reactive({
      print('ggvesdf1')
      df1 <- isolate(df@data)
      gdf <- df1[, c("Altitude", "Classement.HOT")]
      gdf
    })  
    
    observe({
      
      print('update map size/opa/color')
      x <- coords()[,1]
      y <- coords()[,2]
      leafletProxy('map')%>%
        addCircleMarkers(lng=x,fillColor = "#03F", #couleur en fonction des étoiles ?
                         lat=y,
                         stroke = F,
                         layerId = as.character(1:length(x)),
                         radius = 10,
                         fillOpacity = 1
        )
      
    })
    
   observe({
     print('legend')
     leafletProxy("map")%>%
       clearControls() # %>%
       #addLegend(opacity = 0.99,position = "bottomright",title = colorVar(),
       #          pal = colorpal(), values = rev(colorData()))
   })
    
    mapData <- reactive({
      print('mapdata')
      
      mb <- input$map_bounds
      
      if(is.null(mb))
        return(1)#as.vector(rep(1,nrow(coords()))))
      if(nrow(coords())!=nrow((ggvisdf())))
        return(1)
      
      as.numeric(coords()[,1]>mb$west&coords()[,1]<mb$east&
                   coords()[,2]>mb$south&coords()[,2]<mb$north)+0.1
      
    })
    
    tooltip <- function(x) {
      ggvisHover <<- x
      if(is.null(x)) return(NULL)
      tt<<-paste0(c("Altitude", "Classement.HOT"), ": ", format(x[1:2]), collapse = "<br/>")
      leafletProxy('map') %>%addControl(tt,layerId = 'tt',position = 'bottomleft')
      tt
    }
    
    
    # ggvisHover <- NULL
    # makeReactiveBinding('ggvisHover')
    # i.active <- NULL
    # makeReactiveBinding('i.active')
    
    
    # observeEvent(ggvisHover,{
    #   h <- ggvisHover[1:2]
    #   i.active <<- ggvisdf()[,'x']==h[[1]]&ggvisdf()[,'y']==h[[2]]
    # })
    
    
    # observeEvent(input$map_marker_mouseover,{
    #   id <- as.numeric(input$map_marker_mouseover$id)
    #   if(!is.na(id)){
    #     i.active <<- id
    #   }
    # })
    # 
    
    # observeEvent(i.active,{
    #   leafletProxy('map') %>%
    #     # removeMarker('hover') %>%
    #     addCircleMarkers(lat=coords()[i.active,2],opacity = 1,
    #                      fillOpacity = 0,
    #                      radius = 15,
    #                      lng=coords()[i.active,1],
    #                      layerId = 'hover',weight = 6,
    #                      color = 'red') 
    # })
    
    # mouseOver <- reactive({
    #   
    #   p <- ggvisdf()[i.active,c('x','y')]
    #   if(class(i.active)=='numeric'){tooltip(p)}
    #   p
    # })
    
    # ggvisdf %>% 
    #   ggvis(~x,~y) %>%
    #   set_options(width = "auto", height = "auto", resizable=FALSE) %>%    
    #   # add_axis("x", title = xVar())  %>% 
    #   add_tooltip(tooltip, "hover") %>%
    #   layer_points(data =mouseOver,stroke:='red',size := 150,fillOpacity=0,strokeWidth:=5) %>%
    #   bind_shiny("p")
    
  
}

# Run the app
shinyApp(ui, server)