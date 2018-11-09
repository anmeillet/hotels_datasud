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
                         choices = as.list(liste_classement), 
                         selected = choices[[1]])
      
      # selectInput("colors", "Color Scheme",
      #             rownames(subset(brewer.pal.info, category %in% c("seq", "div")))
      # )
      
    ),
    
    dashboardBody(
      
      fluidRow(
      box(width = 12,status = 'warning',
          leafletOutput("map",height = 450)
      )),
      
      fluidRow(
      box(width = 8,status = 'warning',
          plotOutput("barplot1",height = 450)
      )
      )

    )
  )
)