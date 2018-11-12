# User interface ----
ui <- bootstrapPage(
  
  dashboardPage(
    
    skin = 'blue',
    
    dashboardHeader(title = HTML('Datasud')),
    
    dashboardSidebar(
      selectInput(inputId = 'dataset',label = 'Données',
                  choices = as.vector(names(datasets)))
      ,
      uiOutput('quanti'),
      uiOutput('slider'),

      uiOutput('quali'),
      uiOutput('checkbox'),
      
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
        column(
          width=6,
               box(width = NULL,heigth = 450,status = 'info',
                   amChartsOutput(outputId = "barplot1")
               )
        ),
        
        column(
          width = 6,
          box(width = NULL,status = 'info',
              leafletOutput("map",height = 450)
          )
        )
      )
      
      
    )
  )
)