library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)
library(thematic)
library(plotly) # Ajout de plotly

thematic_shiny()

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "quartz"
  ),
  titlePanel("Star War Characters"),
  sidebarLayout(
    sidebarPanel(
      h2("My app from scratch"),
      sliderInput(
        inputId = "taille",
        label = "Height of character",
        min = 0,
        max = 250,
        value = 30
      ),
      selectInput(
        inputId = "gender",
        label = "Sex of character",
        choices = c("masculine", "feminine", "other"),  
        selected = "masculine"
      ),
      actionButton(
        inputId = "bouton",
        label = "cliq"
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "StarWarPlot"), # Changer plotOutput en plotlyOutput
      DTOutput(
        outputId = "StarWarTable"
      )
    )
  )
)

server <- function(input, output) {
  
  # Déclaration des valeurs réactives
  rv <- reactiveValues(starwars_filter = starwars)
  
  # Filtrage dynamique selon input$taille et input$gender
  observeEvent(c(input$taille, input$gender), {
    rv$starwars_filter <- starwars %>%
      filter(height > input$taille) %>%
      filter(ifelse(input$gender == "", TRUE, gender == input$gender)) 
  })
  
  # Table interactive avec DT
  output$StarWarTable <- renderDT({
    req(rv$starwars_filter) 
    rv$starwars_filter
  })
  
  observe({
    message(paste("Nombre de lignes filtrées:", nrow(rv$starwars_filter)))
  })
  
  # Graphique interactif des tailles filtrées avec plotly
  output$StarWarPlot <- renderPlotly({
    req(rv$starwars_filter)
    
    # Créer un histogramme avec plot_ly
    p <- rv$starwars_filter %>%
      ggplot(aes(x = height)) + 
      geom_histogram(
        binwidth = 10,
        fill = "white",
        color = "white" 
      ) +
      labs(
        title = "Distribution des tailles sélectionnées"
      ) + 
      geom_density(color="red") + 
      xlim(0, 750) + geom_vline(aes(xintercept=mean(height)),
                  color="blue", linetype="dashed", size=1)
    
    
    # Convertir le ggplot en plotly
    plotly_graph <- ggplotly(p)
    
    # Ajouter une interaction avec clic sur les barres de l'histogramme
    plotly_graph %>% layout(
      clickmode = 'event+select'
    ) %>% event_register('plotly_click') # Enregistre l'événement de clic
    
    plotly_graph
  })
  
  # Écouter les clics sur le graphique
  observeEvent(input$StarWarPlot_click, {
    # Récupérer les données de la barre cliquée
    click_data <- input$StarWarPlot_click
    clicked_bin <- click_data$points[[1]]$x  # Position de la barre cliquée
    
    # Trouver la plage de tailles (bin) correspondante
    lower_bin <- floor(clicked_bin / 10) * 10
    upper_bin <- lower_bin + 10
    
    # Filtrer les personnages dans cette plage de tailles
    selected_data <- rv$starwars_filter %>%
      filter(height >= lower_bin & height < upper_bin)
    
    # Afficher le nombre d'individus dans cette plage
    showNotification(paste("Nombre d'individus dans cette tranche : ", nrow(selected_data)), type = "message")
  })
}

shinyApp(ui = ui, server = server)







