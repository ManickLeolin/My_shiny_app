data(starwars)
head(starwars)  # Pour voir les 6 premières lignes du dataset
unique(starwars$height)  # Valeurs uniques dans la colonne "height"
unique(starwars$gender)  # Valeurs uniques dans la colonne "gender"
library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)
library(thematic)
library(plotly)
thematic_shiny()

ui <- fluidPage(
  theme = bs_theme(
    version = 5,
    bootswatch = "minty"
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
        label = "Click me"
      )
    ),
    mainPanel(
      plotOutput(outputId = "StarWarPlot"),
      DTOutput(outputId = "StarWarTable")
    )
  )
)

server <- function(input, output) {
  
  # Déclaration des valeurs réactives
  rv <- reactiveValues(starwars_filter = starwars)
  
  # Filtrage dynamique selon input$taille et input$gender
  observeEvent(c(input$taille, input$gender), {
    rv$starwars_filter <- starwars %>%
      filter(height > input$taille)  %>%
      filter(ifelse(input$gender == "other", TRUE, gender == input$gender)) # ✅ Corriger le filtre
  })
  
  # Affichage d'un texte statique
  output$starWarTitle <- renderText({
    "Coucou"
  })
  
  # Table interactive avec DT
  output$StarWarTable <- renderDT({
    req(rv$starwars_filter) # ✅ Empêche l'affichage si `rv$starwars_filter` est vide
    rv$starwars_filter
  })
  
  observe({
    message(paste("Nombre de lignes filtrées:", nrow(rv$starwars_filter)))
  })
  
  # Notification lors du clic sur un bouton (si bouton existe dans l'UI)
  observeEvent(input$bouton, {
    message("Vous avez cliqué") # ✅ Correction orthographique
  })
  
  # Notification lors du changement de `input$taille`
  observeEvent(input$taille, {
    showNotification("Vous avez changé la taille sélectionnée") 
  })
  
  # Graphique des tailles filtrées
  output$StarWarPlot <- renderPlot({
    req(rv$starwars_filter) 
    rv$starwars_filter |> 
      ggplot(aes(x = height)) + 
      geom_histogram(
        binwidth = 10,
        fill = "white",
        color = "black" 
      ) +
      labs(
        title = "Distribution des tailles sélectionnées" 
      )
  })
}

shinyApp(ui = ui, server = server)