library(shiny)
library(dplyr)
library(ggplot2)
library(DT)
library(bslib)
library(thematic)
library(plotly)

data("diamonds")

thematic_shiny()

ui <- fluidPage(
  theme = bs_theme(version = 5, bootswatch = "quartz"),
  titlePanel("Exploration des Diamants"),
  sidebarLayout(
    sidebarPanel(
      h2("Filtres"),
      sliderInput(
        inputId = "Price",
        label = "Prix des Diamants",
        min = min(diamonds$price),
        max = max(diamonds$price),
        value = c(min(diamonds$price),5000),
        step = 100,
        pre = "$",  # Affichage en format monétaire
        sep = ","
      ),
      selectInput(
        inputId = "color",
        label = "Choisir une couleur",
        choices = c("Toutes",sort(unique(as.character(diamonds$color)))), 
        selected = "Toutes"
      )
    ),
    mainPanel(
      plotlyOutput(outputId = "DiamondPlot"), 
      DTOutput(outputId = "DiamondTable")
    )
  )
)


server <- function(input, output, session) {
  
  # Vérifier ce que contient la colonne "color"
  observe({
    print(unique(diamonds$color))  # Debugging pour voir les vraies valeurs
  })
  # Notification lors du changement de filtre
  observeEvent(input$Price, {
    showNotification(
      paste("Filtrage par prix :", input$Price[1], "à", input$Price[2]),
      type = "message",
      duration = 3
    )
  })
  
  observeEvent(input$color, {
    showNotification(
      paste("Couleur sélectionnée :", input$color),
      type = "message",
      duration = 3
    )
  })
  
  
  # Filtrer les données en fonction du prix et de la couleur
  filtered_data <- reactive({
    data <- diamonds %>%
      filter(price >= input$Price[1], price <= input$Price[2])
    
    if (input$color != "Toutes") {
      data <- data %>% filter(color == input$color)  
    }
    
    return(data)
  })
  
  # Table interactive
  output$DiamondTable <- renderDT({
    req(filtered_data()) 
    datatable(filtered_data())
  })
  
  # Graphique interactif
  output$DiamondPlot <- renderPlotly({
    req(filtered_data())
    
    p <- ggplot(filtered_data(), aes(x = carat, y = price)) +
      geom_point(alpha = 0.7, color = "blue") +  
      labs(
        title = paste("Diamants - Couleur:", input$color),
        x = "Carats",
        y = "Prix ($)"
      ) +
      theme_minimal()
    
    ggplotly(p)
  })
}

shinyApp(ui = ui, server = server)
