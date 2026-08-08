if (!require(shiny)) install.packages('shiny')
if (!require(leaflet)) install.packages('leaflet')
if (!require(sf)) install.packages('sf')

library(shiny)
library(leaflet)
library(sf)

efficient <- readRDS("data/results.RDS")$efficient
fair <- readRDS("data/results.RDS")$fair
baseline <- readRDS("data/results.RDS")$baseline

ui <- fluidPage(
  titlePanel("Chicago Restaurant Inspection Dashboard"),
  
  fluidRow(
    column(
      width = 12,
      radioButtons(
        inputId = "method",
        label = "Choose a targeting method for next 730 inspections:",
        choices = c("Efficient" = "efficient", "Fair" = "fair"),
        selected = "efficient",
        inline = TRUE
      )
    )
  ),
  
  fluidRow(
    column(
      width = 12,
      leafletOutput("map", height = 500)
    )
  ),
  
  br(),
  
  fluidRow(
    column(
      width = 6,
      wellPanel(
        h4("Efficiency"),
        textOutput("efficiency_text"),
        textOutput("efficiency_context")
      )
    ),
    column(
      width = 6,
      wellPanel(
        h4("Geographic Fairness"),
        textOutput("fairness_text"),
        textOutput("fairness_context")
      )
    )
  )
)

server <- function(input, output, session) {
  
  selected_data <- reactive({
    if (input$method == "efficient") efficient else fair})
  
  comparison_data <- reactive({
    if (input$method == "efficient") fair else efficient})
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -87.6298, lat = 41.8781, zoom = 10)
  })
  
  observe({
    sel <- selected_data()
    marker_color <- if (input$method == "efficient") "firebrick" else "navy"

    
    popup_text <- paste0(
      "<b>", sel$dba_name, "</b><br/>",
      "Address: ", sel$address, "<br/>",
      "Community Area: ", sel$community, "<br/>",
      "Facility Group: ", sel$facility_group, "<br/>",
      "Predicted Fail Risk: ", round(sel$fail_risk, 3)
    )
    
    leafletProxy("map", data = sel) %>%
      clearMarkers() %>%
      addCircleMarkers(
        data = sel,
        popup = popup_text,
        label = sel$dba_name,
        radius = 5,
        stroke = FALSE,
        fillOpacity = 0.7,
        color = marker_color,
        fillColor = marker_color
      )
  })
  
  output$efficiency_text <- renderText({
    sel <- selected_data()
    lift <- mean(sel$fail_risk)/baseline
    paste0(
      "Lift = ",
      round(lift, 2),
      "x the citywide average predicted failure risk."
    )
  })
  
  output$efficiency_context <- renderText({
    comp <- comparison_data()
    lift_comp <- mean(comp$fail_risk)/baseline
    paste0(
      "Random targeting would be about 1.00x. ",
      "The alternative method scores ",
      round(lift_comp, 2),
      "x."
    )
  })
  
  output$fairness_text <- renderText({
    sel <- selected_data()
    balance <- 1 - sum(abs(tapply(sel$fail_risk,sel$community,length)-10))/(730*2)
    
    paste0(
      "Fairness score = ",
      round(balance, 3),
      " on a 0 to 1 scale, where higher means a more even distribution across community areas."
    )
  })
  
  output$fairness_context <- renderText({
    comp <- comparison_data()
    balance_comp <- 1 - sum(abs(tapply(comp$fail_risk,comp$community,length)-10))/(730*2)
    
    paste0(
      "A perfectly even allocation across community areas would score 1.000. ",
      "The alternative method scores ",
      round(balance_comp, 3),
      "."
    )
  })
}

shinyApp(ui, server)
