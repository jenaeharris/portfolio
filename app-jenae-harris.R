# Title: App 1: Storms Visualizer
# Description: Shiny app that visualizes tropical cyclones or "storms" in the North Atlantic
# Details: 
# Author: Jenae Harris
# Date: 10/26/23


# ======================================================
# Packages (you can use other packages if you want)
# ======================================================
library(shiny)
library(tidyverse)      # for syntactic manipulation of tables
library(sf)             # provides classes and functions for vector data
library(rnaturalearth)  # map data sets from Natural Earth


# ======================================================
# Auxiliary objects (that don't depend on input widgets)
# ======================================================
# world map data for ggplot()
world_countries = ne_countries(returnclass = "sf")

# map to be used as canvas (feel free to customize it)
atlantic_map = ggplot(data = world_countries) +
  geom_sf() +
  coord_sf(xlim = c(-110, 0), ylim = c(5, 65))


# You may need to add one or more auxiliary objects for your analysis
# (by "auxiliary" we mean anything that doesn't depend on input widgets)




# ===========================================================
# Define UI for graphing a map, and display data table
# ===========================================================
ui <- fluidPage(
  
  # Application title
  titlePanel("North Atlantic Tropical Cyclones"),
  
  # -------------------------------------------------------
  # Sidebar with input widgets 
  # -------------------------------------------------------
  sidebarLayout(
    sidebarPanel(
      # the following widgets are for template purposes,
      # please replace them with widgets of your choice
      sliderInput(inputId = "year",
                   label = "Year:",
                   sep = "",
                   min = 1975, 
                   max = 2021,
                   value = 2000),
      checkboxInput(inputId = "month_facet",
                    label = "Facet by Month",
                    value = FALSE),
      checkboxInput(inputId = "wind_speed", 
                    label = "Show Wind Speed",
                    value = FALSE),
      checkboxInput(inputId = "major_hurricanes",
                    label = "Show Major Hurricanes",
                    value = FALSE),
      sliderInput(inputId = "pressure",
                  label = "Pressure:",
                  min = 880,
                  max = 1025,
                  step = 5,
                  value = 1025)
      ), # closes sidebarPanel
    
    # -----------------------------------------------------------
    # Main Panel with outputs: plot map of storms, and show table
    # -----------------------------------------------------------
    mainPanel(
      plotOutput(outputId = "plot_map"),
      hr(),
      dataTableOutput(outputId = "summary_table")
    )
  ) # closes sidebarLayout
) # closes fluidPage


# ======================================================
# Server logic to graph the map, and obtain table
# ======================================================
server <- function(input, output) {
  
  # ------------------------------------------------------------
  # Reactive table of filtered storms
  # (adapt code to manipulate storms according to your analysis)
  # ------------------------------------------------------------
  tbl = reactive({
    # the following filter() is just for demo purposes
    # adapt code to manipulate storms according to your analysis
    tbl = storms |> filter(year == input$year, pressure <= input$pressure)
    
    
    if(input$major_hurricanes) {
      tbl = tbl %>% 
        filter(category >= 3)
    }
    return(tbl)
  })
  
  
  # ------------------------------------------------------------
  # Map of storms
  # (adapt code to make a map according to your analysis)
  # ------------------------------------------------------------
  output$plot_map <- renderPlot({
    
    # this is just starting code to get map for demo purposes
    # (yours will be more complex)
 p = atlantic_map
     if(input$month_facet) {
    p = p +
      geom_point(data = tbl(), aes(x = long, y = lat, color = name), alpha = 0.5) + 
     geom_path(data = tbl(), aes(x = long, y = lat, group = name), alpha = 0.5) +
       facet_wrap(.~month)
   } else if (input$wind_speed) {
     p = p +
       geom_point(data = tbl(), aes(x = long, y = lat, size = wind, color = name), alpha = 0.5) +
       geom_path(data = tbl(), aes(x = long, y = lat, group = name), alpha = 0.5)
  } else {
     p = p +
       geom_point(data = tbl(), aes(x = long, y = lat, color = name)) + 
       geom_path(data = tbl(), aes(x = long, y = lat, group = name, color = name)) +
       guides(alpha = "none")
   }
 p
   })

    # map output
  
  
  # ----------------------------------------------------------
  # Summary Data Table
  # Should contain at least 5 columns:
  # - name
  # - start date
  # - end date
  # - maximum wind
  # - minimum pressure
  # ----------------------------------------------------------
  output$summary_table <- renderDataTable({
    # the following is a dummy table;
    # adapt code to manipulate tbl() according to your analysis
    tbl() |>
       group_by(name) |>
      summarize(start_date = paste0(month[1], "-", day[1]), end_date = paste0(month[n()], "-", day[n()]), wind_max = max(wind), pressure_min = min(pressure))
  })
} # closes server


# Run the application 
shinyApp(ui = ui, server = server)
