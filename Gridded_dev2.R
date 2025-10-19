# Load required packages
setwd("D:/Shiny-Dashboard")

library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(ncdf4)
library(sp)
library(tidyverse)
library(plotly)
library(bslib)
library(leaflet.extras)

########## Data Processing: Gridded Data ##########
# Loading Processed Data + Resulting CP Analysis
load("D:/Shiny-Dashboard/annual_Berkeley_anom.RData")
load('D:/Shiny-Dashboard/ResultsBerkeley.RData')

# Rename temperature and time data
time_data <- time[121:174]
temperature_data <- tas_annual[,,121:174]

# Reshape the temperature data to a matrix
temperature_matrix <- matrix(temperature_data, 
                             nrow = dim(temperature_data)[1] * dim(temperature_data)[2], 
                             ncol = dim(temperature_data)[3])

# Combine temperature and time data into a data frame
combined_data <- cbind(expand.grid(lon = 1:dim(temperature_data)[1], 
                                   lat = 1:dim(temperature_data)[2]), 
                       time = rep(time_data, each = dim(temperature_data)[1] * dim(temperature_data)[2]), 
                       temperature = as.vector(temperature_matrix))

# Add the changepoint fit to the combined data frame
combined_data$fittrend <- as.vector(fittrend)

# Remove rows with NaN values
combined_data <- combined_data[complete.cases(combined_data), ]

# Convert longitude from 1-360 to -180 to 180
combined_data$lon <- combined_data$lon - 180
combined_data$lon[combined_data$lon > 180] <- combined_data$lon[combined_data$lon > 180] - 360

# Convert latitude from 1-180 to -90 to 90
combined_data$lat <- combined_data$lat - 90

########## Data Processing: Country Data ##########
# USA_data = read.table("united-states-of-america-TAVG-monthly.txt", skip = 96)
# colnames(USA_data)[colnames(USA_data) == "V1"] <- "Year"
# colnames(USA_data)[colnames(USA_data) == "V2"] <- "Month"
# colnames(USA_data)[colnames(USA_data) == "V3"] <- "Monthly_Anomaly"

########## App ##########

# Define UI
ui <- navbarPage(
  title = "Temperature Monitoring",
  theme = bs_theme(
    version = 5,                 
    bootswatch = NULL,        
    bg = "#f8f9fa",            
    fg = "#1e1e1e",             
    primary = "#89CFF0",
    base_font = font_google("Roboto"),       
    heading_font = font_google("Inter") 
  ),
  header = tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "styles.css")
  ),
  
  tabPanel("Overview",
           #hero
           fluidRow(
             column(width = 12, class = "intro",
                    h1(class = "o-title", "Global Temperature Trends Dashboard"),
                    h3(class = "o-sub", "Explore Global Temperature Anomalies"),
                    p(class = "o-para", "Welcome to this monitoring dashboard for trends in global surface temperature. Using gridded data and known data analysis techniques, we will make an interactive 
               dashboard that will be used for climate outreach. The techniques used involve fitting a changepoint model to each grid of data, revealing whether a 
               specific lat/lon grid underwent a 'surge' in surface temperature. The data used was gathered from Berkeley Earth following a time period of (1850-2023).")
             )
           ),
           
           #info
           fluidRow(
             class="info-cards-row",
             card(
               class = "about",
               card_header(icon("globe"), "About This Dashboard"),
               card_body(
                 p("This interactive dashboard enables exploration of global surface temperature changes using high-resolution gridded data from Berkeley Earth."),
                 hr(),
                 h5("Key Features:"),
                 tags$ul(
                   tags$li("Interactive map-based exploration"),
                   tags$li("Changepoint detection analysis"),
                   tags$li("Location-specific time series"),
                   tags$li("Hemisphere and global averages"),
                   tags$li("Downloadable data and visualizations")
                 )
               )
             ),
             card(
               class = "methods",
               card_header(icon("chart-line"), "Methodology"),
               card_body(
                 tags$dl(
                   tags$dt("Data Source:"),
                   tags$dd("Berkeley Earth Surface Temperature"),
                   tags$dt("Time Period:"),
                   tags$dd("1850-2023 (174 years)"),
                   tags$dt("Spatial Resolution:"),
                   tags$dd("1° × 1° global grid (64,800 cells)"),
                   tags$dt("Analysis Method:"),
                   tags$dd("Changepoint detection models identify shifts in temperature trends at each location"),
                   tags$dt("Metric:"),
                   tags$dd("Temperature anomalies (°C deviation from baseline)")
                 )
               )
             ),
             card(
               class = "usage",
               card_header(icon("question-circle"), "User Guide"),
               card_body(
                 tags$ul(
                   tags$li("Navigate to the ", tags$strong("Maps"), " tab"),
                   tags$li("Click any location on the map or use the search bar"),
                   tags$li("View temperature time series with fitted trends"),
                   tags$li("Explore changepoint locations and warming rates"),
                   tags$li("Download data for selected locations"),
                   tags$li("Compare regional and global trends")
                 )
               )
             )
           )
        ),
  
  tabPanel("Maps", 
           fluidRow(
             column(width = 6, class = "map-col",
                    card(
                      class="map-card",
                      full_screen = TRUE,
                      card_header(
                        "Interactive Maps",
                        # actionButton("reset_map", "Reset View", size = "sm", class = "btn-outline-secondary")
                      ),
                      
                      card_body(
                        div(class="map-body",
                          tabsetPanel(
                            tabPanel("Map", leafletOutput("map", height = "500px")),
                            tabPanel("Country Map", leafletOutput("map2", height = "500px"))
                          )
                        )
                      )
                    )
             ),
             
             column(width = 6, class="time-col",
                    card(
                      class="time-card",
                      full_screen= TRUE,
                      card_header(class="time-header", "Temperature Anomaly Time Series"),
                      div(class="time-body",
                        card_body(
                          div(class="timeseries_tabs", 
                            navset_pill(
                              nav_panel("Selected Location",div(class = "selected-graph", plotlyOutput("timeSeries", height = "450px"), hr(), verbatimTextOutput("model_summary_text"))),
                              nav_panel("Northern Hemisphere", div(class = "north-graph", plotlyOutput("northAverage", height = "500px"))),
                              nav_panel("Southern Hemisphere", div(class = "south-graph", plotlyOutput("southAverage", height = "500px"))),
                              nav_panel("Global Average", div(class = "global-graph", plotlyOutput("globalAverage", height = "500px"))
                              )
                            )
                          )
                        )
                      )
                    )
             )
           )
  )
)

# Define server logic
server <- function(input, output, session) {
  
  output$map <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addSearchOSM(options = searchOptions(zoom = 5)) %>%
      setView(lng = -122, lat = 37.0902, zoom = 5)
  })
  
  output$map2 <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      addSearchOSM(options = searchOptions(zoom = 5)) %>%
      setView(lng = -122, lat = 37.0902, zoom = 3)
  })
  
  observeEvent(input$map_click, {
    click <- input$map_click
    longitude <- click$lng
    latitude <- click$lat
    
    # Determine the boundaries of the grid cell
    lon1 <- floor(longitude)
    lon2 <- lon1 + 1
    lat1 <- floor(latitude)
    lat2 <- lat1 + 1
    
    # Create a single grid cell polygon
    create_single_grid_cell <- function(lon1, lat1, lon2, lat2) {
      coords <- matrix(c(lon1, lat1, 
                         lon2, lat1, 
                         lon2, lat2, 
                         lon1, lat2, 
                         lon1, lat1), 
                       ncol = 2, byrow = TRUE)
      sp::SpatialPolygons(list(sp::Polygons(list(sp::Polygon(coords)), ID = "1")))
    }
    
    single_cell <- create_single_grid_cell(lon1, lat1, lon2, lat2)
    
    # Highlight the selected polygon
    leafletProxy("map") %>%
      clearShapes() %>%
      addPolygons(data = single_cell, color = "red", fillOpacity = 0.3)
    
    # Filter data for the clicked location
    selected_data <- subset(combined_data, lat == floor(latitude) & lon == floor(longitude))
    
    # Take Selected Loc Data to determine NS & EW
    NS = NULL
    if (selected_data$lat[1] >= 0){
      NS = "N"
    } else {
      NS = "S"
    }
    EW = NULL
    if (selected_data$lon[1] >= 0){
      EW = "E"
    } else {
      EW = "W"
    }
    
    if (nrow(selected_data) > 0) {
      # Calculate Slope per Decade
      first_ten <- head(selected_data, 10)  # First 10 years
      last_ten <- tail(selected_data, 10)   # Last 10 Years
      
      # Apply a regression model for each segment 
      first_reg <- lm(fittrend ~ time, data = first_ten)
      last_reg <- lm(fittrend ~ time, data = last_ten)
      
      summary_first <- summary(first_reg)
      summary_last <- summary(last_reg)
      
      coefficients_first <- summary_first$coefficients 
      coefficients_last <- summary_last$coefficients
      
      slope_per_decade_beg <- coefficients_first[2, 1] * 10  # Pulling slope per year values and converting to decade
      slope_per_decade_end <- coefficients_last[2, 1] * 10
      
      # Generate text for bottom of plot
      if (round(slope_per_decade_beg, digits = 3) == round(slope_per_decade_end, digits = 3)) {
        output$model_summary_text <- renderText({
          paste("The Magnitude of the Trend is:", 
                format(slope_per_decade_end, scientific = TRUE, digits = 3), "°C/Decade.")
        })
      } else {
        output$model_summary_text <- renderText({
          paste("The Magnitude before the Changepoint:",
                format(slope_per_decade_beg, scientific = TRUE, digits = 3), "°C/Decade",
                "and after:", format(slope_per_decade_end, scientific = TRUE, digits = 3), "°C/Decade.")
        })
      }
      
      output$timeSeries <- renderPlotly({
        colors <- c("Observations" = "black", "Trend" = "red")
        
        p <- ggplot(selected_data, aes(x = time, y = temperature, color = "Observations")) + 
          geom_line(size = 0.25) +
          geom_line(data = selected_data, aes(x = time, y = fittrend, color = 'Trend')) +
          scale_color_manual(values = colors, name = NULL) +
          scale_x_continuous(
            breaks = seq(floor(min(selected_data$time)/10)*10, 
                         ceiling(max(selected_data$time)/10)*10, 
                         by = 10)
          ) +
          theme_bw() + 
          theme(
            legend.key.size = unit(1.5, 'cm'),
            legend.position = c(.13, .84),
            legend.text = element_text(size = 12), 
            legend.background = element_rect(colour = "transparent", fill = 'lightgrey'),
            axis.text = element_text(size = 13), 
            axis.text.x = element_text(angle = 0, hjust = 0.5),
            axis.title = element_text(size = 13), 
            plot.title = element_text(size = 16, face = 'bold'),
            plot.margin = margin(10, 10, 10, 10)
          ) +
          labs(
            title = paste("Temperature Data at", abs(selected_data$lat[1]), "°", NS, ",", 
                          abs(selected_data$lon[1]), "°", EW),
            x = "Year",
            y = "Temperature Anomaly (°C)",
            color = "colors"
          )
        
        ggplotly(p, tooltip = c("x", "y")) %>%
          plotly::layout(
            hovermode = "x unified",
            title = list(
              text = paste("Temperature Data at", abs(selected_data$lat[1]), "°", NS, ",", 
                           abs(selected_data$lon[1]), "°", EW),
              font = list(size = 16, family = "Merriweather"),
              yanchor = "top",
              y = 0.98
            ),
            xaxis = list(
              title = list(text = "Year", font = list(size = 13)),
              tickfont = list(size = 11),
              dtick = 10
            ),
            yaxis = list(
              title = list(text = "Temperature Anomaly (°C)", font = list(size = 13)),
              tickfont = list(size = 11)
            ),
            legend = list(
              x = 0.02,
              y = 0.98,
              bgcolor = 'rgba(211, 211, 211, 0.5)',
              bordercolor = 'transparent'
            ),
            margin = list(t = 60, r = 20, b = 50, l = 60)
          ) %>%
          config(displayModeBar = TRUE, scrollZoom = TRUE)
      })
      
    } else {
      output$timeSeries <- renderPlot({
        NULL
      })
      output$model_summary_text <- renderText({
        "No data available for this location."
      })
    }
  })
  
  # Function to plot and compute trend
  plot_with_trend <- function(data, title) {
    lm_model <- lm(avg_temperature ~ time, data = data)
    data$trend <- predict(lm_model)
    
    # Compute trend in °C per decade
    trend_per_decade <- coef(lm_model)[2] * 10
    
    # Get p-value for the slope
    p_value <- summary(lm_model)$coefficients[2, 4]
    
    # Determine significance
    significance <- ifelse(p_value < 0.05, "The trend is significant.", "The trend is not significant.")
    
    # Build ggplot
    p <- ggplot(data, aes(x = time, y = avg_temperature)) +
      geom_line(color = "black", size = 0.4) +
      geom_line(aes(y = trend), color = "red", size = 1) +
      theme_minimal() +
      labs(
        title = title,
        x = "Year",
        y = "Average Temperature Anomaly (°C)"
      ) +
      theme(
        plot.title = element_text(size = 16, face = "bold", family = "Merriweather"),
        axis.title = element_text(size = 13),
        axis.text = element_text(size = 11),
        plot.margin = margin(10, 10, 10, 10)
      )
    
    ggplotly(p, tooltip = c("x", "y")) %>%
      layout(
        hovermode = "x unified",
        title = list(
          text = paste0(
            title, "<br><sup>",
            "Trend: ", round(trend_per_decade, 3), "°C/decade — ", significance, "</sup>"
          ),
          font = list(size = 16, family = "Merriweather")
        ),
        xaxis = list(title = "Year", tickfont = list(size = 11)),
        yaxis = list(title = "Average Temperature (°C)", tickfont = list(size = 11)),
        margin = list(t = 60, r = 20, b = 50, l = 60)
      ) %>%
      config(displayModeBar = TRUE, scrollZoom = TRUE)
  }
  
  # Compute and plot North Average temperature time series with linear trend
  north_data <- combined_data %>%
    filter(lat >= 0) %>%
    group_by(time) %>%
    summarize(avg_temperature = mean(temperature, na.rm = TRUE))
  
  output$northAverage <- renderPlotly({
    plot_with_trend(north_data, "Northern Hemisphere Average Temperature Anomaly")
  })
  
  # Compute and plot South Average temperature time series with linear trend
  south_data <- combined_data %>%
    filter(lat <= 0) %>%
    group_by(time) %>%
    summarize(avg_temperature = mean(temperature, na.rm = TRUE))
  
  output$southAverage <- renderPlotly({
    plot_with_trend(south_data, "Southern Hemisphere Average Temperature Anomaly")
  })
  
  # Compute and plot Global Average temperature time series with linear trend
  global_data <- combined_data %>%
    group_by(time) %>%
    summarize(avg_temperature = mean(temperature, na.rm = TRUE))
  
  output$globalAverage <- renderPlotly({
    plot_with_trend(global_data, "Global Average Temperature Anomaly")
  })
}

# Run the application
shinyApp(ui = ui, server = server)