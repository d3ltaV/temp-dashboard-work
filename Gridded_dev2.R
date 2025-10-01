# Load required packages
setwd("/Users/nico/Monitoring_Dashboard")


library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(ncdf4)
library(sp)
library(tidyverse)

########## Data Processing: Gridded Data ##########
#Loading Processed Data + Resulting CP Analysis
load("/Users/nico/Desktop/Claudie Dropbox/Datasets/annual_Berkeley_anom.RData")
load('/Users/nico/Desktop/Claudie Dropbox/Results/ResultsBerkeley.RData')
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
#Add the changepoint fit to the combined data frame
combined_data$fittrend <- as.vector(fittrend)
# Remove rows with NaN values
combined_data <- combined_data[complete.cases(combined_data), ]

# Convert longitude from 1-360 to -180 to 180
combined_data$lon <- combined_data$lon - 180
combined_data$lon[combined_data$lon > 180] <- combined_data$lon[combined_data$lon > 180] - 360

# Convert latitude from 1-180 to -90 to 90
combined_data$lat <- combined_data$lat - 90



########## Data Processing: Country Data ##########

USA_data = read.table("united-states-of-america-TAVG-monthly.txt", skip = 96)

colnames(USA_data)[colnames(USA_data) == "V1"] <- "Year"
colnames(USA_data)[colnames(USA_data) == "V2"] <- "Month"
colnames(USA_data)[colnames(USA_data) == "V3"] <- "Monthly_Anomaly"

########## App ##########
library(leaflet.extras)


# Define UI
ui <- fluidPage(
  titlePanel("Temperature Dashboard"),
  fluidRow(
    column(width = 6,
           tabsetPanel(
             tabPanel("Map", leafletOutput("map", height = "500px")),
             tabPanel("Country Map", leafletOutput("map2", height = "500px"))
           )
    ),
    
    column(width = 6,
           h3("Time Series"),
           tabsetPanel(
             tabPanel("Selected Location", plotOutput("timeSeries", height = "500px")),
             #tabPanel("North Average", plotOutput("northAverage", height = "500px")),
             #tabPanel("South Average", plotOutput("southAverage", height = "500px")),
             #tabPanel("Global Average", plotOutput("globalAverage", height = "500px"))
           ),
           textOutput("model_summary_text")
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
    
    #Take Selected Loc Data to determine NS & EW
    NS = NULL
    if (selected_data$lat[1] >= 0){
      NS = "N"
    }else{
      NS = "S"
    }
    EW = NULL
    if (selected_data$lon[1] >= 0){
      EW = "E"
    }else{
      EW = "W"
    }
    
    if (nrow(selected_data) > 0) {
    #Calculate Slope per Decade
    first_ten <- head(selected_data,10) #First 10 years
    last_ten <- tail(selected_data,10) #Last 10 Years
    
    #Apply a regression model for each segment 
    
    first_reg <- lm(fittrend ~ time, data = first_ten)
    last_reg <- lm(fittrend ~ time, data = last_ten)
    
    summary_first <- summary(first_reg)
    summary_last <- summary(last_reg)
    
    coefficients_first <- summary_first$coefficients 
    coefficients_last <- summary_last$coefficients
    
    slope_per_decade_beg <- coefficients_first[2, 1] * 10 #Pulling slope per year values and converting to decade
    slope_per_decade_end <- coefficients_last[2, 1] * 10
      
    #Generate text for bottom of plot
      if (round(slope_per_decade_beg,digits = 3) == round(slope_per_decade_end,digits = 3)) {
        output$model_summary_text <- renderText({
          paste("The Magnitude of the Trend is:", 
                format(slope_per_decade_end, scientific = TRUE, digits = 3),"°C/Decade.")
      })
      } else {
        output$model_summary_text <- renderText({
          paste("The Magnitude before the Changepoint:",
                format(slope_per_decade_beg, scientific = TRUE, digits = 3),"°C/Decade",
                "and after:", format(slope_per_decade_end, scientific = TRUE, digits = 3),"°C/Decade.")
        })
        }
      #For test casing...
      #selected_data <- subset(combined_data, lat == 30 & lon == 115)
      
      output$timeSeries <- renderPlot({
        colors <- c("Observations" = "black", "Trend" = "red")
        ggplot(selected_data, aes(x = time, y = temperature, color = "Observations")) + 
          geom_line(size = 0.25) +
          geom_line(data = selected_data, aes(x = time, y = fittrend, color = 'Trend')) +
          scale_color_manual(values = colors, name = NULL) +
          theme_bw() + 
          theme(legend.key.size = unit(1.5,'cm'),legend.position = c(.13,.84),legend.text = element_text(size=12), 
                legend.background = element_rect(colour = "transparent", fill = 'lightgrey'),axis.text=element_text(size=15), 
                axis.title = element_text(size = 13), plot.title = element_text(size = 18, face='bold')) +
          labs(
            title = paste("Temperature Data at", selected_data$lat,NS,",", selected_data$lon,EW), #"Temperature Data at Selected Location",
            x = "Year",
            y = "Temperature Anomaly(°C)",
            color = "colors"
            #,
            #caption = paste(
            # "Trend:", round(slope_per_decade_beg, 3), "°C/decade")
             # "|", significance)
          )
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
  
  # function to plot and compute trend
  plot_with_trend <- function(data, title) {
    lm_model <- lm(avg_temperature ~ time, data = data)
    data$trend <- predict(lm_model)
    
    # Compute trend in °C per decade
    trend_per_decade <- coef(lm_model)[2] * 10
    
    # Get p-value for the slope
    p_value <- summary(lm_model)$coefficients[2, 4]
    
    # Determine significance
    significance <- ifelse(p_value < 0.05, "The trend is significant.", "The trend is not significant.")
    
    ggplot(data, aes(x = time, y = avg_temperature)) +
      geom_line(size = 0.25) +
      geom_line(aes(y = trend), color = "red", lwd = 1) +
      theme_minimal() +
      labs(
        title = title,
        x = "Year",
        y = "Average Temperature (°C)",
        caption = paste(
          "Trend:", round(trend_per_decade, 3), "°C/decade",
          "|", significance
        )
      )
  }
  
  # Compute and plot North Average temperature time series with linear trend
  north_data <- combined_data %>%
    filter(lat >= 0) %>%
    group_by(time) %>%
    summarize(avg_temperature = mean(temperature, na.rm = TRUE))
  
  output$northAverage <- renderPlot({
    plot_with_trend(north_data, "Average Temperature in the Northern Hemisphere")
  })
  
  # Compute and plot South Average temperature time series with linear trend
  south_data <- combined_data %>%
    filter(lat <= 0) %>%
    group_by(time) %>%
    summarize(avg_temperature = mean(temperature, na.rm = TRUE))
  
  output$southAverage <- renderPlot({
    plot_with_trend(south_data, "Average Temperature in the Southern Hemisphere")
  })
  
  # Compute and plot Global Average temperature time series with linear trend
  global_data <- combined_data %>%
    group_by(time) %>%
    summarize(avg_temperature = mean(temperature, na.rm = TRUE))
  
  output$globalAverage <- renderPlot({
    plot_with_trend(global_data, "Global Average Temperature")
  })
}

# Run the application
shinyApp(ui = ui, server = server)

