# Install necessary packages if not already installed
# install.packages("shiny")
# install.packages("ggplot2")
# install.packages("dplyr")
# install.packages("plotly")
# install.packages("lubridate")
# install.packages("tidygeocoder")
# install.packages("leaflet")
# install.packages("ggridges")

# Load necessary libraries
library(shiny)
library(ggplot2)
library(dplyr)
library(plotly)
library(lubridate)
library(tidygeocoder)
library(leaflet)
library(tidyr)

# Load your dataset (replace with your actual dataset path)
flights_data <- read.csv("/Users/bingqian/datacleaning/cleaned_dataset.csv")

# General Data Preparation
flights_data <- flights_data %>%
  mutate(
    Month = month(FL_DATE, label = TRUE, abbr = TRUE),
    Total_Delay = ARR_DELAY + DEP_DELAY,
    ORIGIN_STATE = sub(".*,\\s*", "", ORIGIN_CITY),  # Extract state from ORIGIN_CITY
    DEST_STATE = sub(".*,\\s*", "", DEST_CITY),       # Extract state from DEST_CITY
    Season = case_when(
      Month %in% c("Dec", "Jan", "Feb") ~ "Winter",
      Month %in% c("Mar", "Apr", "May") ~ "Spring",
      Month %in% c("Jun", "Jul", "Aug") ~ "Summer",
      Month %in% c("Sep", "Oct", "Nov") ~ "Autumn"
    )
  ) %>%
  filter(!is.na(DISTANCE) & !is.na(Total_Delay)) 

# Processed data for bar chart
processed_data <- flights_data %>%
  group_by(AIRLINE_CODE, AIRLINE) %>%
  summarise(
    Total_Operations = n(),
    Total_Delayed = sum(ARR_DELAY > 0 | DEP_DELAY > 0, na.rm = TRUE)
  ) %>%
  mutate(
    Percent_Delayed = (Total_Delayed) / Total_Operations * 100,
    Label = paste(AIRLINE, "(", AIRLINE_CODE, ")", sep = "")
  )

# Heatmap Data Preparation
heatmap_data <- flights_data %>%
  group_by(Month,Season, AIRLINE) %>%
  summarise(
    Total_Operations = n(),
    Total_Delayed = sum(ARR_DELAY > 0 | DEP_DELAY > 0, na.rm = TRUE)
  ) %>%
  mutate(
    Delay_Rate = Total_Delayed / Total_Operations * 100
  )

# Distance vs Delay Data Preparation
distance_delay_data <- flights_data %>%
  mutate(Distance_Bin = cut(DISTANCE, breaks = seq(0, max(DISTANCE), by = 100), include.lowest = TRUE)) %>%
  group_by(Distance_Bin) %>%
  summarise(
    Avg_Delay = mean(Total_Delay, na.rm = TRUE),
    Total_Flights = n()
  ) %>%
  mutate(Distance_Label = as.numeric(sub("\\((.+),.*", "\\1", as.character(Distance_Bin))))

# Calculate total flights for each airport (as both origin and destination)
airport_total_flights <- flights_data %>%
  select(ORIGIN, DEST) %>%
  pivot_longer(
    cols = c(ORIGIN, DEST),
    names_to = "Airport_Type",
    values_to = "Airport"
  ) %>%
  group_by(Airport) %>%
  summarise(Total_Flights = n(), .groups = "drop")  # Count total flights (origin + destination)

# Filter airports with at least 1000 total flights
filtered_airports <- airport_total_flights %>%
  filter(Total_Flights >= 2000) %>%
  pull(Airport)  # Extract the list of valid airports

# Filter flights data to include only valid airports
filtered_flights_data <- flights_data %>%
  filter(ORIGIN %in% filtered_airports | DEST %in% filtered_airports)

# Preprocess data for state-level delay analysis
state_delay_data <- filtered_flights_data %>%
  select(ORIGIN_STATE, DEST_STATE, Total_Delay) %>%
  pivot_longer(
    cols = c(ORIGIN_STATE, DEST_STATE),
    names_to = "State_Type",
    values_to = "STATE"
  ) %>%
  group_by(STATE) %>%
  summarise(
    Avg_Delay = mean(Total_Delay, na.rm = TRUE),  # Average delay
    Total_Delay = sum(Total_Delay, na.rm = TRUE),  # Total delay
    Total_Flights = n(),  # Total flights
    .groups = "drop"
  ) %>%
  filter(!is.na(STATE))  # Remove rows with NA states

# Choropleth Map Data Preparation (States)
state_delay_data <- flights_data %>%
  select(ORIGIN_STATE, DEST_STATE, Total_Delay) %>%
  pivot_longer(cols = c(ORIGIN_STATE, DEST_STATE), names_to = "State_Type", values_to = "STATE") %>%
  group_by(STATE) %>%
  summarise(
    Avg_Delay = mean(Total_Delay, na.rm = TRUE),
    Total_Delay = sum(Total_Delay, na.rm = TRUE),
    Total_Flights = n()
  ) %>%
  filter(!is.na(STATE))  # Remove NA states

# Preprocess data for combined departing and arriving delays
airport_delay_data <- flights_data %>%
  # Combine departing and arriving flights into a single column
  pivot_longer(
    cols = c(ORIGIN, DEST),
    names_to = "Flight_Type",
    values_to = "Airport"
  ) %>%
  pivot_longer(
    cols = c(ORIGIN_CITY, DEST_CITY),
    names_to = "City_Type",
    values_to = "Airport_City"
  ) %>%
  # Ensure proper pairing of Flight_Type and City_Type
  filter((Flight_Type == "ORIGIN" & City_Type == "ORIGIN_CITY") |
         (Flight_Type == "DEST" & City_Type == "DEST_CITY")) %>%
  group_by(Airport, Airport_City) %>%
  summarise(
    Avg_Delay = mean(Total_Delay, na.rm = TRUE),  # Average delay (both departing and arriving flights)
    Total_Flights = n(),                         # Total number of flights (departing + arriving)
    .groups = "drop"
  ) %>%
  filter(Total_Flights >= 2000)  # Filter airports with at least 1000 flights

# Data preparation for airport-level delay analysis
airport_delay_correlation_data <- flights_data %>%
  group_by(ORIGIN, ORIGIN_CITY) %>%
  summarise(
    Total_Operations = n(),
    Total_Delayed = sum(ARR_DELAY > 0 | DEP_DELAY > 0, na.rm = TRUE),
    Delay_Rate = Total_Delayed / Total_Operations * 100,  # Delay rate as a percentage
    Avg_Delay = mean(Total_Delay, na.rm = TRUE)
  ) %>%
  filter(!is.na(Total_Operations) & Total_Operations > 0)  # Filter valid data

flights_box_data <- flights_data %>%
  mutate(
    Time_of_Day = factor(
      case_when(
        DEP_TIME >= 0 & DEP_TIME < 600 ~ "Night",
        DEP_TIME >= 600 & DEP_TIME < 1200 ~ "Morning",
        DEP_TIME >= 1200 & DEP_TIME < 1800 ~ "Afternoon",
        DEP_TIME >= 1800 & DEP_TIME <= 2359 ~ "Evening"
      ),
      levels = c("Morning", "Afternoon", "Evening", "Night") 
    ),
    Day_of_Week = factor(
      weekdays(as.Date(FL_DATE)),
      levels = c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    )
  )



# Prepare the dataset with geocoded coordinates
cache_file <- "geocoded_paths.rds"

if (!file.exists(cache_file)) {
  unique_airports <- flights_data %>%
    select(ORIGIN, ORIGIN_CITY) %>%
    distinct() %>%
    rename(Airport = ORIGIN, City = ORIGIN_CITY)

  geocoded_data <- unique_airports %>%
    geocode(City, method = "osm", lat = latitude, long = longitude)

  saveRDS(geocoded_data, cache_file)
} else {
  geocoded_data <- readRDS(cache_file)
}

# Merge geocoded coordinates back into the main dataset
flights_data <- flights_data %>%
  left_join(geocoded_data, by = c("ORIGIN" = "Airport")) %>%
  rename(origin_lat = latitude, origin_long = longitude) %>%
  left_join(geocoded_data, by = c("DEST" = "Airport")) %>%
  rename(dest_lat = latitude, dest_long = longitude)

# Filter rows with valid coordinates
flights_data <- flights_data %>%
  filter(!is.na(origin_lat) & !is.na(origin_long) & !is.na(dest_lat) & !is.na(dest_long))

# Geocoding the airports
# Cache results in a local file to avoid re-running geocoding every time
cache_file <- "geocoded_airports.rds"

if (!file.exists(cache_file)) {
  unique_cities <- airport_delay_data %>%
    distinct(ORIGIN_CITY)

  geocoded_data <- unique_cities %>%
    geocode(ORIGIN_CITY, method = "osm", lat = latitude, long = longitude)
  
  saveRDS(geocoded_data, cache_file)  # Save geocoded data to cache
} else {
  geocoded_data <- readRDS(cache_file)  # Load cached geocoded data
}

# Merge geocoded coordinates back into the main dataset
airport_delay_data <- airport_delay_data %>%
  left_join(geocoded_data, by = c("Airport_City" = "ORIGIN_CITY")) %>%
  filter(!is.na(latitude) & !is.na(longitude))  # Remove rows without geocodes

# Define UI
ui <- navbarPage(
  title = "Flight Delays Analysis",
  tabPanel(
    "Q1",
    fluidPage(
      # Section Header
      fluidRow(
        column(width = 12, h3("Q1: How do delays vary across different airlines?"))
      ),
      
      # Add spacing
      br(),
      
      # Question 1: Airlines with highest average delay and delay rate
      fluidRow(
        column(width = 12, p("This bar chart shows the percentage of flights delayed for each airline.")),
        column(width = 12, plotlyOutput("barPlot", height = "600px"))
      ),
      
      # Add spacing
      br(), br(),
      
      # Question 2: Common causes of delay for the airline with the highest average delay
      fluidRow(
        column(width = 12, h4("1.1 For the top 3 airlines that experienced the highest average delay, what are the common causes of delay?")),
        column(width = 4, h5("Southwest Airlines"), plotlyOutput("southwestPieChart", height = "400px")),
        column(width = 4, h5("JetBlue Airways"), plotlyOutput("jetbluePieChart", height = "400px")),
        column(width = 4, h5("Frontier Airlines"), plotlyOutput("frontierPieChart", height = "400px"))
      ),
      
      # Add spacing
      br(), br(),
      
      # Question 3: Delay variations across months or seasons
      fluidRow(
        column(width = 12, h4("1.2 How do delays for each airline vary across different times of the year (e.g., by month or season)?")),
        column(width = 12, p("Use the toggle to view heatmap data by month or season.")),
        column(
          width = 12, 
          radioButtons(
            inputId = "heatmapView", 
            label = "View Heatmap by:",
            choices = c("Month", "Season"), 
            selected = "Month", 
            inline = TRUE
          ),
          plotlyOutput("interactiveHeatmap", height = "600px")
        )
      )
    )
  ),
  tabPanel(
    "Q2",
    fluidPage(
      fluidRow(
        column(width = 12, h3("Q2: Which airports have the highest proportion of delayed flights compared to their total operations?"))
      ),
      fluidRow(
        column(width = 12, p("This map visualization shows airports with the highest delay rates.")),
        column(
          width = 12,
          selectInput(
            inputId = "mapType",
            label = "Select Map Type:",
            choices = c("States", "Cities"),
            selected = "States"
          )
        )
      ),
      
      # Add spacing
      br(),
      
      fluidRow(
        column(width = 12, uiOutput("choroplethMapOutput"))  # Dynamically render the map
      ),
      
      # Add spacing
      br(),br(),
      
      fluidRow(
        column(width = 12, h4("2.1 For airports with high average delays, which flight paths have the highest average delay for each airport?")),
        column(width = 12, p("This map shows flight paths and highlights routes with significant delays."))
      ),  
      fluidRow(
        column(
          width = 12,
          # Horizontal bar panel with controls
          wellPanel(
            fluidRow(
              column(
                width = 6,
                radioButtons(
                  inputId = "flight_mode",
                  label = "View Flights:",
                  choices = c("Departing" = "departing", "Arriving" = "arriving"),
                  selected = "departing",
                  inline = TRUE
                )
              ),
              column(
                width = 6,
                selectInput(
                  inputId = "origin_airport",
                  label = "Select Airport:",
                  choices = unique(flights_data$ORIGIN),
                  selected = unique(flights_data$ORIGIN)[1]
                )
              )
            )
          )
        ),
        column(
          width = 12,
          # Full-width map output
          leafletOutput("routeMap", height = "700px")
        )
      ),
      
      # Add spacing
      br(),br(),
      
      fluidRow(
        column(width = 12, h4("2.2 Do certain airports experience higher delays during specific months or seasons?")),
        column(width = 12, p("Use the dropdown to group delay data by season or month.")),
        column(
          width = 12,
          selectInput(
            inputId = "barChartType",
            label = "Select Grouping:",
            choices = c("Season", "Month"),
            selected = "Season"
          )
        )
      ),
      fluidRow(
        column(width = 12, plotlyOutput("stackedBarChartScrollable", height = "700px"))
      )
    )
  ),
  tabPanel(
    "Q3",
    fluidPage(
      fluidRow(
        column(width = 12, h3("Q3: What factors contribute the most to delays?"))
      ),
      fluidRow(
        column(width = 12, 
              radioButtons(
                inputId = "delayMetric",
                label = "Select Metric to View:",
                choices = c("Frequency", "Average Delay"),
                selected = "Average Delay",
                inline = TRUE
              )
        ),
        column(
          width = 12,
          plotlyOutput("delayTypeBarChart", height = "700px")
        )
      ),
      
      # Add spacing
      br(),br(),
      
      fluidRow(
        column(width = 12, h4("3.1 Are certain delay types more frequent at specific times of the day / days of the week ?")),
        column(width = 12, p(""))
      ),
      sidebarLayout(
        sidebarPanel(
          selectInput("delay_type", "Select Delay Type:", 
                      choices = c("Carrier", "Weather", "NAS", "Security", "Late Aircraft")),
          radioButtons("time_dimension", "Group By:",
                       choices = c("Time of Day" = "Time_of_Day", "Day of Week" = "Day_of_Week"))
        ),
        mainPanel(
          plotlyOutput("areaGraph")
        )
      ),
      
      # Add spacing
      br(),br(),
      
      fluidRow(
        column(width = 12, h4("3.2 Can flight distance be considered a significant factor contributing to flight delays? ")),
        column(width = 12, plotlyOutput("lineGraphDistanceDelay", height = "600px"))
      )
    )
  )
)


# Define Server Logic
server <- function(input, output,session) {
  
  output$barPlot <- renderPlotly({
  bar_plot <- ggplot(processed_data, aes(
    x = reorder(Label, Percent_Delayed),
    y = Percent_Delayed,
    fill = Percent_Delayed,
    text = paste(
      "Airline:", Label,
      "<br>Percent Delayed:", round(Percent_Delayed, 1), "%"
    )
  )) +
    geom_bar(stat = "identity", width = 0.5) +
    coord_flip() +
    labs(
      title = "Delay Rates by Airline", 
      x = "Airline (Code)", 
      y = "% of Flights Delayed"
    ) +
    theme_minimal(base_size = 10) +
    theme(
      axis.text.y = element_text(size = 8), 
      axis.title.y = element_blank(), 
      legend.position = "none"
    ) +
    scale_fill_gradient(low = "lightgray", high = "darkblue")
  
  # Convert ggplot to plotly for interactivity, using the 'text' aesthetic for hover info
  ggplotly(bar_plot, tooltip = "text")
})

output$interactiveHeatmap <- renderPlotly({
  if (input$heatmapView == "Month") {
    # Group by Month
    heatmap_data_month <- heatmap_data %>%
      group_by(Month, AIRLINE) %>%
      summarise(Delay_Rate = mean(Delay_Rate, na.rm = TRUE))
    
    heatmap_plot <- ggplot(heatmap_data_month, aes(
      x = Month, 
      y = AIRLINE, 
      fill = Delay_Rate,
      text = paste(
        "Month:", Month,
        "<br>Airline:", AIRLINE,
        "<br>Delay Rate:", round(Delay_Rate, 1), "%"
      )
    )) +
      geom_tile(color = "white", linewidth = 0.2) +
      labs(
        title = "Heatmap of Delay Rate by Airline and Month", 
        x = "Month", 
        y = "Airline", 
        fill = "% Delayed"
      ) +
      scale_fill_gradient(low = "lightyellow", high = "darkred") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
  } else {
    # Group by Season
    heatmap_data_season <- heatmap_data %>%
      group_by(Season, AIRLINE) %>%
      summarise(Delay_Rate = mean(Delay_Rate, na.rm = TRUE))
    
    heatmap_plot <- ggplot(heatmap_data_season, aes(
      x = Season, 
      y = AIRLINE, 
      fill = Delay_Rate,
      text = paste(
        "Season:", Season,
        "<br>Airline:", AIRLINE,
        "<br>Delay Rate:", round(Delay_Rate, 1), "%"
      )
    )) +
      geom_tile(color = "white", linewidth = 0.2) +
      labs(
        title = "Heatmap of Delay Rate by Airline and Season", 
        x = "Season", 
        y = "Airline", 
        fill = "% Delayed"
      ) +
      scale_fill_gradient(low = "lightyellow", high = "darkred") +
      theme_minimal(base_size = 12) +
      theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10))
  }
  
  # Use the text aesthetic for hover tooltips
  ggplotly(heatmap_plot, tooltip = "text")
})


calculate_delay_frequencies <- function(airline_code) {
  flights_data %>%
    filter(AIRLINE_CODE == airline_code) %>%
    summarise(
      Carrier = sum(DELAY_DUE_CARRIER > 0, na.rm = TRUE),
      Weather = sum(DELAY_DUE_WEATHER > 0, na.rm = TRUE),
      NAS = sum(DELAY_DUE_NAS > 0, na.rm = TRUE),
      Security = sum(DELAY_DUE_SECURITY > 0, na.rm = TRUE),
      Late_Aircraft = sum(DELAY_DUE_LATE_AIRCRAFT > 0, na.rm = TRUE)
    ) %>%
    pivot_longer(
      cols = everything(),
      names_to = "Cause",
      values_to = "Frequency"
    ) %>%
    mutate(
      Cause = recode(Cause,
                     "NAS" = "National Air System",
                     "Late_Aircraft" = "Late Aircraft")  # Rename causes
    )
}

  
# Render Southwest Airlines Pie Chart
output$southwestPieChart <- renderPlotly({
  southwest_data <- calculate_delay_frequencies("WN")
  
  # Create a custom hover text with details
  southwest_data <- southwest_data %>%
    mutate(
      Hover_Text = paste(
        Cause, "Delay", 
        "<br>", format(Frequency, big.mark = ","), "flights",
        "<br>", round(Frequency / sum(Frequency) * 100, 1), "%"
      )
    )
  
  plot_ly(
    data = southwest_data,
    labels = ~Cause,
    values = ~Frequency,
    type = "pie",
    textinfo = "label+percent",  # Display label and percent on the chart
    hoverinfo = "text",         # Use custom hover text
    text = ~Hover_Text,         # Provide custom text for hover
    marker = list(colors = c("blue", "orange", "yellow", "red", "green"))
  ) %>%
    layout(
      title = list(text = "Southwest Airlines (WN)", font = list(size = 16), x = 0.5),
      margin = list(t = 50)
    )
})


# Render JetBlue Airways Pie Chart
output$jetbluePieChart <- renderPlotly({
  jetblue_data <- calculate_delay_frequencies("B6")
  
  # Create a custom hover text
  jetblue_data <- jetblue_data %>%
    mutate(
      Hover_Text = paste(
        Cause, "Delay", 
        "<br>", format(Frequency, big.mark = ","), "flights",
        "<br>", round(Frequency / sum(Frequency) * 100, 1), "%"
      )
    )
  
  plot_ly(
    data = jetblue_data,
    labels = ~Cause,
    values = ~Frequency,
    type = "pie",
    textinfo = "label+percent",
    hoverinfo = "text",
    text = ~Hover_Text,
    marker = list(colors = c("blue", "orange", "yellow", "red", "green"))
  ) %>%
    layout(
      title = list(text = "JetBlue Airways (B6)", font = list(size = 16), x = 0.5),
      margin = list(t = 50)
    )
})

# Render Frontier Airlines Pie Chart
output$frontierPieChart <- renderPlotly({
  frontier_data <- calculate_delay_frequencies("F9")
  
  # Create a custom hover text
  frontier_data <- frontier_data %>%
    mutate(
      Hover_Text = paste(
        Cause, "Delay", 
        "<br>", format(Frequency, big.mark = ","), "flights",
        "<br>", round(Frequency / sum(Frequency) * 100, 1), "%"
      )
    )
  
  plot_ly(
    data = frontier_data,
    labels = ~Cause,
    values = ~Frequency,
    type = "pie",
    textinfo = "label+percent",
    hoverinfo = "text",
    text = ~Hover_Text,
    marker = list(colors = c("blue", "orange", "yellow", "red", "green"))
  ) %>%
    layout(
      title = list(text = "Frontier Airlines (F9)", font = list(size = 16), x = 0.5),
      margin = list(t = 50)
    )
})


# Line Graph for Distance vs. Delay
output$lineGraphDistanceDelay <- renderPlotly({
    line_graph <- ggplot(distance_delay_data, aes(x = Distance_Label, y = Avg_Delay)) +
        geom_line(color = "blue", linewidth = 1) +
        geom_point(
            aes(text = paste(
                "Distance:", Distance_Label,
                "<br>Avg Delay:", round(Avg_Delay, 2)
            )),
            color = "red",
            size = 2
        ) +
        labs(
            title = "Average Delay vs. Distance",
            x = "Distance (Miles)",
            y = "Average Delay (Minutes)"
        ) +
        theme_minimal(base_size = 12)
    
    # Use ggplotly with tooltips for interactivity
    ggplotly(line_graph, tooltip = "text")
})


  # Dynamic Choropleth Map Output
  output$choroplethMapOutput <- renderUI({
    if (input$mapType == "States") {
      plotlyOutput("choroplethMapStates", height = "700px")
    } else {
      leafletOutput("choroplethMapCities", height = "700px")
    }
  })

 # Add state centroids for labeling
state_centroids <- data.frame(
  STATE = state.abb,  # State abbreviations
  STATE_NAME = state.name,  # Full state names
  lon = state.center$x,  # Longitude of state centroids
  lat = state.center$y   # Latitude of state centroids
)

# Manually adjust centroids for Alaska and Hawaii
state_centroids <- state_centroids %>%
  mutate(
    lon = ifelse(STATE == "AK", -150, lon),  # Adjust longitude for Alaska
    lat = ifelse(STATE == "AK", 63, lat),    # Adjust latitude for Alaska
    lon = ifelse(STATE == "HI", -157, lon),  # Adjust longitude for Hawaii
    lat = ifelse(STATE == "HI", 20, lat)     # Adjust latitude for Hawaii
  )

# Merge state centroids into the delay data
state_delay_data <- state_delay_data %>%
  left_join(state_centroids, by = "STATE")

# Choropleth Map for States with Labels
output$choroplethMapStates <- renderPlotly({
  plot_geo(state_delay_data, locationmode = "USA-states") %>%
    add_trace(
      z = ~Avg_Delay,
      locations = ~STATE,
      text = ~paste(
        "State Abbreviation:", STATE,
        "<br>State:", STATE_NAME,
        "<br>Avg Delay:", round(Avg_Delay, 2), " min",
        "<br>Total Flights:", Total_Flights,
        "<br>Total Delay:", round(Total_Delay, 2), " min"
      ),
      hoverinfo = "text",  # Enable custom hover text
      color = ~Avg_Delay,
      colors = "Reds"
    ) %>%
    add_markers(
      x = ~lon,  # Longitude for text
      y = ~lat,  # Latitude for text
      text = ~STATE,  # State abbreviations
      hoverinfo = "none",  # Disable hover for text labels
      showlegend = FALSE,  # Remove legend entry for text markers
      marker = list(size = 1, opacity = 0),  # Invisible markers for text placement
      textfont = list(color = "black", size = 12),  # Text font size and color
      mode = "text"  # Text mode
    ) %>%
    layout(
      title = "Average Flight Delay by State",
      geo = list(
        scope = "usa",
        projection = list(type = "albers usa"),
        showlakes = TRUE,
        lakecolor = "lightblue"
      ),
      margin = list(l = 0, r = 0, t = 50, b = 0)
    )
})


output$choroplethMapCities <- renderLeaflet({
  leaflet(airport_delay_data) %>%
    addTiles() %>%
    addCircleMarkers(
      ~longitude, ~latitude,
      radius = ~sqrt(Avg_Delay) * 4,  # Scale radius based on average delay
      color = ~ifelse(Avg_Delay > 35, "darkred", 
               ifelse(Avg_Delay > 20, "red", 
               ifelse(Avg_Delay > 10, "orange", "green"))),  # Color coding based on delay
      fillOpacity = 0.8,
      popup = ~paste0(
        "<b>Airport:</b> ", Airport, "<br>",
        "<b>City:</b> ", Airport_City, "<br>",
        "<b>Average Delay:</b> ", round(Avg_Delay, 2), " minutes<br>",
        "<b>Total Flights:</b> ", Total_Flights
      )
    ) %>%
    setView(lng = -98.35, lat = 39.5, zoom = 4) %>%  # Center map on the USA
    addLegend(
      position = "bottomright",
      colors = c("green", "orange", "red", "darkred"),
      labels = c("0-10 min", "10-20 min", "20-35 min", ">35 min"),
      title = "Avg Delay",
      opacity = 1
    )
})

  
output$stackedBarChartScrollable <- renderPlotly({
  # Combine departures and arrivals into a unified airport total dataset
  airport_totals <- flights_data %>%
    select(ORIGIN_CITY, DEST_CITY, Total_Delay, Month, Season) %>%
    pivot_longer(
      cols = c(ORIGIN_CITY, DEST_CITY),
      names_to = "Flight_Type",
      values_to = "Airport_City"
    ) %>%
    group_by(Airport_City) %>%
    summarise(
      Total_Delay = sum(Total_Delay, na.rm = TRUE),
      Total_Flights = n()
    ) %>%
    filter(Total_Flights >= 2000) 

  # Group data by airport and selected time grouping (Month or Season)
  grouping_column <- if (input$barChartType == "Season") "Season" else "Month"

  filtered_data <- flights_data %>%
    pivot_longer(
      cols = c(ORIGIN_CITY, DEST_CITY),
      names_to = "Flight_Type",
      values_to = "Airport_City"
    ) %>%
    filter(Airport_City %in% airport_totals$Airport_City) %>%  # Include only filtered airports
    group_by(Airport_City, !!sym(grouping_column)) %>%
    summarise(
      Total_Delay = sum(Total_Delay, na.rm = TRUE),
      Total_Flights = n()
    ) %>%
    ungroup()

  # Dynamically adjust chart height based on number of airports
  num_airports <- n_distinct(filtered_data$Airport_City)
  chart_height <- num_airports * 20  # 20 pixels per airport row

  # Create the stacked bar chart
  stacked_bar_plot <- ggplot(filtered_data, aes(
    x = reorder(Airport_City, Total_Delay),
    y = Total_Delay,
    fill = !!sym(grouping_column),
    text = paste(
      "Airport:", Airport_City,
      "<br>Total Delay:", Total_Delay,
      "<br>", grouping_column, ":", !!sym(grouping_column)
    )
  )) +
    geom_bar(stat = "identity", position = "stack", width = 0.7) +
    coord_flip() +  # Flip the chart for horizontal bars
    labs(
      title = "Total Delays by Airport and Time Period",
      x = "Airport",
      y = "Total Delay (Minutes)",
      fill = input$barChartType
    ) +
    theme_minimal(base_size = 12) +
    theme(
      axis.text.y = element_text(size = 10),  # Adjust font size for better visibility
      axis.title.y = element_blank(),  # Hide the vertical axis title
      legend.position = "right"  # Position the legend to the side
    )

  # Convert to Plotly for interactivity and enable scrolling
  ggplotly(stacked_bar_plot, tooltip = "text") %>%
    layout(
      height = chart_height,  # Dynamically set height for scrolling
      margin = list(l = 200),  # Add left margin for longer airport names
      xaxis = list(title = "Total Delay (Minutes)"),
      yaxis = list(title = "Airport"),
      scrollZoom = TRUE  # Enable scrolling
    )
})


output$delayTypeBarChart <- renderPlotly({
    # Dynamically calculate the data based on user selection
    delay_contribution_data <- if (input$delayMetric == "Frequency") {
        flights_data %>%
            summarise(
                Carrier_Delay = sum(DELAY_DUE_CARRIER > 0, na.rm = TRUE),
                Weather_Delay = sum(DELAY_DUE_WEATHER > 0, na.rm = TRUE),
                NAS_Delay = sum(DELAY_DUE_NAS > 0, na.rm = TRUE),
                Security_Delay = sum(DELAY_DUE_SECURITY > 0, na.rm = TRUE),
                Late_Aircraft_Delay = sum(DELAY_DUE_LATE_AIRCRAFT > 0, na.rm = TRUE)
            ) %>%
            pivot_longer(
                cols = everything(),
                names_to = "Delay_Type",
                values_to = "Metric_Value"
            ) %>%
            mutate(
                Delay_Type = factor(
                    Delay_Type,
                    levels = c("Carrier_Delay", "Weather_Delay", "NAS_Delay", "Security_Delay", "Late_Aircraft_Delay"),
                    labels = c("Carrier", "Weather", "NAS", "Security", "Late Aircraft")
                )
            )
    } else {
        flights_data %>%
            summarise(
                Carrier_Delay = mean(DELAY_DUE_CARRIER, na.rm = TRUE),
                Weather_Delay = mean(DELAY_DUE_WEATHER, na.rm = TRUE),
                NAS_Delay = mean(DELAY_DUE_NAS, na.rm = TRUE),
                Security_Delay = mean(DELAY_DUE_SECURITY, na.rm = TRUE),
                Late_Aircraft_Delay = mean(DELAY_DUE_LATE_AIRCRAFT, na.rm = TRUE)
            ) %>%
            pivot_longer(
                cols = everything(),
                names_to = "Delay_Type",
                values_to = "Metric_Value"
            ) %>%
            mutate(
                Delay_Type = factor(
                    Delay_Type,
                    levels = c("Carrier_Delay", "Weather_Delay", "NAS_Delay", "Security_Delay", "Late_Aircraft_Delay"),
                    labels = c("Carrier", "Weather", "NAS", "Security", "Late Aircraft")
                )
            )
    }

    # Create the bar chart with hover customization
    bar_plot <- ggplot(delay_contribution_data, aes(
        x = Delay_Type,
        y = Metric_Value,
        fill = Delay_Type,
        text = paste(
            "Delay Type:", Delay_Type,
            "<br>Value:", round(Metric_Value, 2)
        )
    )) +
        geom_bar(stat = "identity", width = 0.6) +
        labs(
            title = paste(input$delayMetric, "by Delay Type"),
            x = "Delay Type",
            y = if (input$delayMetric == "Frequency") "Frequency of Delays" else "Average Delay (Minutes)"
        ) +
        theme_minimal(base_size = 12) +
        theme(
            legend.position = "none",
            axis.text.x = element_text(size = 12),
            axis.text.y = element_text(size = 12, hjust = 1),
            plot.title = element_text(hjust = 0.5, size = 16)
        ) +
        scale_y_continuous(
            labels = scales::comma  # Format the Y-axis values with commas
        ) +
        scale_fill_brewer(palette = "Set3")

    ggplotly(bar_plot, tooltip = "text")
})

output$areaGraph <- renderPlotly({
  # Dynamically select the grouping column
  grouping_column <- if (input$time_dimension == "Time_of_Day") "Time of Day" else "Day of Week"
  
  # Dynamically select the delay type column
  delay_column <- switch(input$delay_type,
                         "Carrier" = "DELAY_DUE_CARRIER",
                         "Weather" = "DELAY_DUE_WEATHER",
                         "NAS" = "DELAY_DUE_NAS",
                         "Security" = "DELAY_DUE_SECURITY",
                         "Late Aircraft" = "DELAY_DUE_LATE_AIRCRAFT")
  
  # Ensure the selected column exists in the dataset
  if (!delay_column %in% colnames(flights_box_data)) {
    return(NULL)
  }
  
  # Aggregate data for area graph
  plot_data <- flights_box_data %>%
    filter(!is.na(!!sym(delay_column))) %>%
    mutate(
      `Time of Day` = case_when(
        Time_of_Day == "Morning" ~ "Morning",
        Time_of_Day == "Afternoon" ~ "Afternoon",
        Time_of_Day == "Evening" ~ "Evening",
        Time_of_Day == "Night" ~ "Night"
      ),
      `Day of Week` = weekdays(as.Date(FL_DATE))
    ) %>%
    group_by(!!sym(grouping_column), Delay_Type = input$delay_type) %>%
    summarise(
      Avg_Delay = mean(!!sym(delay_column), na.rm = TRUE),
      .groups = "drop"
    )
  
  # Calculate overall average delay
  overall_avg_delay <- mean(flights_box_data[[delay_column]], na.rm = TRUE)
  
  # Create the area graph dynamically based on grouping column
  area_plot <- ggplot(plot_data, aes(
    x = !!sym(grouping_column),  # Dynamically set x-axis based on input
    y = Avg_Delay,
    fill = Delay_Type,
    group = Delay_Type,
    text = paste(
      grouping_column, ":", !!sym(grouping_column),
      "<br>Avg Delay:", round(Avg_Delay, 2),
      "<br>Delay Type:", Delay_Type
    )
  )) +
    geom_area(alpha = 0.8, position = "stack") +
    geom_point(aes(y = Avg_Delay), color = "black", size = 2) +
    geom_hline(yintercept = overall_avg_delay, color = "red", linetype = "dashed", linewidth = 1) +
    scale_x_discrete(limits = if (grouping_column == "Time of Day") {
      c("Morning", "Afternoon", "Evening", "Night")
    } else {
      c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
    }) +
    labs(
      title = paste("Average", input$delay_type, "Delays by", grouping_column),
      x = grouping_column,
      y = "Average Delay (minutes)"
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Set3")
  
  # Use ggplotly with custom hover
  ggplotly(area_plot, tooltip = "text")
})


# Filter airports with more than 15 minutes average delay and more than 2000 total flights (arriving + departing)
filtered_airports_for_map <- flights_data %>%
  select(ORIGIN, ORIGIN_CITY, DEST, DEST_CITY, ARR_DELAY, DEP_DELAY) %>%
  pivot_longer(
    cols = c(ORIGIN, DEST),
    names_to = "Flight_Type",
    values_to = "Airport"
  ) %>%
  pivot_longer(
    cols = c(ORIGIN_CITY, DEST_CITY),
    names_to = "City_Type",
    values_to = "City"
  ) %>%
  filter(
    (Flight_Type == "ORIGIN" & City_Type == "ORIGIN_CITY") | 
    (Flight_Type == "DEST" & City_Type == "DEST_CITY")
  ) %>%
  group_by(Airport, City) %>%
  summarise(
    Avg_Delay = mean(ARR_DELAY + DEP_DELAY, na.rm = TRUE),  # Average delay (arriving + departing)
    Total_Flights = n(),                                   # Total flights (arriving + departing)
    .groups = "drop"
  ) %>%
  filter(Avg_Delay > 15 & Total_Flights > 2000) %>%  # Apply the filter for both conditions
  select(Airport, City) %>%
  distinct()

# Update airport selection input to only include airports with Avg_Delay > 15 and Total_Flights > 2000
observe({
  updateSelectInput(
    session,
    "origin_airport",
    choices = filtered_airports_for_map$Airport,
    selected = filtered_airports_for_map$Airport[1]
  )
})

# Render route map with legend
output$routeMap <- renderLeaflet({
  req(input$origin_airport, input$flight_mode)

  # Filter routes based on selected airport and flight mode
  if (input$flight_mode == "departing") {
    selected_routes <- flights_data %>%
      filter(ORIGIN == input$origin_airport) %>%
      group_by(DEST, DEST_CITY, ORIGIN_CITY, ORIGIN, origin_lat, origin_long, dest_lat, dest_long) %>%
      summarise(
        Avg_Delay = mean(ARR_DELAY + DEP_DELAY, na.rm = TRUE),
        Total_Flights = n(),
        .groups = "drop"
      )
  } else {
    selected_routes <- flights_data %>%
      filter(DEST == input$origin_airport) %>%
      group_by(ORIGIN, ORIGIN_CITY, DEST_CITY, DEST, origin_lat, origin_long, dest_lat, dest_long) %>%
      summarise(
        Avg_Delay = mean(ARR_DELAY + DEP_DELAY, na.rm = TRUE),
        Total_Flights = n(),
        .groups = "drop"
      )
  }

  # Handle case when no routes are available
  if (nrow(selected_routes) == 0) {
    return(leaflet() %>%
             addTiles() %>%
             addPopups(
               lng = -98.35, lat = 39.5,
               popup = "No routes available for the selected airport."
             ))
  }

  # Color coding for delays
  selected_routes <- selected_routes %>%
    mutate(
        Delay_Color = case_when(
        Avg_Delay < 5 ~ "green",
        Avg_Delay >= 5 & Avg_Delay <= 20 ~ "yellow",
        Avg_Delay > 20 & Avg_Delay <= 35 ~ "red",
        Avg_Delay > 35 ~ "darkred",
        is.na(Avg_Delay) ~ "gray",  # Explicitly handle NA values
        TRUE ~ "black"  # Fallback for unexpected cases
      )
    )

  # Generate the map with routes and markers
  map <- leaflet(data = selected_routes) %>%
    addTiles() %>%
    addCircleMarkers(
      lng = if (input$flight_mode == "departing") ~dest_long else ~origin_long,
      lat = if (input$flight_mode == "departing") ~dest_lat else ~origin_lat,
      color = ~Delay_Color, radius = 6,
      label = if (input$flight_mode == "departing") {
        ~paste(DEST_CITY, "Avg Delay:", round(Avg_Delay, 2), "mins Total Flights:", Total_Flights)
      } else {
        ~paste(ORIGIN_CITY, "Avg Delay:", round(Avg_Delay, 2), "mins Total Flights:", Total_Flights)
      }
    ) %>%
    addCircleMarkers(
      lng = if (input$flight_mode == "departing") ~origin_long else ~dest_long,
      lat = if (input$flight_mode == "departing") ~origin_lat else ~dest_lat,
      color = "blue", radius = 8,
      label = if (input$flight_mode == "departing") {
        ~paste(ORIGIN_CITY, "Airport Code:", ORIGIN)
      } else {
        ~paste(DEST_CITY, "Airport Code:", DEST)
      }
    )

  # Add route polylines
  for (i in 1:nrow(selected_routes)) {
    map <- map %>%
      addPolylines(
        lng = c(
          if (input$flight_mode == "departing") selected_routes$origin_long[i] else selected_routes$dest_long[i],
          if (input$flight_mode == "departing") selected_routes$dest_long[i] else selected_routes$origin_long[i]
        ),
        lat = c(
          if (input$flight_mode == "departing") selected_routes$origin_lat[i] else selected_routes$dest_lat[i],
          if (input$flight_mode == "departing") selected_routes$dest_lat[i] else selected_routes$origin_lat[i]
        ),
        color = selected_routes$Delay_Color[i], weight = 2
      )
  }

  # Add a legend to explain route colors
  map %>%
    addLegend(
      position = "bottomright",
      colors = c("green", "yellow", "red", "darkred", "gray"),
      labels = c("<5 min", "5-20 min", "20-35 min", ">35 min", "No Data"),
      title = "Avg Delay",
      opacity = 1
    ) %>%
    setView(
      lng = mean(if (input$flight_mode == "departing") selected_routes$origin_long else selected_routes$dest_long, na.rm = TRUE),
      lat = mean(if (input$flight_mode == "departing") selected_routes$origin_lat else selected_routes$dest_lat, na.rm = TRUE),
      zoom = 5
    )
})


}

# Run the Shiny App
shinyApp(ui = ui, server = server)