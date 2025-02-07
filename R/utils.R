# Load necessary libraries first
library(dplyr)
library(readr)
library(purrr)
library(geosphere)
library(ggplot2)
library(ggmap)
library(magrittr)
library(leaflet)
library(sf)
library(osmdata)
library(leaflet.extras)
library(lubridate)
library(tidyr)
library(knitr)
library(htmlwidgets)

# Constants
ANALYSIS_START_DATE <- as.Date("2019-01-01")
ANALYSIS_END_DATE <- as.Date("2025-01-01")
BUFFER_DISTANCE_FT <- 100
BUFFER_DISTANCE_M <- BUFFER_DISTANCE_FT * 0.3048  # Convert feet to meters

# Economic damage constants (NSC estimates)
FATALITY_COST <- 1778000
INCAPACITATING_INJURY_COST <- 155000
INJURY_COST <- 24000
CRASH_COST <- 11400

# Chicago bounding box (after osmdata is loaded)
CHICAGO_BBOX <- getbb("Chicago, Illinois, USA", format_out = "matrix")

# Shared data loading functions
load_crash_data <- function(file_path) {
  read_csv(file_path) %>%
    mutate(CRASH_DATE = mdy_hms(CRASH_DATE)) %>%
    filter(CRASH_DATE >= ANALYSIS_START_DATE & CRASH_DATE <= ANALYSIS_END_DATE) %>%
    filter(!is.na(LONGITUDE) & !is.na(LATITUDE))
}

load_fatality_data <- function(file_path) {
  read_csv(file_path) %>%
    mutate(CRASH_DATE = mdy_hms(Crash_Date)) %>%
    filter(CRASH_DATE >= ANALYSIS_START_DATE & CRASH_DATE <= ANALYSIS_END_DATE) %>%
    rename(
      LOCATION = Location,
      FATALITY_PERSON_ID = Person_ID,
      FATALITY_CRASH_LOCATION = Crash_Location,
      FATALITY_VICTIM = Victim
    )
}

# Spatial data processing functions
convert_to_sf <- function(data, lon = "LONGITUDE", lat = "LATITUDE", crs = 4326) {
  st_as_sf(data, coords = c(lon, lat), crs = crs)
}

load_district_shapefile <- function(file_path, district_col = "DISTRICT", new_name = NULL) {
  districts <- st_read(file_path)
  districts <- st_transform(districts, crs = 4326)
  
  if (!is.null(new_name)) {
    districts <- districts %>%
      rename(!!new_name := !!district_col) %>%
      select(geometry, !!new_name)
  }
  
  return(districts)
}

# Economic damage calculation
calculate_economic_damage <- function(data) {
  data %>%
    mutate(
      fatality_count = ifelse(!is.na(FATALITY_VICTIM) & FATALITY_VICTIM != "", 1, 0),
      incapacitating_injury_count = ifelse(!is.na(INJURIES_INCAPACITATING) & INJURIES_INCAPACITATING > 0, INJURIES_INCAPACITATING, 0),
      injury_count = ifelse(!is.na(INJURIES_TOTAL) & INJURIES_TOTAL > 0, INJURIES_TOTAL, 0),
      crash_count = ifelse(!is.na(CRASH_RECORD_ID) & CRASH_RECORD_ID != "", 1, 0),
      estimated_economic_damages = 
        ifelse(fatality_count > 0, (FATALITY_COST * fatality_count) + CRASH_COST,
               ifelse(incapacitating_injury_count > 0, (INCAPACITATING_INJURY_COST * incapacitating_injury_count) + CRASH_COST,
                      ifelse(injury_count > 0, INJURY_COST * injury_count, CRASH_COST * crash_count)))
    )
}

# Mapping functions
create_district_map <- function(data, districts, district_name, centroids = NULL) {
  map <- leaflet(data) %>%
    addProviderTiles(providers$CartoDB.Positron) %>%
    addPolygons(
      data = districts, 
      fillColor = "transparent",
      weight = 2,
      color = "#000000",
      highlight = highlightOptions(
        weight = 3,
        color = "#666666",
        fillOpacity = 0.7,
        bringToFront = FALSE
      )
    )
    
  if (!is.null(centroids)) {
    map <- map %>%
      addLabelOnlyMarkers(
        data = centroids,
        label = ~label,
        labelOptions = labelOptions(
          noHide = TRUE,
          direction = 'auto',
          textOnly = TRUE
        )
      )
  }
  
  map %>%
    addCircleMarkers(
      lng = ~st_coordinates(geometry)[, 1],
      lat = ~st_coordinates(geometry)[, 2],
      radius = ~fatality_count * 5,
      color = "#FF0000",
      fillOpacity = 0.6,
      popup = ~paste(fatality_count, "fatalities")
    ) %>%
    setView(lng = -87.688037, lat = 41.939453, zoom = 12)
}

# Summary statistics function
create_district_summary <- function(data, group_col) {
  data %>%
    group_by(!!sym(group_col)) %>%
    summarize(
      total_crashes = sum(crash_count, na.rm = TRUE),
      crashes_with_injuries = sum(INJURIES_TOTAL > 0, na.rm = TRUE),
      sum_injuries = sum(INJURIES_TOTAL, na.rm = TRUE),
      sum_injuries_incapacitating = sum(INJURIES_INCAPACITATING, na.rm = TRUE),
      pedestrian_crashes = sum(FIRST_CRASH_TYPE == 'PEDESTRIAN', na.rm = TRUE),
      cyclists_crashes = sum(FIRST_CRASH_TYPE == 'PEDALCYCLIST', na.rm = TRUE),
      hit_and_run_crashes = sum(HIT_AND_RUN_I == 'Y', na.rm = TRUE),
      injuries_in_hit_and_run = sum(INJURIES_TOTAL * (HIT_AND_RUN_I == 'Y'), na.rm = TRUE),
      total_fatalities = sum(!is.na(FATALITY_VICTIM), na.rm = TRUE),
      total_cyclist_fatalities = sum(FATALITY_VICTIM == 'CYCLIST', na.rm = TRUE),
      total_driver_fatalities = sum(FATALITY_VICTIM == 'DRIVER', na.rm = TRUE),
      total_passenger_fatalities = sum(FATALITY_VICTIM == 'PASSENGER', na.rm = TRUE),
      total_pedestrian_fatalities = sum(FATALITY_VICTIM == 'PEDESTRIAN', na.rm = TRUE),
      total_motorcyclist_fatalities = sum(FATALITY_VICTIM == 'MOTORCYCLIST', na.rm = TRUE),
      total_scooter_fatalities = sum(FATALITY_VICTIM == 'SCOOTER', na.rm = TRUE),
      estimated_economic_damages = sum(estimated_economic_damages, na.rm = TRUE)
    )
}

# Export functions
export_summary <- function(data, filename) {
  # Convert sf object to dataframe and remove geometry column if present
  if (inherits(data, "sf")) {
    data <- as.data.frame(data)
    data <- data[, !names(data) %in% "geometry"]
  }
  write.csv(data, file.path("output/summaries", filename), row.names = FALSE)
}

export_map <- function(map, filename) {
  saveWidget(map, file.path("output/maps", filename))
}
